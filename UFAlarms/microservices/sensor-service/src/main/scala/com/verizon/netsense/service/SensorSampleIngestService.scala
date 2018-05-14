package com.verizon.netsense.service

import akka.actor.ActorSystem
import akka.stream._
import akka.stream.scaladsl.{Broadcast, Flow, GraphDSL, Keep, RestartSink, RunnableGraph, Sink}
import akka.{Done, NotUsed}
import com.verizon.netsense.config.KafkaConfig._
import com.verizon.netsense.config.{KafkaConfig, SensorKafkaConnector}
import com.verizon.netsense.constants.SensorSampleConstants.{ligStatOff, ligStatOn, lt, netStatConnected}
import com.verizon.netsense.db.PhantomService
import com.verizon.netsense.exceptions.CustomExceptions.UnableToGetDataFromDbException
import com.verizon.netsense.helper._
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.model.{Light, NodeStatus, SensorPayload, SensorSampleEvent}
import com.verizon.netsense.utils.{Logging, ObjectMapperUtil, SensorValueConverter}
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.ByteArraySerializer

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object SensorSampleIngestService extends Instrumented with Logging with PhantomService with Common {

  implicit val system = ActorSystem.create("Sensor-Sample-Ingest-system")

  implicit val mat = ActorMaterializer(
    ActorMaterializerSettings(system)
      .withInputBuffer(Math.pow(2, 10).toInt, Math.pow(2, 20).toInt)
      .withSupervisionStrategy(systemDecider)
  )

  def eventIngestionInit: NotUsed =
    sensorSampleDbIngestionGraph.run()

  implicit val ec = ExecutionContext.Implicits.global

  log.info("Using upstream topics: " + sensorTopicMapping.values.toList.toString)

  private[this] val sensorValueConverterTimer: Timer = metrics.timer("sensor-value-converter")
  private[this] val kafkaSinkTimer: Timer = metrics.timer("kafka-sink-producer")
  private[this] val cassandraPersistCompletes: Timer = metrics.timer("cassandra-persist-success-timer")
  private[this] val cassandraLightDriverPersistCompletes: Timer =
    metrics.timer("cassandra-light-driver-persist-success-timer")
  private[this] val cassandraLightSenStatPersistCompletes: Timer =
    metrics.timer("cassandra-light-senstat-persist-success-timer")
  private[this] val cassandraLightUpdateLatency: Timer = metrics.timer("cassandra-light-update-latency-timer")
  private[this] val cassandraNodeStatusUpdateLatency: Timer =
    metrics.timer("cassandra-node_status-update-latency-timer")


  lazy val setupProducerSettings = SensorKafkaConnector.configKafkaProducerSettings(
    bootStrapServers,
    _keySerializer = new ByteArraySerializer,
    _valueSerializer = new ByteArraySerializer
  )


  import scala.concurrent.duration._

  lazy val sensorValueConverterFlow = Flow[SensorSampleEvent]
    .mapAsync(parallelism) { e =>
      Future(sensorValueConverterTimer.time(SensorValueConverter.convertSensorValues(e))).recover {
        case ex @ (_: ArithmeticException | _: RuntimeException) =>
          log.error("Unable to convert the sensor value " + e.toJSON + " " + ex); throw ex
        case ex => log.error("Unhandled exception on value conversion " + e.toJSON + " " + ex); throw ex
      }
    }
    .withAttributes(ActorAttributes.supervisionStrategy(flowDeciderForQuery))

  lazy val filterDriverLevelSensorValues: (SensorSampleEvent) => (Boolean) = {
    case SensorSampleEvent(_, nodeid, _, _, _, SensorPayload(_, _, `lt`, _, _)) if nodeid.nonEmpty => true
    case _                                                                                         => false
  }

  lazy val filterAndProliferateSensorDriverValues = Flow[List[SensorSampleEvent]]
    .mapConcat(identity)
    .filter(filterDriverLevelSensorValues)
    .map(e => e.copy(l = e.l.copy(n = e.sid)))

  /**
   * Db sink helps to updating driver column on the [[com.verizon.netsense.db.LightTable]]
   * Cassandra table on receiving driver_level [[lt]] device sensor sample.
   *
   * Note: If no light record is found, this impl. will drop the element
    **/
  lazy val updateLightModeDbSink = Flow[SensorSampleEvent]
    .mapAsync(parallelism)(
      e =>
        cassandraLightUpdateLatency.time(Try(PhantomService.getLightStatus(nodeId = e.sid)).toEither match {
          case Left(left) => log.error("Unable to get the latest light mode from db " + left.getMessage); throw left
          case Right(right) =>
            right
              .recover {
                case ex: Exception => throw new UnableToGetDataFromDbException(e.toJSON + ex, ex)
              }
              .map {
                case Some(light) =>
                  Future {
                    val newLightStatus: Light = light.copy(driver = Some(e.l.v.toInt))
                    Try(PhantomService.storeLightStatus(newLightStatus)).toEither match {
                      case Left(l) =>
                        log.error("Unable to update the light mode " + newLightStatus.toJSON + " " + l.getMessage);
                        throw l
                      case Right(r) =>
                        r.onComplete {
                          case Success(s) =>
                            log.debug("Successfully Updated Light Mode  " + newLightStatus.toJSON)
                            cassandraLightDriverPersistCompletes.time(s)
                          case Failure(ex) =>
                            log.error("Failed in updating light mode " + newLightStatus.toJSON + " " + ex); throw ex
                        }
                    }
                  }
                case None => Future.unit
              }
        })
    )
    .toMat(Sink.ignore)(Keep.right)
    .withAttributes(ActorAttributes.supervisionStrategy(sinkDecider))

  /**
   * Db sink helps in updating sen_stat, lig_stat and net_stat column(s) on the
   * [[com.verizon.netsense.db.NodeStatusModel]] Cassandra table on receiving
   * driver_level [[lt]] device sensor sample.
   *
   * Here lig_stat is set to "on" (for lt > 0) (or) "off" (for lt < 0)
   *
   * Note: If node_status record is found, this impl. will drop the element
    **/
  lazy val updateSenStatAtNodeStatusDbSink: Sink[SensorSampleEvent, Future[Done]] = Flow[SensorSampleEvent]
    .mapAsync(parallelism) { e =>
      cassandraNodeStatusUpdateLatency.time(Try(PhantomService.getNodeStatus(e.sid)).toEither match {
        case Left(left) => log.error("Unable to get the node status from db " + left.getMessage); throw left
        case Right(right) =>
          right
            .recover {
              case ex: Exception => throw new UnableToGetDataFromDbException(e.toJSON + ex, ex)
            }
            .map {
              case Some(nodeStatus) =>
                Future {
                  val _sen_stat = e.l.v.toInt
                  val _lig_stat = if (_sen_stat > 0) ligStatOn else ligStatOff
                  val newNodeStatus: NodeStatus = nodeStatus.copy(nodeId = e.sid,
                                                                  netStat = Some(netStatConnected),
                                                                  senStat = Some(e.l.v.toString),
                                                                  ligStat = Some(_lig_stat))
                  Try(PhantomService.storeNodeStatus(newNodeStatus)).toEither match {
                    case Left(l) =>
                      log
                        .error("Unable to update sen_stat on node_status " + newNodeStatus.toJSON + " " + l.getMessage)
                      throw l
                    case Right(r) =>
                      r.onComplete {
                        case Success(s) =>
                          log.debug("Successfully updated sen_stat on node_status  " + newNodeStatus.toJSON)
                          cassandraLightSenStatPersistCompletes.time(s)
                        case Failure(ex) =>
                          log.error("Failed in updating sen_stat on node_status " + newNodeStatus.toJSON + " " + ex)
                          throw ex
                      }
                  }
                }
              case None => Future.unit
            }
      })
    }
    .toMat(Sink.ignore)(Keep.right)
    .withAttributes(ActorAttributes.supervisionStrategy(sinkDecider))

  lazy val consumerSinkForDbCall = Flow[List[SensorSampleEvent]]
    .mapAsync(parallelism)(
      e =>
        Future {
          e.map(x => (x, Try(PhantomService.storeEvent(x)).toEither)).foreach { a =>
            a._2 match {
              case Left(l: Throwable) =>
                log.error("Unable to store the event " + a._1.toJSON + " " + l.getMessage)
                throw l
              case Right(r) =>
                r.onComplete {
                  case Success(s) =>
                    log.debug("Persisted successfully sensor sample " + a._1.toJSON)
                    cassandraPersistCompletes.time(s)
                  case Failure(ex) => log.error("Failed in ingesting of sensor sample " + a._1.toJSON + " " + ex)
                }
            }
          }
      }
    )
    .toMat(Sink.ignore)(Keep.right)
    .withAttributes(ActorAttributes.supervisionStrategy(sinkDecider))

  /**
    * Sink Stage to filter the sensor samples from the list
    * see [[KafkaConfig.parsedUpstreamTopicSet]] and forwards the samples upstream based on
    * the sensor to topics mapping see also [[Common.sensorTopicMapping]] with message uuid being key
    * and sensorSampleEvent(decoded being value) in Array[Byte]
    **/
  lazy val kafkaSensorSink =
    Flow[List[SensorSampleEvent]]
      .mapConcat(identity)
      .filter(sensorTypeFilter)
      .mapAsync(parallelism) { x =>
        kafkaSinkTimer.time {
          Future(
            new ProducerRecord[Array[Byte], Array[Byte]](sensorTopicMapping(x.l.s),
              x.uuid.getBytes(),
              ObjectMapperUtil.toJsonBytes(x))
          )
        }
      }
      .toMat(configKafkaProducer(_producerSettings = setupProducerSettings))(Keep.right)

  var sinkRetryCount: Long = 0

  def incSinkRetryCounter(): Unit = sinkRetryCount = sinkRetryCount + 1

  lazy val restartableProducerSinkSensorIngestion = RestartSink
    .withBackoff(
      minBackoff = 1.seconds,
      maxBackoff = 5.seconds,
      randomFactor = 0.2
    ) { () =>
      if (sinkRetryCount == 0) {
        incSinkRetryCounter()
        log.info("Starting Kafka producer SensorSampleIngestion: " + bootStrapServers)
      } else {
        {
          incSinkRetryCounter()
          log.error(
            "Reconnecting kafka producer on SensorSampleIngestion "
            + sinkRetryCount + " broker: " + bootStrapServers
          )
        }
      }
      kafkaSensorSink
    }
    .withAttributes(ActorAttributes.supervisionStrategy(flowDeciderForQuery))

  /*
   * This subgraph helps in updating the light driver levels on light_mode
   * and sen_stat, net_stat, lig_stat on node_status where the UI picks the values
   * to show the latest light state on the map
   * */
  lazy val lightModeUpdateSubGraph = Sink.fromGraph(g = GraphDSL.create() { implicit b =>
    import GraphDSL.Implicits._

    val sensorValueSource = b.add(Flow[List[SensorSampleEvent]])

    val filterAndProliferateFlow = b.add(filterAndProliferateSensorDriverValues)

    val broadcastSensorValues = b.add(Broadcast[SensorSampleEvent](2))

    val UpdateLightDriverDbSink = b.add(updateLightModeDbSink).in

    val UpdateSenStatDbSink = b.add(updateSenStatAtNodeStatusDbSink).in

    sensorValueSource ~> filterAndProliferateFlow ~> broadcastSensorValues ~> UpdateLightDriverDbSink
    broadcastSensorValues ~> UpdateSenStatDbSink
    SinkShape(sensorValueSource.in)
  })

  val sensorSampleDbIngestionGraph = RunnableGraph.fromGraph(g = GraphDSL.create() { implicit b =>
    import GraphDSL.Implicits._

    // Source
    val SensorSampleEventSource: Outlet[SensorSampleEvent] = b.add(CommonKafkaConsumer.SensorSampleSourceSubgraph).out

    // Flows

    val broadcast = b.add(Broadcast[List[SensorSampleEvent]](3))
    val sensorValueConverter = b.add(sensorValueConverterFlow)

    // Sinks
    val DbSink = b.add(consumerSinkForDbCall).in

    val KafkaSink = b.add(restartableProducerSinkSensorIngestion).in

    val UpdateLightDriverDbSink = b.add(lightModeUpdateSubGraph).in


    SensorSampleEventSource  ~> sensorValueConverter ~> broadcast ~> DbSink
                                                        broadcast ~> KafkaSink
                                                        broadcast ~> UpdateLightDriverDbSink
    ClosedShape
    /*
    * Toggling down the power alarms feature from develop
    * */
   /* val PowerServiceSub = b.add(FixtureService.fixtureSubGraph).in
    val IngestionAndFixtureBroadcast = b.add(Broadcast[SensorSampleEvent](2))
    SensorSampleEventSource ~> IngestionAndFixtureBroadcast ~> sensorValueConverter ~> broadcast ~> DbSink
    broadcast ~> KafkaSink
    broadcast ~> UpdateLightDriverDbSink

    IngestionAndFixtureBroadcast ~> PowerServiceSub*/
  })

}
