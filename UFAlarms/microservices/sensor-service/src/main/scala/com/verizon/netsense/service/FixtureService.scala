package com.verizon.netsense.service

import java.time.Instant
import java.util.UUID
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import akka.stream._
import akka.stream.scaladsl.{Flow, GraphDSL, Keep, Merge, Partition, RestartSink, Sink}
import com.verizon.netsense.constants.SensorSampleConstants
import com.verizon.netsense.db.{DbLayer, PhantomService}
import com.verizon.netsense.helper.Common
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.model.{FixtureQueryRequestBinded, _}
import com.verizon.netsense.utils.{DeviceModel, DeviceModels, Logging, ObjectMapperUtil}
import akka.{Done, NotUsed}
import akka.actor.ActorSystem
import akka.kafka.ProducerSettings
import com.verizon.netsense.config.KafkaConfig._
import com.verizon.netsense.config.{ConfigLoader, SensorKafkaConnector}
import com.verizon.netsense.entity.Entity
import com.verizon.netsense.exceptions.CustomExceptions._
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.ByteArraySerializer

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by nalamte on 3/18/18.
  */
object FixtureService extends Instrumented
  with Logging
  with PhantomService
  with Common
  with Entity {

  import scala.concurrent.duration._

  implicit val ec = ExecutionContext.Implicits.global

  implicit val system = ActorSystem.create("Fixture-Service-system")

  private[this] val kafkaSinkTimer: Timer = metrics.timer("fixture-kafka-sink-producer")

  lazy val appConfig = ConfigLoader.config.getConfig("app")

  //To Query driver level from currentTime to (currentTime-driverLevelTimeDifference)
  lazy val driverLevelTimeDifference = appConfig.getLong("driverLevelTimeDifference")

  //Number of retrials we do to get driver level from device
  lazy val ltRetrialLimit = appConfig.getLong("ltRetrialLimit")

  lazy val ltDelayTimeInSec = appConfig.getLong("ltDelayTimeInSec")

  lazy val powerFilter = Flow[SensorSampleEvent]
    .filter(filterPowerSensorValues)
    .withAttributes(ActorAttributes.supervisionStrategy(flowDeciderForFixture))


  lazy val filterPowerSensorValues: (SensorSampleEvent) => (Boolean) = {
    case SensorSampleEvent(_, nodeid, _, _, _, SensorPayload(_, SensorSampleConstants.W|SensorSampleConstants.mW, SensorSampleConstants.p, _, _))
      if nodeid.nonEmpty => log.debug("Filtering p (Power Sensor): ");true
    case SensorSampleEvent(_, nodeid, _, _, _, SensorPayload(_, _, SensorSampleConstants.mP, _, _))
      if nodeid.nonEmpty => log.debug("Filtering mP (Power Sensor)");true
    case _ => false
  }

  lazy val fixtureQueryFlow: Flow[SensorSampleEvent, FixtureQueryRequestBinded, NotUsed] = Flow[SensorSampleEvent]
    .mapAsync(parallelism) { e =>
      DbLayer().getFixtureForNode(e.sid).recover{
        case ex =>
          throw new ExceptionWhenQueryingFixture(s"Failed to fetch fixture from Neo4j for nodeid ${e.sid}"){
            override val underlyingMessage = ""
          }
        }
        .map {
          fixture => log.debug(s"Fetched Fixture for Node ${e.sid}: $fixture")
          FixtureQueryRequestBinded(e, fixture)
        }
    }.withAttributes(ActorAttributes.supervisionStrategy(flowDeciderForFixture))


  lazy val checkForRecentLtValueFlow: Flow[FixtureQueryRequestBinded, FixtureQueryRequestBinded, NotUsed]
  = Flow[FixtureQueryRequestBinded]
    .mapAsync(parallelism) { e =>
      val timestamp = getTimestampsToQueryRecentDriverLevel
      DbLayer().getSensorHistoryData(e.sensorSampleEvent.sid,
        SensorSampleConstants.lt, timestamp._1, timestamp._2, limit = 1).recover{
        case ex =>
          throw new UnableToGetHistoricalData(s"Failed to get Lt from DB for nodeid ${e.sensorSampleEvent.sid}")
      }
      .map {
        ltValuelist => log.debug(s"Fetched latest Driver level from Cassandra: + $ltValuelist")
        e.copy(driverLevelValue = ltValuelist)
      }
    }.withAttributes(ActorAttributes.supervisionStrategy(flowDeciderForFixture))

  def getTimestampsToQueryRecentDriverLevel: (Long, Long) = {
    val diff = driverLevelTimeDifference
    val toTime = System.currentTimeMillis() * 1000
    (toTime - diff, toTime)
  }

  lazy val segregate: (FixtureQueryRequestBinded) => Int = {
    case fQB: (FixtureQueryRequestBinded) if fQB.driverLevelValue.nonEmpty => 1
    case _ => 0
  }

  /*
  * This flow will send command to device with sensorType lt
  * and wait for 5 sec and merge with CheckForRecentLtValueFlow
  * This whole process will be repeated ltRetrialLimit times iff
  * sensor service wont get lt value from device
  * */
  lazy val sendCommandWithDelayFlow = Flow[FixtureQueryRequestBinded]
    .async
    .map(fQB => {
      if (fQB.retrialCounter <= ltRetrialLimit) fQB.copy(retrialCounter = fQB.retrialCounter + 1)
      else throw new UnableToGetLtDataFromDB(s"Failed to get Lt from DB for nodeid ${fQB.sensorSampleEvent.sid}" +
        s" after sending device command $ltRetrialLimit times"){
        override val underlyingMessage = ""
      }
    })
    .map(sendCommandToDevice)
    .delay(FiniteDuration.apply(ltDelayTimeInSec, TimeUnit.SECONDS), DelayOverflowStrategy.emitEarly)
    .addAttributes(Attributes.inputBuffer(Math.pow(2, 15).toInt, Math.pow(2, 20).toInt))
    .withAttributes(ActorAttributes.supervisionStrategy(flowDeciderForFixture))

  lazy val sendCommandToDevice: FixtureQueryRequestBinded => FixtureQueryRequestBinded = fqb => {
    fqb.fixture.nodeType match {
      case Some(nodeType) =>
        val model: DeviceModels.Model = DeviceModel.apply(nodeType)
        val nodeModel = nodeModelToTypeMap.get(model)
        DeviceCommandService.sensorCommandToDevice(nodeModel,
          List(SensorSampleConstants.lt),
          generateUUid,
          fqb.sensorSampleEvent.sid,
          getLatestTimeStamp,
          generateUUid)
        log.debug(s"Sending command to Device: $fqb")
        fqb
      case None =>
        throw new UnableToGetNodeModelFromNeo(s"Node model missing from neo4j for nodeid ${fqb.sensorSampleEvent.sid}"){
          override val underlyingMessage = ""
        }
    }
  }

  lazy val calculatePowerAndCheckValueWithInRange: Flow[FixtureQueryRequestBinded, List[DeviceAlarm], NotUsed] =
    Flow[FixtureQueryRequestBinded]
      .mapAsync(parallelism) { e =>
        Future {
          PowerCalculationService.calculatePower(e)
        }
      }.withAttributes(ActorAttributes.supervisionStrategy(flowDeciderForFixture))

  lazy val setupProducerSettings: ProducerSettings[Array[Byte], Array[Byte]]
  = SensorKafkaConnector.configKafkaProducerSettings(bootStrapServers,
    _keySerializer = new ByteArraySerializer,
    _valueSerializer =
      new ByteArraySerializer)

  var sinkRetryCount = new AtomicInteger(0)

  lazy val kafkaSensorSink: Sink[List[DeviceAlarm], Future[Done]] =
    Flow[List[DeviceAlarm]]
      .mapConcat(identity)
      .mapAsync(parallelism) { x =>
        kafkaSinkTimer.time {
          log.debug("Sending Power Alarm to Kafka sink: " + x)
          Future(
            new ProducerRecord[Array[Byte], Array[Byte]](coreNodeAlarmTopic,
              x.nodeid.getBytes(),
              ObjectMapperUtil.toJsonBytes(x))
          )
        }
      }
      .toMat(configKafkaProducer(_producerSettings = setupProducerSettings))(Keep.right)

  lazy val restartableProducerPowerAlarmSink: Sink[List[DeviceAlarm], NotUsed] = RestartSink.withBackoff(
    minBackoff = 1.seconds,
    maxBackoff = 5.seconds,
    randomFactor = 0.2
  ) { () =>
    if (sinkRetryCount.getAndIncrement == 0) {
      log.info("Starting Kafka producer Sensor FixtureIngestion: " + bootStrapServers)
    }
    else {
      {
        log.error("Reconnecting kafka producer on SensorSampleFixtureIngestion "
          + sinkRetryCount.getAndIncrement + " broker: " + bootStrapServers)
      }
    }
    kafkaSensorSink
  }.withAttributes(ActorAttributes.supervisionStrategy(flowDeciderForFixture))

  /*
  * This subgraph will process p,mP sensors with mW units and
  * calculates minRange, maxRange by using fixture and driver level
  * of node. Generates overPower and underPower alarms by comparing
  * powerValue, minRange and maxRange
  * */
  lazy val fixtureSubGraph = Sink.fromGraph(g = GraphDSL.create() { implicit b =>
    import GraphDSL.Implicits._

    // Source
    val SensorValueSource = b.add(Flow[SensorSampleEvent])

    // Flows
    val PowerFilter = b.add(powerFilter)
    val FixtureQueryFlow = b.add(fixtureQueryFlow)
    val CheckForRecentLtValueFlow = b.add(checkForRecentLtValueFlow)

    // Partition
    val PartitionOnLtAvailability = b.add(Partition[FixtureQueryRequestBinded](2, segregate))

    // Flows
    val DelayAndRetryFlow = b.add(sendCommandWithDelayFlow)
    val PowerCalculationFlow = b.add(calculatePowerAndCheckValueWithInRange)

    // Merge
    val merge = b.add(Merge[FixtureQueryRequestBinded](2))

    // Sink
    val AlarmSink = b.add(restartableProducerPowerAlarmSink).in

    SensorValueSource ~> PowerFilter ~> FixtureQueryFlow ~> merge ~> CheckForRecentLtValueFlow ~> PartitionOnLtAvailability.in
    PartitionOnLtAvailability.out(0) ~> DelayAndRetryFlow ~> merge
    PartitionOnLtAvailability.out(1) ~> PowerCalculationFlow ~> AlarmSink

    SinkShape(SensorValueSource.in)
  })

}
