package com.verizon.netsense.service

import akka.NotUsed
import akka.actor.ActorSystem
import akka.kafka.scaladsl.Consumer.Control
import akka.kafka.scaladsl.Producer
import akka.stream._
import akka.stream.scaladsl.{Flow, GraphDSL, Keep, Merge, Partition, RestartSink, RestartSource, RunnableGraph, Source}
import com.fasterxml.jackson.core.JsonProcessingException
import com.verizon.netsense.config.KafkaConfig._
import com.verizon.netsense.config.{ConfigLoader, SensorKafkaConnector}
import com.verizon.netsense.constants.SensorSampleConstants.{ALL, ENERGY}
import com.verizon.netsense.db.{DbLayer, PhantomService}
import com.verizon.netsense.entity.{ISMessageError, ISResponseError}
import com.verizon.netsense.exceptions.CustomExceptions._
import com.verizon.netsense.helper.{Common, EventWrapper, QueryServiceValidator}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.model.{SensorQueryEnvelope, _}
import com.verizon.netsense.utils.{DeviceModel, Logging, ObjectMapperUtil}
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{ByteArraySerializer, StringDeserializer}

import scala.concurrent.Future
import scala.util.Try

/**
  * Created by subrsi9 on 5/18/17.
  */
object SensorHistoryQueryService extends Instrumented
  with Logging
  with Common {

  implicit val queryServiceActorSystem = ActorSystem("Query-Service-system")

  import queryServiceActorSystem.dispatcher

  implicit val mat = ActorMaterializer(
    ActorMaterializerSettings(queryServiceActorSystem)
      .withInputBuffer(Math.pow(2, 10).toInt, Math.pow(2, 20).toInt)
      .withSupervisionStrategy(systemDecider)
  )

  def queryServiceInit: NotUsed = queryServiceGraph.run()

  private[this] val kafkaSourceTimer: Timer = metrics.timer("kafka-source-consumer")
  private[this] val kafkaSinkTimer: Timer = metrics.timer("kafka-sink-producer")
  private[this] val unMarshallingFlowTimer: Timer = metrics.timer("json-unmarshall-flow-timer")
  private[this] val unMarshallingFailureTimer: Timer = metrics.timer("json-unmarshall-flow-Exception")

  lazy val setupConsumerSettings = SensorKafkaConnector.configKafkaConsumerSettings(bootStrapServers,
    groupId,
    new StringDeserializer,
    new StringDeserializer)

  lazy val kafkaConsumerSource: Source[ConsumerRecord[String, String], Control] = configKafkaSource(setupConsumerSettings, Set(requestTopic))
    .map { e => kafkaSourceTimer.time(e) }

  var sourceRetryCount: Long = 1

  import scala.concurrent.duration._

  lazy val kafkaRestartableConsumerSource: Source[ConsumerRecord[String, String], NotUsed] =
    RestartSource.withBackoff(
      minBackoff = 1.seconds,
      maxBackoff = 5.seconds,
      randomFactor = 0.2
    ) { () =>
      if (sourceRetryCount == 1) {
        sourceRetryCount = sourceRetryCount + 1
        log.info("Starting Consumer for History Service " + bootStrapServers)
      }
      else {
        sourceRetryCount = sourceRetryCount + 1
        log.error("Restarting Consumer on failure " + sourceRetryCount)
      }
      kafkaConsumerSource
    }

  lazy val msgUnmarshallingFlow = Flow[ConsumerRecord[String, String]]
    .map { e =>
      unMarshallingFlowTimer.time {
        (e, Try(ObjectMapperUtil.fromJson[SensorQueryEnvelope](e.value)).toEither)
      }
    }
    .map { a =>
      a._2 match {
        case Right(r) => log.debug("Received request for sensor events query " + r.toJSON); r
        case Left(ex: JsonProcessingException) =>
          log.error("Unable to unmarshall the record from Kafka " + a._1.value() + " " + ex)
          throw ex
        case Left(ex: Exception) =>
          log.error("Unhandled exception caught at unmarshalling " + a._1.value() + " " + ex.getMessage)
          throw ex
        case Left(_) =>
          log.error("Unable to parse the element " + a._1.value())
          throw new Exception("Unable to parse the element")
      }
    }.withAttributes(ActorAttributes.supervisionStrategy(flowDeciderForQuery))

  lazy val messageValidationFlow: Flow[SensorQueryEnvelope, SensorQueryEnvelope, NotUsed] = Flow[SensorQueryEnvelope]
    .filter(QueryServiceValidator.validationPredicate)
    .withAttributes(ActorAttributes.supervisionStrategy(flowDeciderForQuery))

  def buildProducerRecord(sqe: SensorQueryEnvelope, errorMsg: String, status: Int):
  ProducerRecord[Array[Byte], Array[Byte]] = {
    new ProducerRecord[Array[Byte], Array[Byte]](sqe.responsetopic, sqe.messageid.getBytes,
      SensorSampleQueryResponse(sqe.messageid, response = SensorSampleQueryPayload(sqe.request.requestid,
        success = false, sqe.request.timestamp, errorMsg, status, items = null)).toJSON.getBytes)
  }

  /**
    * This graph helps in publishing the intermediate messages(error, command) to Kafka avoiding them to flow
    * through the whole graph, short circuiting the error in intermediate stages.
    */
   val kafkaIntermediateSinkActor =
    Source.actorRef[ProducerRecord[Array[Byte], Array[Byte]]](Int.MaxValue, OverflowStrategy.dropHead)
      .map { msg => log.debug("msg received at actor ref " + new String(msg.value(), "UTF-8") + " " + msg.topic()); msg }
      .toMat(Producer.plainSink(setupProducerSettings))(Keep.both)
      .addAttributes(Attributes.inputBuffer(Math.pow(2, 15).toInt, Math.pow(2, 20).toInt))
      .withAttributes(ActorAttributes.supervisionStrategy(sinkDecider))
      .run()

  lazy val dbLayer = DbLayer.apply()

  lazy val nodeTypeQueryFlow: Flow[SensorQueryEnvelope, QueryRequestBinded, NotUsed] = Flow[SensorQueryEnvelope]
    .mapAsync(parallelism) { e =>
      Future {
        Try(dbLayer.getSiteModelForNode(e.request.nodeprops.nodeid, e.request.siteprops.siteid)).toEither match {
          case Left(l) => throw l
          case Right(r) => log.debug("Response from graphDb for model query " + r)
            QueryRequestBinded.bindQueryWithGraphResult(e, r)
        }
      }
    }.withAttributes(ActorAttributes.supervisionStrategy(flowDeciderForQuery))


  lazy val sensorCommandFlow: Flow[QueryRequestBinded, (QueryRequestBinded, List[String]), NotUsed] = Flow[QueryRequestBinded]
    .map(qRB => {
      val sensorList = splitSensorByDelimiter(qRB)
      DeviceCommandService.sensorCommandToDevice(qRB.nodeTypeTimeZone.nodeType,sensorList,qRB.sensorQueryEnvelope.request.requestid,
        qRB.sensorQueryEnvelope.request.nodeprops.nodeid,
        qRB.sensorQueryEnvelope.request.timestamp,qRB.sensorQueryEnvelope.messageid)
      (qRB, sensorList)
    }).withAttributes(ActorAttributes.supervisionStrategy(flowDeciderForQuery))


  def splitSensorByDelimiter(qrb: QueryRequestBinded): List[String] =
    qrb.sensorQueryEnvelope.request.extprops.sensorid.split(',').toList


  def getSensorSampleEvents(qrbSensorListTuple: (QueryRequestBinded, List[String]))
  : ((SensorQueryEnvelope, NodeModelTimeZone), Future[List[List[(Long, Double)]]])
  = {
    ((qrbSensorListTuple._1.sensorQueryEnvelope, qrbSensorListTuple._1.nodeTypeTimeZone),
      Try(getSensorDatapointsFromDB(qrbSensorListTuple)).toEither match {
        case Left(l) =>
          kafkaIntermediateSinkActor._1 ! buildProducerRecord(qrbSensorListTuple._1.sensorQueryEnvelope, l.getMessage, 500)
          throw new UnableToGetHistoricalData("sensor samples " + qrbSensorListTuple._1.sensorQueryEnvelope.toJSON + " " + l.getMessage, l)
        case Right(r) => r
      })
  }


  lazy val buildSQEForEachSensor: (String, Int, SensorQueryEnvelope) => (SensorQueryEnvelope) = (s, sensorSize, sqe) => {
    if (sensorSize > 1) sqe.copy(request = sqe.request.copy(extprops = sqe.request.extprops.copy(sensorid = s, limit = 1)))
    else sqe.copy(request = sqe.request.copy(extprops = sqe.request.extprops.copy(sensorid = s)))
  }

  def getSensorDatapointsFromDB(qrbSensorListTuple: (QueryRequestBinded, List[String])): Future[List[List[(Long, Double)]]] = {
    val sensorSize = qrbSensorListTuple._2.size
    Future.traverse(qrbSensorListTuple._2)(sensorId =>
      PhantomService.getSensorEvents(buildSQEForEachSensor(sensorId, sensorSize,
        qrbSensorListTuple._1.sensorQueryEnvelope)))
  }

  val sensorQueryResponseBuilderFlow = Flow[((SensorQueryEnvelope, NodeModelTimeZone), Future[scala.List[List[(Long, Double)]]])]
    .mapAsync(parallelism) { e =>
      log.debug("Request received for response build " + e._1._1.toJSON)
      EventWrapper
        .buildResponsePayload(e._1, e._2)
        .recover {
          case ex: SiteTimeZoneNotFoundException =>
            log.error("Failed to build response for " + ex.getMessage)
            kafkaIntermediateSinkActor._1 ! buildProducerRecord(e._1._1,
              ex.underlyingMessage + e._1._1.request.siteprops.siteid, 500)
            throw ex
          case ex: Exception =>
            log.error("Failed to build response for " + ex + " " + e._1._1.toJSON)
            kafkaIntermediateSinkActor._1 ! buildProducerRecord(e._1._1, ex.getMessage + e._1._1.toJSON, 500)
            throw ex
        }
    }
    .map(x => (x._1, x._2.toJSON))
    .withAttributes(ActorAttributes.supervisionStrategy(flowDeciderForQuery))

  lazy val requestNotInScopeFlow = Flow[SensorQueryEnvelope]
    .map(x => (x, ISMessageError(messageid = x.messageid, ISResponseError(x.request.requestid,
      success = false, timestamp = x.request.timestamp, error = "Request not in scope of Sensor-service ",
      status = 400)).toJSON))

  lazy val sensorSampleQueryFlow = Flow[SensorQueryEnvelope]
    .via(nodeTypeQueryFlow)
    .via(sensorCommandFlow)
    .async
    .map(getSensorSampleEvents)
    .via(sensorQueryResponseBuilderFlow)
    .withAttributes(ActorAttributes.supervisionStrategy(flowDeciderForQuery))

  lazy val energySiteQueryFlow = Flow[SensorQueryEnvelope]
    .map { e =>
      Try(PhantomService.getAggregatedEnergySavingSite(e)).toEither match {
        case Left(l) =>
          val errorRecord = buildProducerRecord(e, l.getMessage, 500)
          kafkaIntermediateSinkActor._1 ! errorRecord
          throw new UnableToGetHistoricalData("aggregated energy savings site " + e.toJSON + " " + l, l)
        case Right(r) => (e, r)
      }
    }
    .mapAsync(parallelism) { e =>
      EventWrapper.buildSiteResponsePayload(e._1, e._2)
    }
    .map(x => (x._1, x._2.toJSON))
    .withAttributes(ActorAttributes.supervisionStrategy(sinkDecider))

  lazy val energyNodeQueryFlow =
    Flow[SensorQueryEnvelope]
      .map { e =>
        Try(PhantomService.getAggregatedEnergySavingNode(e)).toEither match {
          case Left(l) =>
            kafkaIntermediateSinkActor._1 ! buildProducerRecord(e, l.getMessage, 500)
            throw new UnableToGetHistoricalData("aggregated energy savings node " + e.toJSON + " " + l, l)
          case Right(r) => (e, r)
        }
      }
      .mapAsync(parallelism) { e =>
        EventWrapper.buildNodeResponsePayload(e._1, e._2)
      }
      .map(x => (x._1, x._2.toJSON))
      .withAttributes(ActorAttributes.supervisionStrategy(sinkDecider))

  lazy val seggregateQueries: (SensorQueryEnvelope) => (Int) = {
    case a: SensorQueryEnvelope if SensorQueryEnvelope.getNodeSensorProps(a).isDefined =>
      SensorQueryEnvelope.getNodeSensorProps(a) match {
        case Some(e: (String, String)) if e._1 != null && e._2 != null => e match {
          case e: (String, String) if !e._1.equalsIgnoreCase(ALL) && !e._2.equalsIgnoreCase(ENERGY) => 0
          case e: (String, String) if !e._1.equalsIgnoreCase(ALL) && e._2.equalsIgnoreCase(ENERGY) => 1
          case e: (String, String) if e._1.equalsIgnoreCase(ALL) && e._2.equalsIgnoreCase(ENERGY) => 2
          case _ => 3
        }
        case _ => 3
      }
  }

  lazy val setupProducerSettings = SensorKafkaConnector.configKafkaProducerSettings(bootStrapServers,
    _keySerializer = new ByteArraySerializer,
    _valueSerializer =
      new ByteArraySerializer)


  lazy val loglimitsize = ConfigLoader.config.getConfig("app").getInt("loglimitsize")

  lazy val responseKafkaFlow = Flow[(SensorQueryEnvelope, String)]
    .mapAsync(parallelism) { x =>
      kafkaSinkTimer.time {
        log.debug(s"response sent to Kafka response topic ${x._1.responsetopic}")
        if (x._1.request.extprops.limit < loglimitsize) log.debug(s"with message ${x._2}")
        Future(new ProducerRecord[Array[Byte], Array[Byte]](x._1.responsetopic, x._1.messageid.getBytes, x._2.getBytes))
      }
    }

  lazy val responseKafkaSink = configKafkaProducer(_producerSettings = setupProducerSettings)

  var sinkRetryCount: Long = 0

  def incSinkRetryCounter(): Unit = sinkRetryCount = sinkRetryCount + 1

  lazy val restartableProducerSinkSensorHistory = RestartSink.withBackoff(
    minBackoff = 1.seconds,
    maxBackoff = 5.seconds,
    randomFactor = 0.2
  ) { () =>
    if (sinkRetryCount == 0) {
      incSinkRetryCounter()
      log.info("Starting Kafka producer SensorHistoryQuery: " + bootStrapServers)
    }
    else {
      {
        incSinkRetryCounter()
        log.error("Reconnecting kafka producer on SensorHistoryQuery "
          + sinkRetryCount + " broker: " + bootStrapServers)
      }
    }
    responseKafkaSink
  }.withAttributes(ActorAttributes.supervisionStrategy(flowDeciderForQuery))

  /**
    * Avoiding compression for smaller records to save the cost of decompression latency
    */
  lazy val segreggateLightMode: (ProducerRecord[Array[Byte], Array[Byte]]) => (Int) = {
    case a: ProducerRecord[Array[Byte], Array[Byte]] if a.value().length < messageSizeCutoff => 0
    case _ => 1
  }

  val queryServiceGraph = RunnableGraph.fromGraph(g = GraphDSL.create() { implicit b =>

    import GraphDSL.Implicits._

    // Source
    val Source = b.add(kafkaRestartableConsumerSource).out

    // Partition
    val queryRouter = b.add(Partition[SensorQueryEnvelope](4, seggregateQueries))

    // Flows
    val messageUnpack = b.add(msgUnmarshallingFlow)
    val SensorSampleQuery = b.add(sensorSampleQueryFlow)
    val EnergySavingsNodeQuery = b.add(energyNodeQueryFlow)
    val EnergySavingsSiteQuery = b.add(energySiteQueryFlow)
    val RequestOufOfScopeFlow = b.add(requestNotInScopeFlow) // To Respond out Scope requests to Request Source
    val MessageValidation = b.add(messageValidationFlow)

    // Merge

    val producerRecordBuildFlow = b.add(responseKafkaFlow)

    val merge = b.add(Merge[(SensorQueryEnvelope, String)](4))

    // Sink
    val kafkaProducerSink = b.add(restartableProducerSinkSensorHistory).in

      Source ~> messageUnpack ~> MessageValidation ~> queryRouter.in
                                                      queryRouter.out(0) ~> SensorSampleQuery      ~> merge
                                                      queryRouter.out(1) ~> EnergySavingsNodeQuery ~> merge
                                                      queryRouter.out(2) ~> EnergySavingsSiteQuery ~> merge
                                                      queryRouter.out(3) ~> RequestOufOfScopeFlow  ~> merge
    merge ~> producerRecordBuildFlow ~> kafkaProducerSink

    ClosedShape
  })

}
