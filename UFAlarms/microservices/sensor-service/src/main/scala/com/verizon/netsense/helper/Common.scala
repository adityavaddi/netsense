package com.verizon.netsense.helper

import java.net.ConnectException
import java.time.Instant
import java.util.UUID

import akka.Done
import akka.actor.ActorSystem
import akka.kafka.scaladsl.Consumer.Control
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.kafka.{ConsumerSettings, ProducerSettings, Subscriptions}
import akka.stream.Supervision
import akka.stream.scaladsl.{Sink, Source}
import com.datastax.driver.core.exceptions.{NoHostAvailableException, ReadTimeoutException, WriteTimeoutException}
import com.fasterxml.jackson.core.JsonParseException
import com.verizon.netsense.config.KafkaConfig._
import com.verizon.netsense.config.{ConfigLoader, SensorKafkaConnector}
import com.verizon.netsense.constants.SensorSampleConstants
import com.verizon.netsense.constants.SensorSampleConstants._
import com.verizon.netsense.exceptions.CustomExceptions._
import com.verizon.netsense.model._
import com.verizon.netsense.service.SensorSampleIngestService.log
import com.verizon.netsense.utils
import com.verizon.netsense.utils.{DeviceModels, SensorValueConverter, TimeConverter}
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.ByteArrayDeserializer

import scala.concurrent.Future
import scala.util.control.NonFatal

trait Common extends TimeConverter {

  lazy val sensorTopicMapping = parsedUpstreamTopicSet
    .map(c => c.split(":"))
    .flatMap {
      case Array(k, v) => Map[String, String](k -> s"$v");
      case _           => throw new IllegalArgumentException("Unable to parse the upstream topic map " + parsedUpstreamTopicSet)
    }
    .toMap

  lazy val nodeModelToTypeMap: Map[DeviceModels.Model, DeviceTypeEnum.DeviceTypeEnum] =
    Map(
      DeviceModels.UNODEV2 -> DeviceTypeEnum.WEBSOCKET_NODE,
      DeviceModels.UNODEV3 -> DeviceTypeEnum.WEBSOCKET_NODE,
      DeviceModels.UNODEV4 -> DeviceTypeEnum.WEBSOCKET_NODE,
      DeviceModels.UNODEV5 -> DeviceTypeEnum.WEBSOCKET_NODE,
      DeviceModels.UNODEV6 -> DeviceTypeEnum.WEBSOCKET_NODE,
      DeviceModels.UNODEV7 -> DeviceTypeEnum.PUBSUB_NODE,
      DeviceModels.FALCON -> DeviceTypeEnum.PUBSUB_NODE,
      DeviceModels.MERLIN -> DeviceTypeEnum.PUBSUB_NODE,
      DeviceModels.VDK -> DeviceTypeEnum.PUBSUB_NODE,
      DeviceModels.CNEXT -> DeviceTypeEnum.PUBSUB_NODE)

  lazy val sensorTypeFilter: (SensorSampleEvent) => Boolean = e => sensorTopicMapping.keySet.contains(e.l.s)

  val sinkDecider: Supervision.Decider = {
    case ex: UnableToGetHistoricalData =>
      log.error(ex.getMessage)
      ex.printStackTrace()
      Supervision.Resume
    case ex: WriteTimeoutException =>
      log.error("Unable to write the event to cassandra", ex)
      ex.printStackTrace()
      Supervision.Resume
    case ex: NoHostAvailableException =>
      log.error("Unable to establish cassandra connection ", ex); Supervision.Restart
    case ex: UnableToGetDataFromDbException =>
      log.error(ex.getMessage); Supervision.Resume
    case ex: ReadTimeoutException =>
      log.error("Unable to read the events from cassandra", ex)
      Supervision.Resume
    case ex: NullPointerException =>
      log.error("Null element in stream " + ex.getMessage)
      ex.printStackTrace()
      Supervision.Resume
    case ex: Exception =>
      log.error("Unhandled exception in stream " + ex.getMessage)
      ex.printStackTrace()
      Supervision.Resume
  }

  lazy val sensorTypeToParentMap: Map[String, String] =
    Map(jtx -> jt,
      jty -> jt,
      jtz -> jt,
      jtm -> jt,
      RF -> rf,
      sn -> rf,
      bRL -> bR,
      bRA -> bR)

  def convertCASELtoRestPackForVideoNode(requestid: String, nodeid: String, timestamp: String, sensorid: String)
  : SensorSampleRequest = {
    val sensorId = sensorTypeToParentMap.getOrElse(sensorid, sensorid)
    SensorSampleRequest(
      uuid = requestid,
      sid = nodeid,
      a = "GET",
      f = sensorId,
      p = SensorValueConverter.constructRestPathForDeviceRequest(nodeid,
        sensorId),
      d = timestamp,
      l = SensorSampleProps(s = sensorId,
        t = convertISOToMicros(timestamp)).toMsgPack
    )
  }

  def convertCASELtoMsgPackForCoreNode(nodeid: String, sensorid: String): SensorRequestToDevice = {
    val sensorId = sensorTypeToParentMap.getOrElse(sensorid, sensorid)
    SensorSampleCoreRequest(Array.apply(nodeid),
      name = SensorSampleConstants.SENSOR_SAMPLE_REQ_STR,
      sensor = sensorId)
  }

  lazy val parallelism = ConfigLoader.streamParallelism


  def generateEmptyRequestWithQueryEnvelope(query: SensorQueryEnvelope): (SensorQueryEnvelope, SensorRequestToDevice) =
    (query, EmptyRequest())



  lazy val flowDeciderForQuery: Supervision.Decider = {
    case NonFatal(ex: UnableToGetHistoricalData) =>
      log.error(ex.getMessage)
      ex.printStackTrace(); Supervision.Resume
    case ex: MessageUnMarshallingException =>
      log.error("Unable to Unmarshal the elements " + ex.getMessage, ex)
      Supervision.Resume
    case ex: EmptyResultSetException =>
      log.error(ex.getMessage); Supervision.Resume
    case ex: EventMissingFieldsException =>
      log.warn(ex.getMessage)
      Supervision.Resume
    case ex: JsonParseException => Supervision.Resume
    case ex: NullPointerException =>
      log.error("Null element in stream in sensor query " + ex.getMessage)
      ex.printStackTrace()
      Supervision.Resume
    case ex: Exception => log.error("Unhandled exception in stream for sensor query" + ex.getMessage)
      ex.printStackTrace()
      Supervision.Resume
  }

  lazy val flowDeciderForFixture: Supervision.Decider = {
    case NonFatal(ex: UnableToGetHistoricalData) => log.error(ex.getMessage)
      ex.printStackTrace()
      Supervision.Resume
    case ex: FixtureHarmlessException => log.warn(ex.getMessage)
      Supervision.Resume
    case ex: FixtureHarmfulException => log.error(ex.getMessage)
      ex.printStackTrace()
      Supervision.Resume
    case ex: NullPointerException => log.error("Null element in stream in fixture service" + ex.getMessage)
      ex.printStackTrace()
      Supervision.Resume
    case ex: Exception => log.error("Unhandled exception in stream for fixture service" + ex.getMessage)
      ex.printStackTrace()
      Supervision.Resume
  }

  lazy val systemDecider: Supervision.Decider = {
    case ex: ConnectException => log.error("Unable to connect to Graphite " + ex.getMessage); Supervision.Resume
    case ex: NoHostAvailableException => log.error("Unable to connect to DB " + ex.getMessage); Supervision.Resume
    case NonFatal(ex) =>
      log.error("Exception found in the Stream " + ex.getMessage); ex.printStackTrace(); Supervision.Resume
    case ex: Exception => log.error("Unhandled Exception seen " + ex.getMessage); Supervision.stop;
  }

  def configKafkaSource[K, V](_consumerSettings: ConsumerSettings[K, V],
                              _topics: Set[String]): Source[ConsumerRecord[K, V], Control] =
    Consumer.plainSource(_consumerSettings, Subscriptions.topics(_topics))

  def configKafkaProducer[K, V](_producerSettings: ProducerSettings[K, V]): Sink[ProducerRecord[K, V], Future[Done]] =
    Producer.plainSink(_producerSettings)

  def setupConsumerSettings(groupId: String)(implicit system: ActorSystem) =
    SensorKafkaConnector.configKafkaConsumerSettings(bootStrapServers,
                                                     groupId,
                                                     new ByteArrayDeserializer,
                                                     new ByteArrayDeserializer)(system)

  def configureKafkaConsumers(topicNames: Set[String], groupId: String)(implicit system: ActorSystem) =
    configKafkaSource(setupConsumerSettings(groupId), topicNames)

  def generateUUid = UUID.randomUUID().toString

  def getLatestTimeStamp = {
    Instant.ofEpochMilli(System.currentTimeMillis()).toString
  }

}
