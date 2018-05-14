package com.verizon.netsense.service

import akka.actor.ActorSystem
import akka.kafka.scaladsl.Producer
import akka.stream.scaladsl.{Keep, Source}
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, OverflowStrategy}
import com.verizon.netsense.config.KafkaConfig.{bootStrapServers, sensorCommandCoreNodeTopic, sensorCommandVideoNodeTopic}
import com.verizon.netsense.config.SensorKafkaConnector
import com.verizon.netsense.helper.Common
import com.verizon.netsense.model._
import com.verizon.netsense.utils
import com.verizon.netsense.utils.Logging
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.ByteArraySerializer

object DeviceCommandService extends Logging
  with Common
  with utils.TimeConverter {

  implicit val commandServiceActorSystem = ActorSystem("Command-Service-system")

  implicit val mat = ActorMaterializer(
    ActorMaterializerSettings(commandServiceActorSystem)
      .withInputBuffer(Math.pow(2, 10).toInt, Math.pow(2, 20).toInt)
      .withSupervisionStrategy(systemDecider)
  )

  def generateEmptyRequestWithQueryEnvelope(nodeId: String): SensorRequestToDevice = EmptyRequest(nodeId)

  lazy val setupProducerSettings = SensorKafkaConnector.configKafkaProducerSettings(bootStrapServers,
    _keySerializer = new ByteArraySerializer,
    _valueSerializer =
      new ByteArraySerializer)

  /**
    * This graph helps in publishing the intermediate messages(error, command) to Kafka avoiding them to flow
    * through the whole graph, short circuiting the error in intermediate stages.
    */
  private val kafkaIntermediateSinkActor =
    Source.actorRef[ProducerRecord[Array[Byte], Array[Byte]]](Int.MaxValue, OverflowStrategy.dropHead)
      .map { msg => log.debug("msg received at actor ref " + new String(msg.value(), "UTF-8") + " " + msg.topic()); msg }
      .toMat(Producer.plainSink(setupProducerSettings))(Keep.both).run()

  def sensorCommandToDevice(nodeType: Option[DeviceTypeEnum.DeviceTypeEnum], sensorList: List[String],
                            requestid: String, nodeid: String, timestamp: String, messageid: String): Unit = {
    val deviceRequest = nodeType match {
      case Some(DeviceTypeEnum.PUBSUB_NODE) =>
        val sensorAndTopic = getSensorAndTopicName(sensorList, sensorCommandVideoNodeTopic)
        (convertCASELtoRestPackForVideoNode(requestid, nodeid, timestamp, sensorAndTopic._1), sensorAndTopic._2)

      case Some(DeviceTypeEnum.WEBSOCKET_NODE) =>
        val sensorAndTopic = getSensorAndTopicName(sensorList, sensorCommandCoreNodeTopic)
        (convertCASELtoMsgPackForCoreNode(nodeid, sensorAndTopic._1), sensorAndTopic._2)

      case _ => (generateEmptyRequestWithQueryEnvelope(nodeid), None)
    }
    deviceRequest._2 match {
      case Some(topicName) => {
        log.debug(s"Sending sensor-request SS => Bridge ${deviceRequest._1.toJSON} to topic $topicName")
        val msg = new ProducerRecord(topicName, messageid.getBytes(), deviceRequest._1.toMsgPack)
        kafkaIntermediateSinkActor._1 ! msg
      }
      case None =>
        log.warn("Unable to send request to device" + deviceRequest._1.toJSON)
    }
  }

  def getSensorAndTopicName(sensorList: List[String], topic: String): (String, Option[String]) = {
    sensorList.headOption match {
      case None => ("", None)
      case Some(sensor) => (sensor, Some(topic))
    }
  }



}
