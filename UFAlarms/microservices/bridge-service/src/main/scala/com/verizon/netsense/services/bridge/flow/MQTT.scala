package com.verizon.netsense.services.bridge.flow

import java.net.InetAddress
import java.util.UUID

import akka.{Done, NotUsed}
import akka.stream.alpakka.mqtt.scaladsl.{MqttSink, MqttSource}
import akka.stream.alpakka.mqtt.{MqttMessage, MqttQoS, MqttSourceSettings}
import akka.stream.scaladsl.{Flow, RestartSink, RestartSource, Sink, Source}
import com.verizon.netsense.connector.{ConnectionRetry, MqttConnector}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.services.bridge.Bridge.system
import com.verizon.netsense.services.bridge.utils.ConfigLoader
import com.verizon.netsense.services.bridge.utils.ConfigLoader.{mqttBufferSize, mqttClientId, mqttFromTopic}
import com.verizon.netsense.utils.Logging
import nl.grons.metrics.scala.{MetricName, Timer}

import scala.concurrent.Future
import scala.concurrent.duration._

/**
 * Created by wswaney on 6/6/17.
 */
object MQTT extends /*App with*/ Instrumented with Logging {

  val hostName     = InetAddress.getLocalHost
  val metricsPrefix = hostName + ".bridge.mqtt-flow."

  override lazy val metricBaseName = MetricName(metricsPrefix)
  private[this] lazy val mqttSinkTimer: Timer = metrics.timer("toMQTT-timer")
  private[this] lazy val mqttSourceTimer: Timer = metrics.timer("fromMQTT-timer")

  lazy val uniqueMqttClientId = mqttClientId + UUID.randomUUID().toString + "-" + hostName

  lazy val sourceSettings = MqttConnector.mqttSettings(useInternalMqtt = false)
    .withClientId(uniqueMqttClientId + "-source")

  lazy val mqttSourceSettings = MqttSourceSettings(sourceSettings, Map(mqttFromTopic -> MqttQoS.AtLeastOnce))
  lazy val mqttSource         = MqttSource(mqttSourceSettings, bufferSize = mqttBufferSize)
  lazy val mqttSink: Sink[MqttMessage, Future[Done]] =
    mqttSinkTimer.time{
    MqttSink(sourceSettings.withClientId(uniqueMqttClientId + "-sink"), MqttQoS.AtLeastOnce)}

  lazy val mqttSourceFlow = mqttSource
    .map { msg =>
      log.debug(s"mqtt msg topic: ${msg.topic} payload: ${msg.payload.utf8String}")
        (msg.topic, msg.payload.toArray)

    }
    .withAttributes(Common.attributes)

  var sourceRetryCount: Long = 0

  def incSourceRetryCounter(): Unit = sourceRetryCount = sourceRetryCount + 1

  def mqttMsgWithKafkaTopic(kafkaTopic: String): Flow[MqttMessage, (String,String,Array[Byte]), NotUsed] = Flow[MqttMessage].map(
    msg => {
      log.debug(s"Mqtt topic: ${msg.topic}")
      log.debug(s"Mqtt msg received: ${msg.payload.utf8String}")
      log.debug(s"Kafka topic: ${kafkaTopic}")
      mqttSourceTimer.time{
      (kafkaTopic, msg.topic, msg.payload.toArray)
    }}
  ).withAttributes(Common.attributes)


  lazy val multipleTopicSource = Source(ConfigLoader.mqttToKafkaTopicMappings)
    .flatMapMerge(ConfigLoader.mqttToKafkaTopicMappings.size, s => {
      log.debug(s"Kafka topic mapped to mqtt: ${s._1} <- ${s._2}")
      val settings = MqttSourceSettings(sourceSettings.withClientId(s"${s._1}-${UUID.randomUUID().toString}"), Map(s._2 -> MqttQoS.AtLeastOnce))
      MqttSource(settings, bufferSize = 1).via(mqttMsgWithKafkaTopic(s._1))
    })

  lazy val restartableMultipleMqttSources =   RestartSource.withBackoff(
    minBackoff = 1.seconds,
    maxBackoff = 5.seconds,
    randomFactor = 0.2
  ) {
    () =>
      if (sourceRetryCount == 0) {
        incSourceRetryCounter()
        log.debug("Starting Consumer for fromMqtt " + mqttSourceSettings.connectionSettings.broker +
          " clientid: " + mqttSourceSettings.connectionSettings.clientId)
      } else {
        incSourceRetryCounter()
        log.error("Reconnecting MQTT Consumer on failure count: " + sourceRetryCount + " " +
          mqttSourceSettings.connectionSettings.broker + " clientid: "
          + mqttSourceSettings.connectionSettings.clientId)
        ConnectionRetry.mqttMaxRetryCheck(sourceRetryCount)(system)
      }
      multipleTopicSource
  }.withAttributes(Common.attributes)



//  lazy val restartableMqttSource: Source[(String, Array[Byte]), NotUsed] = RestartSource.withBackoff(
//    minBackoff = 1.seconds,
//    maxBackoff = 5.seconds,
//    randomFactor = 0.2
//  ) {
//    () =>
//      if (sourceRetryCount == 0) {
//      incSourceRetryCounter()
//      log.info("Starting Consumer for fromMqtt " + mqttSourceSettings.connectionSettings.broker +
//      " clientid: " + mqttSourceSettings.connectionSettings.clientId)
//    } else {
//      incSourceRetryCounter()
//      log.error("Reconnecting MQTT Consumer on failure count: " + sourceRetryCount + " " +
//        mqttSourceSettings.connectionSettings.broker + " clientid: "
//        + mqttSourceSettings.connectionSettings.clientId)
//    }
//    mqttSourceFlow
//  }
//    .withAttributes(Common.attributes)

  var sinkRetryCount: Long = 0

  def incSinkRetryCounter(): Unit = sinkRetryCount = sinkRetryCount + 1

  lazy val restartableMqttsink: Sink[MqttMessage, NotUsed] = RestartSink.withBackoff(
    minBackoff = 1.seconds,
    maxBackoff = 5.seconds,
    randomFactor = 0.2
  ) {
    () =>
      if (sinkRetryCount == 0) {
        incSinkRetryCounter()
        log.debug("Starting Publisher for ToMQTT " + mqttSourceSettings.connectionSettings.broker +
          " clientid: " + mqttSourceSettings.connectionSettings.clientId)
      } else {
        incSinkRetryCounter()
        log.error("Reconnecting MQTT Publisher on failure count: " + sinkRetryCount + " " +
          mqttSourceSettings.connectionSettings.broker + " clientid: "
          + mqttSourceSettings.connectionSettings.clientId)
        ConnectionRetry.mqttMaxRetryCheck(sinkRetryCount)(system)
      }
      mqttSink
  }

}
