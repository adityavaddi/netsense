package com.verizon.netsense.helper

import akka.{Done, NotUsed}
import akka.stream.alpakka.mqtt.{MqttMessage, MqttQoS}
import akka.stream.alpakka.mqtt.scaladsl.MqttSink
import akka.stream.scaladsl.{RestartSink, Sink}
import com.verizon.netsense.connector.MqttConnector
import com.verizon.netsense.util.Common
import com.verizon.netsense.utils.Logging
import scala.concurrent.duration._
import scala.concurrent.Future

class MQTTHelper(uniqueMqttClientId: String)
  extends Common
  with Logging{

  //Sink MQTT
  lazy val mqttSettings = MqttConnector.mqttSettings(useInternalMqtt = false)
    .withClientId(uniqueMqttClientId + "-source")

  lazy val mqttSink: Sink[MqttMessage, Future[Done]] =
    MqttSink(mqttSettings.withClientId(uniqueMqttClientId + "-sink"), MqttQoS.AtLeastOnce)

  var sinkRetryCount: Long = 0
  def incSinkRetryCounter(): Unit = sinkRetryCount = sinkRetryCount + 1

  def restartableMqttsink: Sink[MqttMessage, NotUsed] = RestartSink.withBackoff(
    minBackoff = 1.seconds,
    maxBackoff = 5.seconds,
    randomFactor = 0.2
  ) {
    () =>
      if (sinkRetryCount == 0) {
        incSinkRetryCounter()
        log.info("Starting Publisher for ToMQTT " + mqttSettings.broker
          +  " clientid: " + mqttSettings.clientId)
      } else {
        incSinkRetryCounter()
        log.error("Reconnecting MQTT Publisher on failure count: " + sinkRetryCount + " "
          + mqttSettings.broker + " clientid: " + mqttSettings.clientId)
      }
      mqttSink
  }

}
