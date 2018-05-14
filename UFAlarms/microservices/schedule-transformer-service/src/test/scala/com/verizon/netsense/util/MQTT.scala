package com.verizon.netsense.util

import java.net.InetAddress
import java.util.UUID

import akka.stream.alpakka.mqtt.scaladsl.{MqttSink, MqttSource}
import akka.stream.alpakka.mqtt.{MqttMessage, MqttQoS, MqttSourceSettings}
import akka.stream.scaladsl.{RestartSink, RestartSource, Sink, Source}
import akka.{Done, NotUsed}
import com.verizon.netsense.connector.MqttConnector
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import nl.grons.metrics.scala.{MetricName, Timer}

import scala.concurrent.Future
import scala.concurrent.duration._

/**
  * Created by wswaney on 6/6/17.
  */
object MQTT extends Logging {

  val hostName     = InetAddress.getLocalHost
  val metricsPrefix = hostName + ".bridge.mqtt-flow."

  lazy val uniqueMqttClientId = UUID.randomUUID().toString + "-" + hostName

  lazy val sourceSettings = MqttConnector.mqttSettings(useInternalMqtt = true)
    .withClientId(uniqueMqttClientId + "-source")

  lazy val mqttSink: Sink[MqttMessage, Future[Done]] =
    MqttSink(sourceSettings.withClientId(uniqueMqttClientId + "-sink"), MqttQoS.AtLeastOnce)

  var sinkRetryCount: Long = 0

  def incSinkRetryCounter(): Unit = sinkRetryCount = sinkRetryCount + 1

  lazy val restartableMqttsink: Sink[MqttMessage, NotUsed] = RestartSink.withBackoff(
    minBackoff = 1.seconds,
    maxBackoff = 5.seconds,
    randomFactor = 0.2
  ) { () => mqttSink}

}