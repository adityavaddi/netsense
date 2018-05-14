package com.vz.nsp.eventsimulator.service.actor

import akka.stream.ThrottleMode
import akka.stream.scaladsl.{Sink, Source}
import com.verizon.netsense.metrics.Instrumented
import com.vz.nsp.eventsimulator.service.config.ConfigLoader
import com.vz.nsp.eventsimulator.service.util._
import nl.grons.metrics.scala.Timer
import org.eclipse.paho.client.mqttv3.{MqttClient, MqttMessage}

import scala.concurrent.Future
import scala.concurrent.duration._

/**
 * Created by Jittara on 4/25/17.
 * This file publishing mqtt data to topic V1.mqtt.
 */
case class SouthPublisherTrigger(condition: Boolean)

/**
 * In this class getting the configuration details and creating message,topic and publishing to rabbitmq
 */
class SouthboundPublisher extends BaseActor with Instrumented with Logging {

  private[this] val southPublisherTimer: Timer = metrics.timer("southpublisher-timer")

  /**
   * publish method for southbound simulator
   */
  def publish = {
    val client = new MqttClient(MQTTConnection.brokerUrl, MqttClient.generateClientId, null)
    log.debug("Southbound Publisher Connecting to host " + MQTTConnection.brokerUrl)
    client connect MQTTConnection.getMqttConnection
    log.debug("Southbound Publisher connected !!!")
    val msgTopic = client.getTopic(MQTTConnection.publisherTopic)
    Source
      .fromIterator(() => Iterator from 0)
      .mapAsync(ConfigLoader.parallel)(x => Future(getMessage))
      .throttle(ConfigLoader.throttleLevel, 1.second, ConfigLoader.throttleLevel, ThrottleMode.shaping)
      // .take(ConfigLoader.messageCount)
      .map { x =>
        log.debug("Southbound simulator publishing message " + MsgPackUtil.fromBytes(x))
        southPublisherTimer.time()
        val msg = new MqttMessage(getMessage)
        msg.setQos(ConfigLoader.qos)
        msgTopic.publish(msg)
      }
      .runWith(Sink.ignore)
  }

  def receive = {
    case SouthPublisherTrigger(conditionVal) => publish
    case _                                   => log.info("Something went wrong while connecting to Southbound Publisher!!!")
  }

  def getMessage: Array[Byte] = {
    val random = new scala.util.Random
    val messageTypes: List[Array[Byte]] =
      List(MessageFormat.alarmFormat, MessageFormat.sensorFormat, MessageFormat.loginFormat)
    messageTypes(random.nextInt(messageTypes.size))
  }
}
