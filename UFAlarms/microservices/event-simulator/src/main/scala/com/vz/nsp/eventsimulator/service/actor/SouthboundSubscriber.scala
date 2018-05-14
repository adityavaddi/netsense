package com.vz.nsp.eventsimulator.service.actor

import com.verizon.netsense.metrics.Instrumented
import com.vz.nsp.eventsimulator.service.util.{Logging, MQTTConnection}
import nl.grons.metrics.scala.Timer
import org.eclipse.paho.client.mqttv3._
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence

/**
 * Created by Jittara on 4/5/17.
 * Southbound subscriber to listen to topic
 *
 */
case object SouthboundSubscriberTrigger

class SouthboundSubscriber extends BaseActor with Instrumented with Logging {

  private[this] val southConsumerTimer: Timer = metrics.timer("southconsumer-timer")

  val callback = new MqttCallback {
    override def messageArrived(topic: String, message: MqttMessage): Unit = {
      log.info("Receiving Data, Topic : %s, Message : %s".format(topic, message))
      southConsumerTimer.time()
    }
    override def connectionLost(cause: Throwable): Unit =
      log.error("Error in Southbound Subscriber " + cause)
    override def deliveryComplete(token: IMqttDeliveryToken): Unit = {}
  }

  /**
   * Subscribe method for Southbound subscriber
   */
  def subscribe =
    try {
      log.debug("Southbound Subscriber started !!!")
      val persistence = new MemoryPersistence
      val client      = new MqttClient(MQTTConnection.brokerUrl, MqttClient.generateClientId, persistence)
      log.debug("Southbound Subscriber Connecting to host" + MQTTConnection.brokerUrl)
      //Connect to MQTT broker
      client connect MQTTConnection.getMqttConnection
      log.debug("Southbound Subscriber connected !!!")
      client.subscribe(MQTTConnection.subscriberTopic)
      //Callback automatically triggers as and when new message arrives on specified topic
      client.setCallback(callback)
    } catch {
      case e: MqttException => log.error("Exception in Southbound Subscriber " + e)
    }

  def receive = {
    case SouthboundSubscriberTrigger => subscribe
    case _                           => log.info("Something went wrong while connecting to Southbound Subscriber!!!")
  }
}
