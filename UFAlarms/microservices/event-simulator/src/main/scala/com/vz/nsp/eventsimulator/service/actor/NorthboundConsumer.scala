package com.vz.nsp.eventsimulator.service.actor

import akka.actor.ActorRef
import com.newmotion.akka.rabbitmq.{BasicProperties, Channel, Envelope}
import com.rabbitmq.client.DefaultConsumer
import com.verizon.netsense.metrics.Instrumented
import com.vz.nsp.eventsimulator.service.config.ConfigLoader
import com.vz.nsp.eventsimulator.service.util.{AMQPConnection, Logging, MsgPackUtil}
import nl.grons.metrics.scala.Timer

/**
 * Created by Jittara on 4/17/17.
 * NorthboundSimulator publishes messages to RabbitMQ
 */
object NorthboundConsumer extends Instrumented with Logging {
  private[this] val northSubscriberTimer: Timer = metrics.timer("northSubscriber-timer")

  AMQPConnection.consumerConnection

  /**
   * Northbound Subscriber
   */
  def setupSubscriber(channel: Channel, self: ActorRef) {
    channel.queueDeclare(ConfigLoader.queueName, true, false, false, null)
    log.debug("Northbound Subscriber connected ot queue " + ConfigLoader.queueName)
    val consumer = new DefaultConsumer(channel) {
      override def handleDelivery(consumerTag: String,
                                  envelope: Envelope,
                                  properties: BasicProperties,
                                  body: Array[Byte]) {
        log.debug(
          "Receiving Data, Topic : %s, Message : %s".format(ConfigLoader.queueName, MsgPackUtil.fromBytes(body))
        )
        northSubscriberTimer.time()
      }
    }
    channel.basicConsume(ConfigLoader.queueName, true, consumer)
  }

}
