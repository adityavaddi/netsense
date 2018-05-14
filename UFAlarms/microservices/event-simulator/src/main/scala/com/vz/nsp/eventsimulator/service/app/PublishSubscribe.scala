package com.vz.nsp.eventsimulator.service.app

import akka.actor.{ActorRef, ActorSystem, Props}
import com.newmotion.akka.rabbitmq.{
  Channel,
  ChannelActor,
  ChannelMessage,
  ConnectionActor,
  ConnectionFactory,
  CreateChannel
}
import com.vz.nsp.eventsimulator.service.actor.SouthboundPublisher
import com.vz.nsp.eventsimulator.service.app.SimulatorApp.system
import com.vz.nsp.eventsimulator.service.util.MQTTConnection
import io.scalac.amqp._
import org.eclipse.paho.client.mqttv3.MqttClient
import org.reactivestreams.{Publisher, Subscriber}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * Created by rajapa1 on 5/17/17.
 */
object PublishSubscribe {
  implicit val system = ActorSystem()

  def setupPublisher(channel: Channel, self: ActorRef) {
    val queue = channel.queueDeclare().getQueue
//    channel.queueBind(queue, exchange, "")
  }

  for (i <- 1 to 1000) {
    val factory = new ConnectionFactory()
    factory.setHost("34.201.72.135")
    factory.setUsername("farallones")
    factory.setPassword("sensity1")
    val connection = system.actorOf(ConnectionActor.props(factory), "rabbitmq" + i)
    val exchange   = "amq.fanout"

  }

}
