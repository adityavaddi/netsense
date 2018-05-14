package com.vz.nsp.eventsimulator.service.app

import akka.actor.{ActorSystem, Props}
import com.newmotion.akka.rabbitmq.{ChannelActor, ConnectionActor, CreateChannel}
import com.vz.nsp.eventsimulator.service.actor._
import com.vz.nsp.eventsimulator.service.config.ConfigLoader
import com.vz.nsp.eventsimulator.service.util.{AMQPConnection, Logging}

/**
 * Created by Jittara on 4/17/17.
 * NorthboundSimulator publishes messages to RabbitMQ
 */
object SimulatorApp extends Logging {
  implicit val system = ActorSystem.create("NorthboundSimulator-system")

  //Northbound Publisher
  if (ConfigLoader.northboundPublisherEnable.equalsIgnoreCase("true")) {
    log.debug("Northbound publisher started !!!")
    NorthboundPublisher.publishtoRabbitMQ
  }

  //Northbound Consumer
  if (ConfigLoader.northboundSubscriberEnable.equalsIgnoreCase("true")) {
    log.debug("Northbound Subscriber started !!!")
    val connectionDetails = AMQPConnection.consumerConnection
    val connection =
      system.actorOf(ConnectionActor.props(connectionDetails), "Consumer-System")
    connection ! CreateChannel(ChannelActor.props(NorthboundConsumer.setupSubscriber), Some("AMQPsubscriber"))
  }

  //Southbound Publisher
  if (ConfigLoader.mqttPublisherEnable.equalsIgnoreCase("true")) {
    log.debug("Southbound publisher started !!!")
    //Actor creation
    val publisherActor = system.actorOf(Props[SouthboundPublisher], name = "publisherActor")
    // start Actor to publish the messages
    publisherActor ! SouthPublisherTrigger(true)
  }

  //Southbound Consumer
  if (ConfigLoader.mqttSubscriberEnable.equalsIgnoreCase("true")) {
    log.debug("Southbound subscriber started !!!")
    //Actor creation
    val MQTTSubscriberActor =
      system.actorOf(Props[SouthboundSubscriber], name = "subscriberActor")
    Thread.sleep(1000)
    // start Actor to subscribe to topic
    MQTTSubscriberActor ! SouthboundSubscriberTrigger
  }
}
