package com.vz.nsp.eventsimulator.service.config

import com.typesafe.config.ConfigFactory

/**
 * Created by jittara on 4/7/17.
 */
object ConfigLoader {
  lazy val config = ConfigFactory.load()

  // AMQP config
  lazy val amqp                  = config.getConfig("newamqp")
  lazy val addresses             = amqp.getConfigList("addresses")
  lazy val routingKey            = amqp.getString("routing-key")
  lazy val queueName             = amqp.getString("queue-name")
  lazy val exchange              = amqp.getString("exchange-name")
  lazy val throttleLevel         = amqp.getInt("throttle-level")
  lazy val numberOfNodes         = amqp.getInt("number-nodes")
  lazy val throttleLevelConsumer = amqp.getInt("throttle-level-consumer")
  lazy val userName              = amqp.getString("username")
  lazy val password              = amqp.getString("password")
  lazy val port                  = addresses.get(0).getInt("port")
  lazy val host                  = addresses.get(0).getString("host")

  //Node configurations
  lazy val nodeConfig   = config.getConfig("node")
  lazy val sensorsArray = nodeConfig.getStringList("sensors")
  lazy val nodeIds      = nodeConfig.getStringList("node-ids")
  lazy val nodeNames    = nodeConfig.getStringList("node-names")
  lazy val actionTypes  = nodeConfig.getStringList("action-types")

  //Common Configuration
  lazy val simulator = config.getConfig("simulator")
  lazy val parallel  = simulator.getInt("parallel")

  //Mqtt Configuration
  lazy val mqtt                = config.getConfig("mqtt")
  lazy val protocol            = mqtt.getString("protocol")
  lazy val mqttTopic           = mqtt.getString("topic")
  lazy val mqttSubscriberTopic = mqtt.getString("subscriber-topic")
  lazy val mqttPort            = mqtt.getInt("mqtt-port")
  lazy val keepAlive           = mqtt.getInt("keep-alive-interval")
  lazy val maxInflight         = mqtt.getInt("max-inflight")
  lazy val qos                 = mqtt.getInt("qos")

  //South Events
  lazy val southEvents   = config.getConfig("south-events")
  lazy val alarmSeverity = southEvents.getStringList("alarm-severity")
  lazy val alarmType     = southEvents.getStringList("alarm-type")
  lazy val loginAuthType = southEvents.getStringList("login-auth-type")

  //Publisher Subscriber Enable
  lazy val publisherSubscriber        = config.getConfig("publisher-subscriber")
  lazy val northboundPublisherEnable  = publisherSubscriber.getString("northbound-publisher-enable")
  lazy val northboundSubscriberEnable = publisherSubscriber.getString("northbound-subscriber-enable")
  lazy val mqttPublisherEnable        = publisherSubscriber.getString("southbound-publisher-enable")
  lazy val mqttSubscriberEnable       = publisherSubscriber.getString("southbound-subscriber-enable")
  lazy val messageCount               = publisherSubscriber.getInt("message-count")

}
