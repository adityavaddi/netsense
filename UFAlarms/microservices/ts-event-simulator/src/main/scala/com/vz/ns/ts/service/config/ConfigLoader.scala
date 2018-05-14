package com.vz.ns.ts.service.config

import com.typesafe.config.ConfigFactory

/**
 * Created by donthgo on 5/2/17.
 */
object ConfigLoader {

  private val config = ConfigFactory.load()

  // AMQP config
  lazy val amqp         = config.getConfig("amqpp")
  lazy val routingKey   = amqp.getString("routing-key")
  lazy val queueName    = amqp.getString("queue-name")
  lazy val rmqusername  = amqp.getString("username")
  lazy val rmqpassword  = amqp.getString("password")
  lazy val rmqhost      = amqp.getString("rmqhost")
  lazy val rmqport      = amqp.getInt("rmqport")
  lazy val exchangeName = amqp.getString("exchange-name")

  //REST SERVICES configuration
  lazy val http     = config.getConfig("http")
  lazy val httpHost = http.getString("host")
  lazy val httpPort = http.getInt("port")

  //EVENT configuration
  lazy val event        = config.getConfig("event")
  lazy val noOfNodes    = event.getInt("totalNodes")
  lazy val noOfEvents   = event.getInt("totalEvents")
  lazy val sensorsArray = event.getStringList("sensors").toArray

}
