package com.vz.ns.ts.service.config

import com.rabbitmq.client.ConnectionFactory
import com.typesafe.config.ConfigFactory
import com.vz.ns.ts.service.config.ConfigLoader._
import io.scalac.amqp.{Address, Connection, ConnectionSettings}

import scala.collection.immutable.Seq

object AMQPConnection {

  implicit val factory = new ConnectionFactory()

  def getConnection: Connection = {
    val config                       = ConfigFactory.load()
    val settings: ConnectionSettings = ConnectionSettings.apply(config)
    val ad: Seq[Address]             = Seq(Address(rmqhost, rmqport))
    Connection(settings.copy(addresses = ad, username = rmqusername, password = rmqpassword))
  }
}
