package com.verizon.netsense.connector

import com.typesafe.config.ConfigFactory
import io.scalac.amqp.{Connection, ConnectionSettings}

object AmqpConnection {

  def getConnection: Connection = {
    val config = ConfigFactory.load()
    Connection(ConnectionSettings.apply(config))
  }
}
