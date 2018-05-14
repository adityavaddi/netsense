package com.vz.nsp.eventsimulator.service.util

import java.util

import com.rabbitmq.client.ConnectionFactory
import com.typesafe.config.{Config, ConfigFactory}
import com.vz.nsp.eventsimulator.service.config.ConfigLoader
import io.scalac.amqp.{Address, Connection, ConnectionSettings}

import scala.collection.immutable.Seq

/**
 * Created by jittara on 4/5/17.
 */
object AMQPConnection extends Logging {

  implicit val factory = new ConnectionFactory()

  def getConnection: Connection = {
    val config = ConfigFactory.load()

    val list = config.getConfigList("newamqp.addresses")

    val settings: ConnectionSettings = ConnectionSettings.apply(config)

    val ad: Seq[Address] = Seq(Address("netsense-clusterrabbitmq.marathon.l4lb.thisdcos.directory", 5672))

    Connection(settings.copy(addresses = ad, username = "farallones", password = "sensity1"))
  }

  def consumerConnection: ConnectionFactory = {
    factory.setUsername(ConfigLoader.userName)
    factory.setPassword(ConfigLoader.password)
    factory.setPort(ConfigLoader.port)
    factory.setHost(ConfigLoader.host)
    log.debug(
      "AMQP connecting to the host " + ConfigLoader.host,
      " port " + ConfigLoader.port + " with username " + ConfigLoader.userName + " password " + ConfigLoader.password
    )
    factory
  }
}
