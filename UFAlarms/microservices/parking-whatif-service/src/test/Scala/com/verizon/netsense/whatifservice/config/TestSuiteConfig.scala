package com.verizon.netsense.whatifservice.config

import com.outworkers.phantom.connectors.{CassandraConnection, ContactPoints}
import com.typesafe.config.ConfigFactory

/**
 * Created by jittara on 27/02/18.
 */
object TestSuiteConfig {

  lazy val configLoader = ConfigFactory.load()

  lazy val embeddedKafkaPort = configLoader.getInt("embedded-kafka.kafka-port")
  lazy val embeddedZkPort    = configLoader.getInt("embedded-kafka.zookeeper-port")
  private val hosts          = configLoader.getString("cassandra.host").split(",").toSeq
  private val port           = configLoader.getInt("cassandra.port")

  private val testHosts = configLoader.getString("cassandra.test-host").split(",").toSeq
  private val testPort  = configLoader.getInt("cassandra.test-port")
  private val keyspace  = configLoader.getString("cassandra.keyspace")

  lazy val whatIfConnector: CassandraConnection = ContactPoints(hosts, port)
    .keySpace(keyspace)

  lazy val whatIfTestConnector: CassandraConnection =
    ContactPoints(testHosts, testPort).keySpace(keyspace)

}
