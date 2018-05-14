package com.verizon.netsense.connector

import com.outworkers.phantom.connectors.{CassandraConnection, ContactPoints}
import com.typesafe.config.ConfigFactory

object CassandraConnector {

  private val config = ConfigFactory.load()

  private val hosts = config.getString("cassandra.host").split(",").toSeq
  private val port  = config.getInt("cassandra.port")

  private val testHosts = config.getString("cassandra.test-host").split(",").toSeq
  private val testPort  = config.getInt("cassandra.test-port")
  private val keyspace = config.getString("cassandra.keyspace")

  lazy val connector: CassandraConnection = ContactPoints(hosts, port)
    .keySpace(keyspace)

  lazy val testConnector: CassandraConnection = ContactPoints(testHosts, testPort).keySpace(keyspace)

}
