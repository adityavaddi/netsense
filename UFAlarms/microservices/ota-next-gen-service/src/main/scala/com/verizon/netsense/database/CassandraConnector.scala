package com.verizon.netsense.database

import com.outworkers.phantom.connectors.{CassandraConnection, ContactPoints}
import com.typesafe.config.ConfigFactory

object CassandraConnector {

  private val config = ConfigFactory.load()
  private val hosts = config.getString("cassandra.host").split(",").toSeq
  private val port = config.getInt("cassandra.port")
  private val keyspace = config.getString("cassandra.keyspace")

  lazy val connector: CassandraConnection = ContactPoints(hosts, port)
    .keySpace(keyspace)

  private val testPort = 9142

  lazy val testConnector: CassandraConnection = ContactPoints(Seq("127.0.0.1"), testPort).keySpace(keyspace)

}