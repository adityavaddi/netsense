package com.verizon.netsense.services.datamigration.connectors

import java.net.InetAddress

import scala.collection.JavaConverters._
import com.datastax.driver.core.Cluster
import com.verizon.netsense.services.datamigration.utils.Configuration.{cassandraHosts, keyspace}
import com.verizon.netsense.services.datamigration.utils.Logging

object CassandraConnector extends Logging {
  val cassandraIPList = cassandraHosts.map(x => InetAddress.getByName(x)).toList.asJava
  lazy val cluster = Cluster.builder().addContactPoints(cassandraIPList).build()
  lazy val session = cluster.connect(keyspace)

  def closeConnection(): Unit = {
    try {
      session.close()
      cluster.close()
    } catch {
      case ex: Exception => log.warn("Error closing Cassandra Connection: " + ex.getMessage)
    }
  }
}
