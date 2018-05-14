package com.verizon.netsense.services.gps.db

import com.outworkers.phantom.connectors.{CassandraConnection, ContactPoints}
import com.outworkers.phantom.dsl._
import com.verizon.netsense.utils.ConfigLoader

object Defaults {
  val cassandraHost     = ConfigLoader.config.getString("cassandra.host")
  val cassandraKeyspace = ConfigLoader.config.getString("cassandra.keyspace")
  val keySpaceBuilder   = ContactPoints(Seq(cassandraHost))
  val connection        = keySpaceBuilder.keySpace(cassandraKeyspace)
}

class CassandraDBTest(val connection: CassandraConnection) extends Database[CassandraDBTest](connection) {
  object GpsRecords  extends GpsTable with Connector
  object OrgHierarchy  extends Orghierarchy_By_Nodeid with Connector
}

object CassandraDBTest extends CassandraDBTest(Defaults.connection)
