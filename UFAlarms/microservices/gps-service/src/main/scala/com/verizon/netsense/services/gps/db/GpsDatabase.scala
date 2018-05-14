package com.verizon.netsense.services.gps.db

import com.outworkers.phantom.connectors.CassandraConnection
import com.outworkers.phantom.dsl.{Database, DatabaseProvider}
import com.verizon.netsense.connector.CassandraConnector

class GpsDatabase(val connection: CassandraConnection) extends Database[GpsDatabase](connection) {
  object OrgHierarchy  extends Orghierarchy_By_Nodeid with connection.Connector
  object GpsRecord extends GpsTable with connection.Connector
}

object GpsDatabase extends GpsDatabase(CassandraConnector.connector)

object GpsEmbeddedDatabase extends GpsDatabase(CassandraConnector.testConnector)

trait GpsDataBaseProvider extends DatabaseProvider[GpsDatabase] {
  override def database: GpsDatabase
}

trait ProductionDataBase extends GpsDataBaseProvider {
  override val database: GpsDatabase = GpsDatabase
}

trait EmbeddedDatabase extends GpsDataBaseProvider {
  override val database: GpsDatabase = GpsEmbeddedDatabase
}