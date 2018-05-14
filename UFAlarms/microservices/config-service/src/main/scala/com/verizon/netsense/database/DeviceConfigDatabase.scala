package com.verizon.netsense.database

import com.outworkers.phantom.connectors.CassandraConnection
import com.verizon.netsense.connector.CassandraConnector._
import com.verizon.netsense.model.ConcreteDeviceConfigModel

class DeviceConfigDatabase(override val connector: CassandraConnection)
    extends com.outworkers.phantom.database.Database[DeviceConfigDatabase](connector) {

  object deviceConfigModel extends ConcreteDeviceConfigModel with connector.Connector

}

object DeviceConfigProductionDb extends DeviceConfigDatabase(connector)

trait DeviceConfigProductionDatabaseProvider {

  def database: DeviceConfigDatabase
}

trait DeviceConfigProductionDatabase extends DeviceConfigProductionDatabaseProvider {

  override val database = DeviceConfigProductionDb
}

object DeviceConfigEmbeddedDb extends DeviceConfigDatabase(testConnector)

trait DeviceConfigEmbeddedDatabaseProvider {
  def database: DeviceConfigDatabase
}

trait DeviceConfigEmbeddedDatabase extends DeviceConfigEmbeddedDatabaseProvider {
  override val database = DeviceConfigEmbeddedDb
}
