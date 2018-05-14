package com.verizon.netsense.database

import com.outworkers.phantom.connectors.CassandraConnection
import com.outworkers.phantom.database.Database
import com.verizon.netsense.connector.CassandraConnector._
import com.verizon.netsense.model.ConcreteDeviceModel

/**
 * Created by brefsdal on 3/7/17.
 */
class DeviceDatabase(override val connector: CassandraConnection) extends Database[DeviceDatabase](connector) {

  object deviceModel extends ConcreteDeviceModel with connector.Connector

}

object DeviceProductionDb extends DeviceDatabase(connector)

trait DeviceProductionDatabaseProvider {

  def database: DeviceDatabase
}

trait DeviceProductionDatabase extends DeviceProductionDatabaseProvider {

  override val database = DeviceProductionDb
}

object DeviceEmbeddedDb extends DeviceDatabase(testConnector)

trait DeviceEmbeddedDatabaseProvider {
  def database: DeviceDatabase
}

trait DeviceEmbeddedDatabase extends DeviceEmbeddedDatabaseProvider {
  override val database = DeviceEmbeddedDb
}
