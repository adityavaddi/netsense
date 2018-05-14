package com.verizon.netsense.database

import com.outworkers.phantom.connectors.CassandraConnection
import com.outworkers.phantom.database.Database
import com.verizon.netsense.connector.CassandraConnector._
import com.verizon.netsense.model.ConcreteDeviceConnectionStatusModel

/**
 * Created by davor on 7/6/17.
 */
class DeviceConnectionStatusDatabase(override val connector: CassandraConnection)
    extends Database[DeviceConnectionStatusDatabase](connector) {

  object deviceConnectionStatusModel extends ConcreteDeviceConnectionStatusModel with connector.Connector

}

object DeviceConnectionStatusProductionDb extends DeviceConnectionStatusDatabase(connector)

trait DeviceStatusProductionDatabaseProvider {

  def database: DeviceConnectionStatusDatabase
}

trait DeviceConnectionStatusProductionDatabase extends DeviceStatusProductionDatabaseProvider {

  override val database = DeviceConnectionStatusProductionDb
}

object DeviceConnectionStatusEmbeddedDb extends DeviceConnectionStatusDatabase(testConnector)

trait DeviceConnectionStatusEmbeddedDatabaseProvider {
  def database: DeviceConnectionStatusDatabase
}

trait DeviceStatusEmbeddedDatabase extends DeviceConnectionStatusEmbeddedDatabaseProvider {
  override val database = DeviceConnectionStatusEmbeddedDb
}
