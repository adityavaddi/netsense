package com.verizon.netsense.model

import com.outworkers.phantom.CassandraTable
import com.outworkers.phantom.column.TimeUUIDColumn
import com.outworkers.phantom.connectors.RootConnector
import com.outworkers.phantom.dsl._
import com.verizon.netsense.entity._

import scala.concurrent.Future

/**
 * Created by davor on 7/6/17.
 */
class DeviceConnectionStatusModel extends CassandraTable[ConcreteDeviceConnectionStatusModel, DeviceConnectionStatus] {
  override def tableName: String = "connection_status"

  object nodeid extends StringColumn(this) with PartitionKey

  object isconnected extends BooleanColumn(this)

  object since extends TimeUUIDColumn(this)

  override def fromRow(r: Row): DeviceConnectionStatus =
    DeviceConnectionStatus("ConnectionStatus", nodeid(r), isconnected(r), since(r))
}

abstract class ConcreteDeviceConnectionStatusModel extends DeviceConnectionStatusModel with RootConnector {

  def getConnectionStatusByNodeid(nodeid: String): Future[Option[DeviceConnectionStatus]] =
    select
      .where(_.nodeid eqs nodeid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .one()

  def storeConnectionStatus(nodeId: String, isConnected: Boolean, since: UUID): Future[ResultSet] =
    insert
      .value(_.nodeid, nodeId)
      .value(_.isconnected, isConnected)
      .value(_.since, since)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()
}
