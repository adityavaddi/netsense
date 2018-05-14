package com.verizon.netsense.model

import com.outworkers.phantom.CassandraTable
import com.outworkers.phantom.connectors.RootConnector
import com.outworkers.phantom.dsl._
import com.verizon.netsense.entity.Device

import scala.concurrent.Future

/**
 * Created by brefsdal on 3/9/17.
 */
class DeviceModel extends CassandraTable[ConcreteDeviceModel, Device] {

  override def tableName: String = "devices"

  object nodeid extends StringColumn(this) with PartitionKey

  object siteid extends StringColumn(this) with ClusteringOrder

  object orgid extends StringColumn(this)

  override def fromRow(r: Row): Device = Device("Device", nodeid(r), siteid(r), orgid(r))
}

abstract class ConcreteDeviceModel extends DeviceModel with RootConnector {

  def getDevicesByNodeid(nodeid: String): Future[List[Device]] =
    select
      .where(_.nodeid eqs nodeid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .fetch()

  def getDevicesBySiteid(siteid: String): Future[Option[Device]] =
    select
      .where(_.siteid eqs siteid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .one()

  def storeDevice(device: Device): Future[ResultSet] =
    insert
      .value(_.nodeid, device.nodeId)
      .value(_.siteid, device.siteId)
      .value(_.orgid, device.orgId)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()
}
