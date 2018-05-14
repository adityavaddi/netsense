package com.verizon.netsense.model

import com.outworkers.phantom.CassandraTable
import com.outworkers.phantom.connectors.RootConnector
import com.outworkers.phantom.dsl._
import com.verizon.netsense.entity.DeviceCfg

import scala.concurrent.Future

class DeviceConfigModel extends CassandraTable[ConcreteDeviceConfigModel, DeviceCfg] {

  override def tableName: String = "deviceconfigs"

  object nodeid extends StringColumn(this) with PartitionKey

  object configid extends StringColumn(this) with Index

  object cfg extends StringColumn(this)

  object cfgtoken extends StringColumn(this)

  override def fromRow(r: Row): DeviceCfg =
    DeviceCfg("DeviceCfg", nodeid(r), configid(r), cfg(r), cfgtoken(r))
}

abstract class ConcreteDeviceConfigModel extends DeviceConfigModel with RootConnector {

  def getConfigByNodeId(nodeid: String): Future[Option[DeviceCfg]] =
    select
      .where(_.nodeid eqs nodeid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .one()

  def getNodeConfigsByConfigId(configid: String): Future[List[DeviceCfg]] =
    select
      .where(_.configid eqs configid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .allowFiltering()
      .fetch()

  def storeConfig(nodeid: String, token: String, configId: String, cfg: String): Future[ResultSet] =
    insert
      .value(_.nodeid, nodeid)
      .value(_.configid, configId)
      .value(_.cfg, cfg)
      .value(_.cfgtoken, token)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()

  def updateConfigToken(nodeid: String, token: String): Future[ResultSet] =
    update
        .where(_.nodeid eqs nodeid)
        .modify(_.cfgtoken setTo token)
        .consistencyLevel_=(ConsistencyLevel.ONE)
        .future()

  def deleteConfigByNodeId(nodeid: String): Future[ResultSet] =
    delete
      .where(_.nodeid eqs nodeid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()
}
