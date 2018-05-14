package com.verizon.netsense.database

import com.outworkers.phantom.dsl.ResultSet
import com.verizon.netsense.entity.DeviceCfg
import com.verizon.netsense.metrics.Instrumented
import nl.grons.metrics.scala.Timer

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.concurrent.duration._

/**
 * Created by davor on 5/5/17.
 */
trait DeviceConfigDatabaseService extends DeviceConfigProductionDatabase with Instrumented {

  private[this] val cassandraGetConfigByNodeidTimer: Timer = metrics.timer("cassandra-get-config-by-nodeid-timer")
  private[this] val cassandraGetConfigByIdTimer: Timer = metrics.timer("cassandra-get-config-by-id-timer")
  private[this] val cassandraStoreConfigTimer: Timer = metrics.timer("cassandra-store-config-timer")
  private[this] val cassandraUpdateConfigTimer: Timer = metrics.timer("cassandra-update-config-timer")

  def getConfigByNodeId(nodeid: String): Future[Option[DeviceCfg]] =
    cassandraGetConfigByNodeidTimer.time {
      database.deviceConfigModel.getConfigByNodeId(nodeid)
    }

  def getNodeConfigsByConfigId(configId: String): Future[List[DeviceCfg]] =
    cassandraGetConfigByIdTimer.time {
      database.deviceConfigModel.getNodeConfigsByConfigId(configId)
    }

  def storeConfig(nodeid: String, token: String, configId: String, cfg: String): Future[ResultSet] =
    cassandraStoreConfigTimer.time {
      database.deviceConfigModel.storeConfig(nodeid, token, configId, cfg)
    }

  def updateConfigToken(nodeId: String, token: String): Future[ResultSet] =
    cassandraUpdateConfigTimer.time {
      database.deviceConfigModel.updateConfigToken(nodeId, token)
    }

  def deleteDeviceConfig(nodeId: String): Future[ResultSet] =
    cassandraUpdateConfigTimer.time {
      database.deviceConfigModel.deleteConfigByNodeId(nodeId)
    }
}

object DeviceConfigDatabaseService extends DeviceConfigDatabaseService with DeviceConfigProductionDatabase
