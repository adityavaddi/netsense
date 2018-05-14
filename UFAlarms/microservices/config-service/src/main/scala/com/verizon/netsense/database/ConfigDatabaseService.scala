package com.verizon.netsense.database

import com.outworkers.phantom.dsl.ResultSet
import com.verizon.netsense.entity.Config
import com.verizon.netsense.metrics.Instrumented
import nl.grons.metrics.scala.Timer

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.concurrent.duration._

/**
 * Created by davor on 5/4/17.
 */

trait ConfigDatabaseBase {
  def getById(cfgid: String, siteid: String): Future[Option[Config]]
  def getBySiteId(siteid: String, orgid: String): Future[List[Config]]
  def storeConfig(config: Config): Future[ResultSet]
  def deleteConfigById(configid: String): Future[ResultSet]
  def updateConfigByCfgId(configid: String, body: String, token: String): Future[ResultSet]
}

trait ConfigDatabaseService extends ConfigDatabaseBase with ConfigProductionDatabase with Instrumented {

  private[this] val cassandraGetConfigIdTimer: Timer = metrics.timer("cassandra-get-config-id-timer")
  private[this] val cassandraGetConfigSiteIdTimer: Timer = metrics.timer("cassandra-get-config-siteid-timer")
  private[this] val cassandraStoreConfigTimer: Timer = metrics.timer("cassandra-store-config-timer")
  private[this] val cassandraDeleteConfigTimer: Timer = metrics.timer("cassandra-delete-config-id-timer")
  private[this] val cassandraUpdateConfigTimer: Timer = metrics.timer("cassandra-update-config-id-timer")

  override def getById(cfgid: String, siteid: String): Future[Option[Config]] =
    cassandraGetConfigIdTimer.time {
      database.configModel.getConfigByCfgid(cfgid, siteid)
    }

  override def getBySiteId(siteid: String, orgid: String): Future[List[Config]] =
    cassandraGetConfigSiteIdTimer.time {
      database.configModel.getConfigBySiteId(siteid, orgid)
    }

  override def storeConfig(config: Config): Future[ResultSet] =
    cassandraStoreConfigTimer.time {
      database.configModel.storeConfig(config)
    }

  override def deleteConfigById(configid: String): Future[ResultSet] =
    cassandraDeleteConfigTimer.time {
      database.configModel.deleteConfigByCfgId(configid)
    }

  override def updateConfigByCfgId(configid: String, body: String, token: String): Future[ResultSet] =
    cassandraUpdateConfigTimer.time {
      database.configModel.updateConfigByCfgId(configid, body, token)
    }
}

object ConfigDatabaseService extends ConfigDatabaseService with ConfigProductionDatabase
class ConfigDatabaseServiceImpl extends ConfigDatabaseService with ConfigProductionDatabase