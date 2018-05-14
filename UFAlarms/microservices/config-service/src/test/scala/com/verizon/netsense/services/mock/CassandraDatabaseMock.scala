package com.verizon.netsense.services.mock

import com.outworkers.phantom.dsl.ResultSet
import com.verizon.netsense.database.ConfigDatabaseBase
import com.verizon.netsense.entity.Config

import scala.concurrent.Future

class CassandraDatabaseMock extends ConfigDatabaseBase {

  var databaseConfigMap: Map[String, Config] = Map[String, Config]()

  override def getById(cfgid: String, siteid: String): Future[Option[Config]] = {
    val result = databaseConfigMap.get(cfgid)
    Future.successful(result)
  }

  override def getBySiteId(siteid: String, orgid: String): Future[List[Config]] = {
    Future.successful(List[Config]())
  }

  override def storeConfig(config: Config): Future[ResultSet] = {
    databaseConfigMap += (config.cfgid -> config)
    Future.failed(new Exception("test Exception"))
  }

  override def deleteConfigById(configid: String): Future[ResultSet] = {
    databaseConfigMap -= configid
    Future.failed(new Exception("test Exception"))
  }

  override def updateConfigByCfgId(configid: String, body: String, token: String): Future[ResultSet] = {
    Future.failed(new Exception("test Exception"))
  }
}
