package com.verizon.netsense.model

import com.outworkers.phantom.CassandraTable
import com.outworkers.phantom.connectors.RootConnector
import com.outworkers.phantom.dsl._
import com.verizon.netsense.entity.Config

import scala.concurrent.Future

class ConfigModel extends CassandraTable[ConcreteConfigModel, Config] {

  override def tableName: String = "configs"

  object configid extends StringColumn(this) with PartitionKey

  object orgid extends StringColumn(this) with Index

  object siteid extends StringColumn(this) with Index

  object cfgname extends StringColumn(this)

  object cfgtoken extends StringColumn(this)

  object model extends StringColumn(this)

  object body extends StringColumn(this)

  override def fromRow(r: Row): Config =
    Config("Config", configid(r), orgid(r), siteid(r), cfgname(r), cfgtoken(r), model(r), body(r))
}

abstract class ConcreteConfigModel extends ConfigModel with RootConnector {

  def getConfigByCfgid(cfgid: String, siteid: String): Future[Option[Config]] =
    select
      .where(_.configid eqs cfgid)
      .and(_.siteid eqs siteid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .one()

  def getConfigBySiteId(siteid: String, orgid: String): Future[List[Config]] =
    select
      .where(_.siteid eqs siteid)
      .and(_.orgid eqs orgid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .allowFiltering()
      .fetch()

  def storeConfig(config: Config): Future[ResultSet] =
    insert
      .value(_.configid, config.cfgid)
      .value(_.orgid, config.orgid)
      .value(_.siteid, config.siteid)
      .value(_.cfgname, config.cfgname)
      .value(_.cfgtoken, config.token)
      .value(_.model, config.model)
      .value(_.body, config.body)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()

  def deleteConfigByCfgId(configid: String): Future[ResultSet] =
    delete
      .where(_.configid eqs configid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()

  def updateConfigByCfgId(configid: String, body: String, token: String): Future[ResultSet] =
    update
      .where(_.configid eqs configid)
      .modify(_.body setTo body)
      .and(_.cfgtoken setTo token)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()
}
