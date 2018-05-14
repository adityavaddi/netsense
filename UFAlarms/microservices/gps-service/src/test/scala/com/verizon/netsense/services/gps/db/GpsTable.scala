package com.verizon.netsense.services.gps.db


import com.outworkers.phantom.dsl._
import com.verizon.netsense.services.gps.helper.KafkaConfig
import com.verizon.netsense.services.gps.model.Gps

import scala.concurrent.Future


abstract class GpsTable extends CassandraTable[GpsTable, Gps] with RootConnector {

  override def tableName: String = KafkaConfig.gpsTable

  def storeGps(gps: Gps): Future[ResultSet] = {
    insert
      .value(_.nodeid,gps.nodeid)
      .value(_.name,gps.name)
      .value(_.orgid,gps.orgid)
      .value(_.siteid,gps.siteid)
      .value(_.lat,gps.latitude)
      .value(_.lon,gps.longitude)
      .value(_.latuseradded,gps.latuseradded)
      .value(_.lonuseradded,gps.lonuseradded)
      .value(_.created,gps.created)
      .value(_.updated,gps.updated)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()
  }



  def deleteGps(orgId: String, siteId: String, nodeId: String): Future[ResultSet] = {

    delete.where(_.nodeid eqs nodeId)
      .and(_.orgid eqs orgId)
      .and(_.siteid eqs siteId)
      .ifExists
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()

  }


  def getGps(nodeid: String,orgId: String, siteId: String): Future[Option[Gps]] = {
    select.where(_.nodeid eqs nodeid)
      .and(_.orgid eqs orgId)
      .and(_.siteid eqs siteId)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .one()
  }


  def getGpsByOrgIdAndSiteId(orgId: String, siteId: String): Future[List[Gps]] = {

    select.where(_.orgid eqs orgId)
      .and(_.siteid eqs siteId)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .fetch()
  }


  object nodeid extends StringColumn(this) with PrimaryKey

  object name extends OptionalStringColumn(this)

  object orgid extends StringColumn(this) with PartitionKey

  object siteid extends  StringColumn(this) with PartitionKey

  object lat extends OptionalDoubleColumn(this)

  object lon extends OptionalDoubleColumn(this)

  object latuseradded extends OptionalDoubleColumn(this)

  object lonuseradded extends OptionalDoubleColumn(this)

  object created extends OptionalLongColumn(this)

  object updated extends OptionalLongColumn(this)
}
