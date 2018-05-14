package com.verizon.netsense.services.alert.database

import com.outworkers.phantom.dsl._
import com.verizon.netsense.services.alert.database.CassandraDB.{
  alertsByNodeId,
  CLEAR_ALARM_TTL,
  READ_CONSISTENCY_LEVEL,
  WRITE_CONSISTENCY_LEVEL
}
import com.verizon.netsense.services.alert.model.{AlarmSeverity, Alert}

import scala.concurrent.Future

abstract class AlertsByNode extends Table[AlertsByNode, Alert] with RootConnector {

  def getByNodeIdAndSiteId(nodeId: String, alarmType: String, orgId: String, siteId: String): Future[Option[Alert]] =
    select
      .where(_.nodeId eqs nodeId)
      .and(_.alarmType eqs alarmType)
      .and(_.orgId eqs orgId)
      .and(_.siteId eqs siteId)
      .consistencyLevel_=(READ_CONSISTENCY_LEVEL)
      .one()

  def getByNodeId(nodeId: String): Future[List[Alert]] =
    select
      .where(_.nodeId eqs nodeId)
      .consistencyLevel_=(READ_CONSISTENCY_LEVEL)
      .fetch

  def storeByNodeId(alert: Alert): Future[ResultSet] =
    insert
      .value(_.alertId, alert.alertId)
      .value(_.name, alert.name)
      .value(_.msg, alert.msg)
      .value(_.nodeId, alert.nodeId)
      .value(_.orgId, alert.orgId)
      .value(_.severity, alert.severity)
      .value(_.alarmType, alert.`type`)
      .value(_.category, alert.category)
      .value(_.created, alert.created)
      .value(_.updated, alert.updated)
      .value(_.siteName, alert.siteName)
      .value(_.nodeName, alert.nodeName)
      .value(_.orgName, alert.orgName)
      .value(_.siteAddress, alert.siteAddress)
      .value(_.siteId, alert.siteId)
      .value(_.bssId, alert.bssId)
      .value(_.nodeHw, alert.nodeHw)
      .value(_.active, alert.active)
      .consistencyLevel_=(WRITE_CONSISTENCY_LEVEL)
      .future()

  def updateAlertByNodeId(alert: Alert): Future[ResultSet] =
    update
      .where(_.nodeId eqs alert.nodeId)
      .and(_.alarmType eqs alert.`type`)
      .and(_.orgId eqs alert.orgId)
      .and(_.siteId eqs alert.siteId)
      .modify(_.active setTo alert.active)
      .and(_.msg setTo alert.msg)
      .and(_.severity setTo alert.severity)
      .and(_.category setTo alert.category)
      .and(_.updated setTo alert.updated)
      .consistencyLevel_=(WRITE_CONSISTENCY_LEVEL)
      .future()

  def dismissAlertByNodeId(nodeId: String, alarmType: String, orgId: String, siteId: String): Future[ResultSet] =
    update
      .where(_.nodeId eqs nodeId)
      .and(_.alarmType eqs alarmType)
      .and(_.orgId eqs orgId)
      .and(_.siteId eqs siteId)
      .modify(_.severity setTo Some(AlarmSeverity.Clear.toString))
      .ttl(CLEAR_ALARM_TTL)
      .consistencyLevel_=(WRITE_CONSISTENCY_LEVEL)
      .future()

  def deleteAlertByNodeId(nodeId: String, alarmType: String, orgId: String, siteId: String): Future[ResultSet] =
    delete
      .where(_.nodeId eqs nodeId)
      .and(_.alarmType eqs alarmType)
      .and(_.orgId eqs orgId)
      .and(_.siteId eqs siteId)
      .consistencyLevel_=(WRITE_CONSISTENCY_LEVEL)
      .future()

  override def tableName: String = alertsByNodeId

  object alertId extends StringColumn

  object name extends OptionalStringColumn

  object msg extends OptionalStringColumn

  object nodeId extends StringColumn with PartitionKey

  object orgId extends StringColumn with PrimaryKey

  object severity extends OptionalStringColumn

  object alarmType extends StringColumn with PrimaryKey

  object category extends OptionalStringColumn

  object created extends OptionalStringColumn

  object updated extends OptionalStringColumn

  object siteName extends OptionalStringColumn

  object nodeName extends OptionalStringColumn

  object orgName extends OptionalStringColumn

  object siteAddress extends OptionalStringColumn

  object siteId extends StringColumn with PrimaryKey

  object bssId extends OptionalStringColumn

  object nodeHw extends OptionalStringColumn

  object active extends BooleanColumn
}
