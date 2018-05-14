package com.verizon.netsense.services.alert.database

import com.outworkers.phantom.dsl._
import com.verizon.netsense.services.alert.database.CassandraDB.{
  alerts,
  CLEAR_ALARM_TTL,
  READ_CONSISTENCY_LEVEL,
  WRITE_CONSISTENCY_LEVEL
}
import com.verizon.netsense.services.alert.model.{AlarmSeverity, Alert}
import scala.concurrent.duration._

import scala.concurrent.Future

abstract class Alerts extends Table[Alerts, Alert] with RootConnector {

  def store(alert: Alert): Future[ResultSet] =
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

  def updateAlert(alert: Alert): Future[ResultSet] =
    update
      .where(_.alertId eqs alert.alertId)
      .and(_.orgId eqs alert.orgId)
      .and(_.siteId eqs alert.siteId)
      .modify(_.active setTo alert.active)
      .and(_.msg setTo alert.msg)
      .and(_.severity setTo alert.severity)
      .and(_.category setTo alert.category)
      .and(_.updated setTo alert.updated)
      .consistencyLevel_=(WRITE_CONSISTENCY_LEVEL)
      .future()

  def dismissAlert(alertId: String, orgId: String, siteId: String): Future[ResultSet] =
    update
      .where(_.alertId eqs alertId)
      .and(_.orgId eqs orgId)
      .and(_.siteId eqs siteId)
      .modify(_.severity setTo Some(AlarmSeverity.Clear.toString))
      .ttl(CLEAR_ALARM_TTL)
      .consistencyLevel_=(WRITE_CONSISTENCY_LEVEL)
      .future()

  def deleteAlert(alertId: String, orgId: String, siteId: String): Future[ResultSet] =
    delete
      .where(_.alertId eqs alertId)
      .and(_.orgId eqs orgId)
      .and(_.siteId eqs siteId)
      .consistencyLevel_=(WRITE_CONSISTENCY_LEVEL)
      .future()

  def getAlertForOrgAndSite(orgId: String, siteId: String): Future[List[Alert]] =
    select
      .where(_.orgId eqs orgId)
      .and(_.siteId eqs siteId)
      .consistencyLevel_=(READ_CONSISTENCY_LEVEL)
      .fetch()

  def getAlertForAlertId(alertId: String, orgId: String, siteId: String): Future[Option[Alert]] =
    select
      .where(_.alertId eqs alertId)
      .and(_.orgId eqs orgId)
      .and(_.siteId eqs siteId)
      .consistencyLevel_=(READ_CONSISTENCY_LEVEL)
      .one()

  override def tableName: String = alerts

  object alertId extends StringColumn with PrimaryKey

  object name extends OptionalStringColumn

  object msg extends OptionalStringColumn

  object nodeId extends StringColumn

  object orgId extends StringColumn with PartitionKey

  object severity extends OptionalStringColumn

  object alarmType extends StringColumn

  object category extends OptionalStringColumn

  object created extends OptionalStringColumn

  object updated extends OptionalStringColumn

  object siteName extends OptionalStringColumn

  object nodeName extends OptionalStringColumn

  object orgName extends OptionalStringColumn

  object siteAddress extends OptionalStringColumn

  object siteId extends StringColumn with PartitionKey

  object bssId extends OptionalStringColumn

  object nodeHw extends OptionalStringColumn

  object active extends BooleanColumn
}
