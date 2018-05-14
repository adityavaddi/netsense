package com.verizon.netsense.services.eventsimulator.database

import com.outworkers.phantom.dsl._
import com.verizon.netsense.services.eventsimulator.model.Alert

import scala.concurrent.Future

abstract class Alerts extends CassandraTable[Alerts, Alert] with RootConnector {

  def truncateTable(): Future[ResultSet] = truncate().future()

  def createTable() = create.ifNotExists().future()

  def store(alert: Alert): Future[ResultSet] =
    insert
      .value(_.alertid, alert.alertId)
      .value(_.name, alert.name)
      .value(_.msg, alert.msg)
      .value(_.nodeid, alert.nodeId)
      .value(_.orgid, alert.orgId)
      .value(_.severity, alert.severity)
      .value(_.alarmtype, alert.`type`)
      .value(_.category, alert.category)
      .value(_.created, alert.created)
      .value(_.updated, alert.updated)
      .value(_.sitename, alert.siteName)
      .value(_.nodename, alert.nodeName)
      .value(_.orgname, alert.orgName)
      .value(_.siteaddress, alert.siteAddress)
      .value(_.siteid, alert.siteId)
      .value(_.bssid, alert.bssId)
      .value(_.nodehw, alert.nodeHw)
      .value(_.active, alert.active)
      .consistencyLevel_=(ConsistencyLevel.LOCAL_QUORUM)
      .future()

  object alertid extends StringColumn(this) with PartitionKey

  object name extends StringColumn(this)

  object msg extends StringColumn(this)

  object nodeid extends StringColumn(this) with Index

  object orgid extends StringColumn(this) with PrimaryKey

  object severity extends StringColumn(this)

  object alarmtype extends StringColumn(this) with Index

  object category extends StringColumn(this)

  object created extends StringColumn(this)

  object updated extends StringColumn(this)

  object sitename extends OptionalStringColumn(this)

  object nodename extends OptionalStringColumn(this)

  object orgname extends OptionalStringColumn(this)

  object siteaddress extends OptionalStringColumn(this)

  object siteid extends StringColumn(this) with Index

  object bssid extends OptionalStringColumn(this)

  object nodehw extends OptionalStringColumn(this)

  object active extends BooleanColumn(this) with Index
}
