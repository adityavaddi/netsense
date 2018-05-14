package com.verizon.netsense.services.alert.services.database

import com.outworkers.phantom.dsl._
import com.verizon.netsense.services.alert.model.Alert
import com.verizon.netsense.services.alert.services.database.CassandraDBTest.alerts

import scala.concurrent.Future

abstract class Alerts extends Table[Alerts, Alert] with RootConnector {

  def truncateTable(): Future[ResultSet] = truncate().future()

  def createTable() = create.ifNotExists().future()

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
