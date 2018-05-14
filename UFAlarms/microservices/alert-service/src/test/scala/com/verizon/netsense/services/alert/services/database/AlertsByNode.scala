package com.verizon.netsense.services.alert.services.database

import com.outworkers.phantom.dsl._
import com.verizon.netsense.services.alert.model.Alert
import com.verizon.netsense.services.alert.services.database.CassandraDBTest.alertsByNodeId

import scala.concurrent.Future

abstract class AlertsByNode extends Table[AlertsByNode, Alert] with RootConnector {

  def truncateTable(): Future[ResultSet] = truncate().future()

  def createTable() = create.ifNotExists().future()

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
