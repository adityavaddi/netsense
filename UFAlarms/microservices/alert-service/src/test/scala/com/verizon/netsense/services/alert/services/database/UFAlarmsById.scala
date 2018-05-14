package com.verizon.netsense.services.alert.services.database

import com.outworkers.phantom.connectors.RootConnector
import com.outworkers.phantom.dsl._
import com.verizon.netsense.services.alert.database.CassandraDB.ufAlarmsById
import com.verizon.netsense.services.alert.model.UfAlarm

import scala.concurrent.Future

abstract class UFAlarmsById extends Table[UFAlarmsById, UfAlarm] with RootConnector {

  def truncateTable(): Future[ResultSet] = truncate().future()

  def createTable() = create.ifNotExists().future()

  override def tableName: String = ufAlarmsById

  object mappingid extends StringColumn with PartitionKey

  object alarmtype extends StringColumn

  object nodemodels extends SetColumn[String]

  object ufname extends OptionalStringColumn

  object description extends OptionalStringColumn

  object displaytocustomer extends BooleanColumn

  object displaytopartner extends BooleanColumn

  object created extends OptionalLongColumn

  object updated extends OptionalLongColumn

}
