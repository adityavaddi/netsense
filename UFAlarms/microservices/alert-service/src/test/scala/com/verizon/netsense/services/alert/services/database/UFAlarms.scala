package com.verizon.netsense.services.alert.services.database

import com.outworkers.phantom.connectors.RootConnector
import com.outworkers.phantom.dsl._
import com.verizon.netsense.services.alert.database.CassandraDB.{
  ufAlarms,
  READ_CONSISTENCY_LEVEL,
  WRITE_CONSISTENCY_LEVEL
}
import com.verizon.netsense.services.alert.model.UfAlarm

import scala.concurrent.Future

abstract class UFAlarms extends Table[UFAlarms, UfAlarm] with RootConnector {

  def truncateTable(): Future[ResultSet] = truncate().future()

  def createTable() = create.ifNotExists().future()

  override def tableName: String = ufAlarms

  object mappingid extends StringColumn

  object alarmtype extends StringColumn with PartitionKey

  object nodemodels extends SetColumn[String]

  object ufname extends OptionalStringColumn

  object description extends OptionalStringColumn

  object displaytocustomer extends BooleanColumn

  object displaytopartner extends BooleanColumn

  object created extends OptionalLongColumn

  object updated extends OptionalLongColumn

}
