package com.verizon.netsense.services.alert.services.database

import com.outworkers.phantom.dsl._
import com.verizon.netsense.services.alert.model.ApAlarm
import com.verizon.netsense.services.alert.services.database.CassandraDBTest.alarms

import scala.concurrent.Future

abstract class Alarms extends Table[Alarms, ApAlarm] with RootConnector {

  override def fromRow(r: Row): ApAlarm =
    ApAlarm(nodeId(r), alarmType(r), severityCode(r), category(r), message(r), date(r).toString)

  def truncateTable(): Future[ResultSet] = truncate().future()

  def createTable(): Future[Seq[ResultSet]] = create.ifNotExists().future()

  def getAlarm(nodeId: String, alarmType: String): Future[Option[ApAlarm]] =
    select
      .where(_.nodeId eqs nodeId)
      .and(_.alarmType eqs alarmType)
      .one()

  override def tableName: String = alarms

  object nodeId extends StringColumn with PartitionKey

  object alarmType extends StringColumn with PrimaryKey

  object severityCode extends StringColumn

  object category extends StringColumn

  object message extends StringColumn

  object date extends TimeUUIDColumn with PrimaryKey

}
