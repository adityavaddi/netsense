package com.verizon.netsense.services.alert.database

import java.time.Instant

import com.outworkers.phantom.dsl._
import com.verizon.netsense.services.alert.database.CassandraDB.{alarms, WRITE_CONSISTENCY_LEVEL}
import com.verizon.netsense.services.alert.model.ApAlarm
import org.apache.cassandra.utils.UUIDGen

import scala.concurrent.Future

abstract class Alarms extends Table[Alarms, ApAlarm] with RootConnector {

  def store(alarm: ApAlarm): Future[ResultSet] =
    insert
      .value(_.nodeId, alarm.nodeId)
      .value(_.alarmType, alarm.alarmType)
      .value(_.severityCode, alarm.severity)
      .value(_.category, alarm.category)
      .value(_.message, alarm.msg)
      .value(_.date, UUIDGen.getTimeUUID(Instant.parse(alarm.created).toEpochMilli))
      .consistencyLevel_=(WRITE_CONSISTENCY_LEVEL)
      .future()

  override def tableName: String = alarms

  object nodeId extends StringColumn with PartitionKey

  object alarmType extends StringColumn with PrimaryKey

  object severityCode extends StringColumn

  object category extends StringColumn

  object message extends StringColumn

  object date extends TimeUUIDColumn with PrimaryKey
}
