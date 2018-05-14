package com.verizon.netsense.services.eventsimulator.database

import com.outworkers.phantom.dsl._
import com.verizon.netsense.services.eventsimulator.model.ApAlarm

import scala.concurrent.Future

abstract class Device_Alarms extends CassandraTable[Device_Alarms, ApAlarm] with RootConnector {

  override def fromRow(r: Row): ApAlarm =
    ApAlarm(nodeid(r), alarmtype(r), severitycode(r), category(r), message(r), date(r).toString)

  def truncateTable(): Future[ResultSet] =
    truncate().future()

  def createTable(): Future[ResultSet] = create.ifNotExists().future()

  def getAlarm(nodeId: String, alarmType: String): Future[Option[ApAlarm]] =
    select
      .where(_.nodeid eqs nodeId)
      .and(_.alarmtype eqs alarmType)
      .one()

  object nodeid extends StringColumn(this) with PartitionKey

  object alarmtype extends StringColumn(this) with PrimaryKey

  object severitycode extends StringColumn(this)

  object category extends StringColumn(this)

  object message extends StringColumn(this)

  object date extends TimeUUIDColumn(this) with PrimaryKey

}
