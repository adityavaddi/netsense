package com.verizon.netsense.services.eventsimulator.database

import com.outworkers.phantom.dsl._
import com.outworkers.phantom.keys.PartitionKey
import com.verizon.netsense.services.eventsimulator.model.Notification

import scala.concurrent.Future

abstract class Notifications extends CassandraTable[Notifications, Notification] with RootConnector {

  def truncateTable(): Future[ResultSet] =
    truncate().future()

  def createTable() = create.ifNotExists().future();

  object notificationid extends StringColumn(this) with PartitionKey

  object orgid extends StringColumn(this) with PrimaryKey

  object active extends BooleanColumn(this)

  object siteid extends OptionalStringColumn(this) with PrimaryKey

  object name extends OptionalStringColumn(this) with Index

  object msg extends OptionalStringColumn(this)

  object description extends OptionalStringColumn(this)

  object scope extends OptionalStringColumn(this)

  object window extends OptionalStringColumn(this)

  object hold_off extends OptionalIntColumn(this)

  object resend_interval extends OptionalIntColumn(this)

  object notificationtype extends SetColumn[String](this)

  object severity extends SetColumn[String](this)

  object emailUsersList extends SetColumn[String](this)

  object additionalEmails extends SetColumn[String](this)

  object smsUsersList extends SetColumn[String](this)

  object created extends OptionalLongColumn(this)

  object updated extends OptionalLongColumn(this)

}
