package com.verizon.netsense.services.alert.services.database

import com.outworkers.phantom.dsl._
import com.outworkers.phantom.keys.PartitionKey
import com.verizon.netsense.services.alert.model.Notification
import com.verizon.netsense.services.alert.services.database.CassandraDBTest.notifications
import scala.concurrent.Future

abstract class Notifications extends Table[Notifications, Notification] with RootConnector {

  def truncateTable(): Future[ResultSet] = truncate().future()

  def createTable() = create.ifNotExists().future()

  override def tableName: String = notifications

  object notificationId extends StringColumn with PrimaryKey

  object orgId extends StringColumn with PartitionKey

  object active extends BooleanColumn

  object siteId extends StringColumn with PartitionKey

  object name extends OptionalStringColumn

  object msg extends OptionalStringColumn

  object description extends OptionalStringColumn

  object scope extends OptionalStringColumn

  object window extends OptionalStringColumn

  object hold_off extends OptionalIntColumn

  object resend_interval extends OptionalIntColumn

  object notificationType extends SetColumn[String]

  object severity extends SetColumn[String]

  object emailUsersList extends SetColumn[String]

  object additionalEmails extends SetColumn[String]

  object smsUsersList extends SetColumn[String]

  object created extends OptionalLongColumn

  object updated extends OptionalLongColumn

}
