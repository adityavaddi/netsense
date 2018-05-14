package com.verizon.netsense.services.alert.database

import com.outworkers.phantom.dsl._
import com.outworkers.phantom.keys.PartitionKey
import com.verizon.netsense.services.alert.database.CassandraDB.{
  notifications,
  READ_CONSISTENCY_LEVEL,
  WRITE_CONSISTENCY_LEVEL
}
import com.verizon.netsense.services.alert.model.Notification
import scala.concurrent.Future

abstract class Notifications extends Table[Notifications, Notification] with RootConnector {

  def getById(notificationId: String, orgId: String, siteId: String): Future[Option[Notification]] =
    select
      .where(_.notificationId eqs notificationId)
      .and(_.orgId eqs orgId)
      .and(_.siteId eqs siteId)
      .consistencyLevel_=(READ_CONSISTENCY_LEVEL)
      .one()

  def getBySiteId(orgId: String, siteId: String): Future[List[Notification]] =
    select
      .where(_.orgId eqs orgId)
      .and(_.siteId eqs siteId)
      .consistencyLevel_=(READ_CONSISTENCY_LEVEL)
      .fetch()

  def activate(notificationId: String, orgId: String, siteId: String): Future[ResultSet] =
    update
      .where(_.notificationId eqs notificationId)
      .and(_.orgId eqs orgId)
      .and(_.siteId eqs siteId)
      .modify(_.active setTo true)
      .consistencyLevel_=(WRITE_CONSISTENCY_LEVEL)
      .future()

  def deActivate(notificationId: String, orgId: String, siteId: String): Future[ResultSet] =
    update
      .where(_.notificationId eqs notificationId)
      .and(_.orgId eqs orgId)
      .and(_.siteId eqs siteId)
      .modify(_.active setTo false)
      .consistencyLevel_=(WRITE_CONSISTENCY_LEVEL)
      .future()

  def modify(notification: Notification): Future[ResultSet] =
    update
      .where(_.notificationId eqs notification.notificationId)
      .and(_.orgId eqs notification.orgId)
      .and(_.siteId eqs notification.siteId)
      .modify(_.active setTo notification.active)
      .and(_.msg setTo notification.msg)
      .and(_.name setTo notification.name)
      .and(_.description setTo notification.description)
      .and(_.scope setTo notification.scope)
      .and(_.window setTo notification.window)
      .and(_.hold_off setTo notification.holdOff)
      .and(_.resend_interval setTo notification.resendInterval)
      .and(_.notificationType setTo notification.notificationType)
      .and(_.severity setTo notification.severity)
      .and(_.emailUsersList setTo notification.emailUsersList)
      .and(_.additionalEmails setTo notification.additionalEmails)
      .and(_.smsUsersList setTo notification.smsUsersList)
      .and(_.updated setTo notification.updated)
      .consistencyLevel_=(WRITE_CONSISTENCY_LEVEL)
      .future()

  def store(notification: Notification): Future[ResultSet] =
    insert
      .value(_.notificationId, notification.notificationId)
      .value(_.orgId, notification.orgId)
      .value(_.active, notification.active)
      .value(_.siteId, notification.siteId)
      .value(_.name, notification.name)
      .value(_.msg, notification.msg)
      .value(_.description, notification.description)
      .value(_.scope, notification.scope)
      .value(_.window, notification.window)
      .value(_.hold_off, notification.holdOff)
      .value(_.resend_interval, notification.resendInterval)
      .value(_.notificationType, notification.notificationType)
      .value(_.severity, notification.severity)
      .value(_.emailUsersList, notification.emailUsersList)
      .value(_.additionalEmails, notification.additionalEmails)
      .value(_.smsUsersList, notification.smsUsersList)
      .value(_.created, notification.created)
      .value(_.updated, notification.updated)
      .consistencyLevel_=(WRITE_CONSISTENCY_LEVEL)
      .future()

  def deleteNotification(notificationId: String, orgId: String, siteId: String): Future[ResultSet] =
    delete
      .where(_.notificationId eqs notificationId)
      .and(_.orgId eqs orgId)
      .and(_.siteId eqs siteId)
      .consistencyLevel_=(WRITE_CONSISTENCY_LEVEL)
      .future()

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
