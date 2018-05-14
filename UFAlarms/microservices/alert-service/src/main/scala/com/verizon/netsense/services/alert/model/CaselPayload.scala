package com.verizon.netsense.services.alert.model

import java.time.Instant
import java.util.Calendar

import com.fasterxml.jackson.annotation.JsonProperty
import com.verizon.netsense.entity.Entity
import com.verizon.netsense.services.alert.customexceptions._

import scala.concurrent.Future

/**
 * Created by vermri5 on 8/2/17.
 */
case class NodeProps(@JsonProperty("nodeid") nodeId: String)

case class SiteProps(@JsonProperty("siteid") siteId: String)

case class OrgProps(@JsonProperty("orgid") orgId: String)

case class UserProps(@JsonProperty("usertype") userType: Option[String] = None)

case class AlertProps(@JsonProperty("alertid") alertId: Option[String],
                      @JsonProperty name: Option[String],
                      @JsonProperty `type`: Option[String],
                      @JsonProperty("nodeid") nodeId: Option[String] = None,
                      @JsonProperty msg: Option[String] = None,
                      @JsonProperty severity: Option[String] = None,
                      @JsonProperty category: Option[String] = None)
    extends Entity {

  def toAlert(orgId: String, siteId: String): Alert = {
    if (alertId.getOrElse("").isEmpty || nodeId.getOrElse("").isEmpty || `type`.getOrElse("").isEmpty ||
        orgId.isEmpty || siteId.isEmpty) throw new AlertIdMissingException
    Alert(
      alertId.get,
      name,
      msg,
      nodeId.get,
      orgId,
      severity,
      `type`.get,
      category,
      Some(Calendar.getInstance().toInstant.toString),
      Some(Calendar.getInstance().toInstant.toString),
      None,
      None,
      None,
      None,
      siteId,
      None,
      Some("User Created"),
      active = true
    )
  }

  def updateAlert(orgId: String, siteId: String): Alert = {
    if (alertId.getOrElse("").isEmpty || orgId.isEmpty || siteId.isEmpty) throw new AlertIdMissingException
    Alert(
      alertId.getOrElse(throw new AlertIdMissingException),
      name,
      msg,
      nodeId.getOrElse(""), //never update nodeId
      orgId,
      severity,
      `type`.getOrElse(""), //never update type
      category,
      Some(Calendar.getInstance().toInstant.toString),
      Some(Calendar.getInstance().toInstant.toString),
      None,
      None,
      None,
      None,
      siteId,
      None,
      Some("User Created"),
      active = true
    )
  }
}

case class NotificationProps(@JsonProperty("notificationid") notificationId: Option[String],
                             @JsonProperty name: Option[String] = None,
                             @JsonProperty description: Option[String] = None,
                             @JsonProperty active: Option[Boolean] = None,
                             @JsonProperty scope: Option[String] = None,
                             @JsonProperty("notificationtype") notificationType: Option[Seq[String]] = None,
                             @JsonProperty severity: Option[Seq[String]] = None,
                             @JsonProperty window: Option[String] = None,
                             @JsonProperty hold_off: Option[Int] = None,
                             @JsonProperty resend_interval: Option[Int] = None,
                             @JsonProperty msg: Option[String] = None,
                             @JsonProperty emailUsersList: Option[Seq[String]] = None,
                             @JsonProperty smsUsersList: Option[Seq[String]] = None,
                             @JsonProperty additionalEmails: Option[Seq[String]] = None,
                             @JsonProperty current: Option[Long] = None,
                             @JsonProperty updated: Option[Long] = None)
    extends Entity {

  def validateNotification(): Unit = {
    val severityList      = severity.getOrElse(List())
    val validSeverityList = Set("Minor", "Major", "Critical", "Clear")
    val nameList          = Set("DeviceAlarm", "Business Alert - Parking")
    if (!nameList.contains(name.getOrElse("")))
      throw new NotificationIdMissingException(
        s"Invalid Name: $name. Name should be either one of these: ${nameList.mkString(",")}"
      )

    if (severityList.nonEmpty && !severityList.forall(validSeverityList.contains))
      throw new NotificationIdMissingException(
        s"Invalid Severity: $severity. Severity should be either one of these: ${validSeverityList.mkString(",")}"
      )

    if (hold_off.getOrElse(0) < 0 || resend_interval.getOrElse(0) < 0)
      throw new NotificationIdMissingException(s"Hold Off & Resend Interval should be Positive Integers")
  }

  def toNotification(orgId: String, siteId: String): Notification = {
    if (notificationId.getOrElse("").isEmpty || orgId.isEmpty || siteId.isEmpty)
      throw new NotificationIdMissingException
    validateNotification
    Notification(
      notificationId.get,
      orgId,
      active.getOrElse(true),
      siteId,
      name,
      msg,
      description,
      scope,
      window,
      hold_off,
      resend_interval,
      notificationType.getOrElse(List()).toSet,
      severity.getOrElse(List()).toSet,
      emailUsersList.getOrElse(List()).toSet,
      additionalEmails.getOrElse(List()).toSet,
      smsUsersList.getOrElse(List()).toSet,
      Some(Instant.now.getEpochSecond),
      Some(Instant.now.getEpochSecond)
    )
  }
}

case class UfAlarmProps(@JsonProperty("mappingid") mappingId: Option[String],
                        @JsonProperty("alarmtype") alarmType: Option[String],
                        @JsonProperty("nodemodels") nodeModels: Option[Seq[String]] = None,
                        @JsonProperty("ufname") ufName: Option[String] = None,
                        @JsonProperty description: Option[String] = None,
                        @JsonProperty("displaytocustomer") displayToCustomer: Option[Boolean] = None,
                        @JsonProperty("displaytopartner") displayToPartner: Option[Boolean] = None,
                        @JsonProperty current: Option[Long] = None,
                        @JsonProperty updated: Option[Long] = None,
                        @JsonProperty("ufalarmslist") ufAlarmsList: Option[Seq[UfAlarm]] = None)
    extends Entity {

  def toUfAlarm: UfAlarm =
    UfAlarm(
      mappingId.getOrElse(throw new UfAlarmPropsMissingException),
      alarmType.getOrElse(throw new UfAlarmPropsMissingException),
      nodeModels.getOrElse(List()).toSet,
      Some(ufName.getOrElse(throw new UfAlarmPropsMissingException)),
      description,
      displayToCustomer.getOrElse(false),
      displayToPartner.getOrElse(false),
      Some(Instant.now.getEpochSecond),
      Some(Instant.now.getEpochSecond)
    )

  def updatedUfAlarm: UfAlarm =
    UfAlarm(
      mappingId.getOrElse(throw new UfAlarmPropsMissingException),
      alarmType.getOrElse(""), //Never update alarmType
      nodeModels.getOrElse(List()).toSet,
      Some(ufName.getOrElse(throw new UfAlarmPropsMissingException)),
      description,
      displayToCustomer.getOrElse(false),
      displayToPartner.getOrElse(false),
      Some(Instant.now.getEpochSecond),
      Some(Instant.now.getEpochSecond)
    )
}

case class AppRequest(messageid: String, responsetopic: String, @JsonProperty request: RequestBody)

case class RequestBody(requestid: String,
                       `type`: String,
                       model: String,
                       action: String,
                       @JsonProperty userprops: Option[UserProps] = None,
                       @JsonProperty alertprops: Option[AlertProps] = None,
                       @JsonProperty orgprops: Option[OrgProps] = None,
                       @JsonProperty siteprops: Option[SiteProps] = None,
                       @JsonProperty nodeprops: Option[NodeProps] = None,
                       @JsonProperty notificationprops: Option[NotificationProps] = None,
                       @JsonProperty ufalarmprops: Option[UfAlarmProps] = None,
                       transactionID: Option[String] = None,
                       instanceId: String,
                       timestamp: String)

sealed trait AppResponse {
  def future(): Future[AppResponse] = Future.successful(this)
}

case class AppSuccessResponse[T](messageid: String, @JsonProperty response: SuccessResponseBody[T])
    extends AppResponse {}

trait ResponseBody {
  @JsonProperty("requestid")
  val requestId: String
  @JsonProperty("timestamp")
  val timeStamp: String
  val success: Boolean
}

case class Status(success: Boolean)

case class BulkSuccess(success: Boolean, @JsonProperty("ufalarm") ufAlarm: UfAlarm)

case class SuccessResponseBody[T](private val _requestid: String,
                                  private val _timestamp: String,
                                  private val _success: Boolean,
                                  @JsonProperty result: T)
    extends ResponseBody {
  override val requestId: String = _requestid
  override val timeStamp: String = _timestamp
  override val success: Boolean  = _success
}

case class FailureResponseBody(private val _requestid: String,
                               private val _timestamp: String,
                               private val _success: Boolean,
                               error: String,
                               status: Int)
    extends ResponseBody {
  override val requestId: String = _requestid
  override val timeStamp: String = _timestamp
  override val success: Boolean  = _success
}

case class AppFailureResponse(messageid: String, response: FailureResponseBody) extends AppResponse {}

case class MessageId(messageid: String)

sealed case class CaselType(value: String)

object CaselType {

  object ADD_ALERT extends CaselType("createAlert")

  object DELETE_ALERT extends CaselType("deleteAlert")

  object DISMISS_ALERT extends CaselType("dismissAlert")

  object GET_ALERT_FOR_ALERT_ID extends CaselType("getAlert")

  object GET_ALERT_SYS extends CaselType("getAlertSys")

  object GET_ALERTS_FOR_NODE_ID extends CaselType("getAlertsForNode")

  object GET_ALERTS_FOR_ORG_ID_AND_SITE_ID extends CaselType("getAllAlerts")

  object UPDATE_ALERT extends CaselType("updateAlert")

  object NOTIFICATIONS_FOR_SITE_ID extends CaselType("getAllNotificationsForSite")

  object NOTIFICATION_FOR_ID extends CaselType("getNotification")

  object NOTIFICATION_FOR_NAME extends CaselType("getAllNotificationsByName")

  object NOTIFICATION_SYS extends CaselType("getNotificationSys")

  object DELETE_NOTIFICATION extends CaselType("deleteNotification")

  object ADD_NOTIFICATION extends CaselType("createNotification")

  object UPDATE_NOTIFICATION extends CaselType("updateNotification")

  object ACTIVATE_NOTIFICATION extends CaselType("activateNotification")

  object DEACTIVATE_NOTIFICATION extends CaselType("deactivateNotification")

  object GET_ALL_UF_ALARMS extends CaselType("getAllUFAlarms")

  object GET_UF_ALARM extends CaselType("getUFAlarm")

  object CREATE_UF_ALARM extends CaselType("createUFAlarm")

  object CREATE_BULK_UF_ALARMS extends CaselType("createBulkUFAlarms")

  object UPDATE_UF_ALARM extends CaselType("updateUFAlarm")

  object DELETE_UF_ALARM extends CaselType("deleteUFAlarm")

  object RESET_UF_ALARM extends CaselType("resetUFAlarm")

  object GET_NOTIFICATION_TYPES extends CaselType("getNotificationTypes")

  lazy val allRequestTypes = Set(
    ADD_ALERT.value,
    DELETE_ALERT.value,
    DISMISS_ALERT.value,
    GET_ALERT_FOR_ALERT_ID.value,
    GET_ALERT_SYS.value,
    GET_ALERTS_FOR_NODE_ID.value,
    GET_ALERTS_FOR_ORG_ID_AND_SITE_ID.value,
    UPDATE_ALERT.value,
    NOTIFICATIONS_FOR_SITE_ID.value,
    NOTIFICATION_FOR_ID.value,
    NOTIFICATION_FOR_NAME.value,
    NOTIFICATION_SYS.value,
    DELETE_NOTIFICATION.value,
    ADD_NOTIFICATION.value,
    UPDATE_NOTIFICATION.value,
    ACTIVATE_NOTIFICATION.value,
    DEACTIVATE_NOTIFICATION.value,
    GET_ALL_UF_ALARMS.value,
    GET_UF_ALARM.value,
    CREATE_UF_ALARM.value,
    CREATE_BULK_UF_ALARMS.value,
    UPDATE_UF_ALARM.value,
    DELETE_UF_ALARM.value,
    RESET_UF_ALARM.value,
    GET_NOTIFICATION_TYPES.value
  )

}

object StatusCodes extends Enumeration {

  type StatusCodes = Value

  val NOTFOUNDERROR       = Value(404)
  val BADREQUESTERROR     = Value(400)
  val INTERNALSERVERERROR = Value(500)

}
