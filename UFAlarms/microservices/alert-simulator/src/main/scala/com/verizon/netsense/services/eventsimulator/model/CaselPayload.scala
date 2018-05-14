package com.verizon.netsense.services.eventsimulator.model

import java.time.Instant
import java.util.Calendar

import com.fasterxml.jackson.annotation.JsonProperty
import com.verizon.netsense.entity.Entity

import scala.concurrent.Future

/**
 * Created by vermri5 on 8/2/17.
 */
case class NodeProps(@JsonProperty("nodeid") nodeId: String)

case class SiteProps(@JsonProperty("siteid") siteId: String)

case class OrgProps(@JsonProperty("orgid") orgId: String)

case class AlertProps(@JsonProperty("alertid") alertId: Option[String],
                      @JsonProperty name: Option[String],
                      @JsonProperty `type`: Option[String],
                      @JsonProperty("nodeid") nodeId: Option[String] = None,
                      @JsonProperty msg: Option[String] = None,
                      @JsonProperty severity: Option[String] = None,
                      @JsonProperty category: Option[String] = None) {

  def toAlert(orgId: String, siteId: String): Alert =
    Alert(
      alertId.getOrElse(""),
      name.getOrElse("DeviceAlarm"),
      msg.getOrElse(""),
      nodeId.getOrElse(""),
      orgId,
      severity.getOrElse(""),
      `type`.getOrElse(""),
      category.getOrElse(""),
      Calendar.getInstance().toInstant.toString,
      Calendar.getInstance().toInstant.toString,
      None,
      None,
      None,
      None,
      siteId,
      None,
      None,
      true
    )
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
                             @JsonProperty updated: Option[Long] = None) {

  def toNotification(orgId: String, siteId: Option[String]): Notification =
    Notification(
      notificationId.getOrElse(""),
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
      Option(Instant.now.getEpochSecond),
      Option(Instant.now.getEpochSecond)
    )
}

case class AppRequest(messageid: String, responsetopic: String, @JsonProperty request: RequestBody) extends Entity

case class RequestBody(requestid: String,
                       `type`: String,
                       model: String,
                       action: String,
                       @JsonProperty alertprops: Option[AlertProps] = None,
                       @JsonProperty orgprops: Option[OrgProps] = None,
                       @JsonProperty siteprops: Option[SiteProps] = None,
                       @JsonProperty nodeprops: Option[NodeProps] = None,
                       @JsonProperty notificationprops: Option[NotificationProps] = None,
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

  object GET_ALERTS_FOR_ORG_ID extends CaselType("getAllOrgAlerts")

  object GET_ALERTS_FOR_NODE_ID extends CaselType("getAlertsForNode")

  object GET_ALERTS_FOR_ORG_ID_AND_SITE_ID extends CaselType("getAllAlerts")

  object UPDATE_ALERT extends CaselType("updateAlert")

  object NOTIFICATIONS_FOR_SITE_ID extends CaselType("getAllNotificationsForSite")

  object NOTIFICATIONS_FOR_ORG_ID extends CaselType("getAllNotificationsForOrg")

  object NOTIFICATION_FOR_ID extends CaselType("getNotification")

  object NOTIFICATION_FOR_NAME extends CaselType("getAllNotificationsByName")

  object NOTIFICATION_SYS extends CaselType("getNotificationSys")

  object DELETE_NOTIFICATION extends CaselType("deleteNotification")

  object ADD_NOTIFICATION extends CaselType("createNotification")

  object UPDATE_NOTIFICATION extends CaselType("updateNotification")

  object ACTIVATE_NOTIFICATION extends CaselType("activateNotification")

  object DEACTIVATE_NOTIFICATION extends CaselType("deactivateNotification")

  lazy val allRequestTypes = Set(
    ADD_ALERT.value,
    DELETE_ALERT.value,
    DISMISS_ALERT.value,
    GET_ALERT_FOR_ALERT_ID.value,
    GET_ALERT_SYS.value,
    GET_ALERTS_FOR_ORG_ID.value,
    GET_ALERTS_FOR_NODE_ID.value,
    GET_ALERTS_FOR_ORG_ID_AND_SITE_ID.value,
    UPDATE_ALERT.value,
    NOTIFICATIONS_FOR_SITE_ID.value,
    NOTIFICATIONS_FOR_ORG_ID.value,
    NOTIFICATION_FOR_ID.value,
    NOTIFICATION_FOR_NAME.value,
    NOTIFICATION_SYS.value,
    DELETE_NOTIFICATION.value,
    ADD_NOTIFICATION.value,
    UPDATE_NOTIFICATION.value,
    ACTIVATE_NOTIFICATION.value,
    DEACTIVATE_NOTIFICATION.value
  )

}

object StatusCodes extends Enumeration {

  type StatusCodes = Value

  val BADREQUEST          = Value(404)
  val INTERNALSERVERERROR = Value(500)

}
