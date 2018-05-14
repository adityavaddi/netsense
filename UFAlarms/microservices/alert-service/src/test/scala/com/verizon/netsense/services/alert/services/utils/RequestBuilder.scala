package com.verizon.netsense.services.alert.services.utils

import java.util.UUID

import com.verizon.netsense.services.alert.model.CaselType._
import com.verizon.netsense.services.alert.model._
import com.verizon.netsense.services.alert.util.ObjectMapperUtil

import scala.util.Random

abstract class RequestBuilder {

  protected val instanceId                    = ""
  protected var responseTopic                 = ""
  protected val requestId                     = ""
  protected val timeStamp                     = ""
  protected val model                         = ""
  protected val action                        = ""
  protected var orgId                         = ""
  protected var siteId                        = ""
  protected var reqType                       = ""
  protected var nodeId                        = ""
  protected var user: Option[String]          = None
  protected var transactionId: Option[String] = None

  var notificationProps = NotificationProps(None)
  var alertProps        = AlertProps(None, None, None)
  var ufAlarmProps      = UfAlarmProps(None, None)

  def withTransactionId(transactionId: Option[String]): RequestBuilder = {
    this.transactionId = transactionId
    this
  }

  def withUserType(user: Option[String]): RequestBuilder = {
    this.user = user
    this
  }

  def withOrgId(orgId: String): RequestBuilder = {
    this.orgId = orgId
    this
  }

  def withNodeId(nodeId: String): RequestBuilder = {
    this.nodeId = nodeId
    this
  }

  def withSiteId(siteId: String): RequestBuilder = {
    this.siteId = siteId
    this
  }

  def withReqType(reqType: String): RequestBuilder = {
    this.reqType = reqType
    this
  }

  def withResponseTopic(responseTopic: String): RequestBuilder = {
    this.responseTopic = responseTopic
    this
  }

  def build: String = {
    val requestBody = RequestBody(
      requestId,
      reqType,
      model,
      action,
      Option(UserProps(user)),
      Option(alertProps),
      Option(OrgProps(orgId)),
      Option(SiteProps(siteId)),
      Option(NodeProps(nodeId)),
      Option(notificationProps),
      Option(ufAlarmProps),
      transactionId,
      instanceId,
      timeStamp
    )
    val appReq = AppRequest(Random.nextInt.toString, responseTopic, requestBody)
    ObjectMapperUtil.toJson(appReq)
  }
}

object NotificationRequestBuilder {
  def apply(): NotificationRequestBuilder = new NotificationRequestBuilder()
}

class NotificationRequestBuilder extends RequestBuilder {

  override val model = "NotificationModel"
  reqType = NOTIFICATION_FOR_ID.value
  override val requestId = UUID.randomUUID().toString

  def withNotificationProps(n: NotificationProps): NotificationRequestBuilder = {
    notificationProps = n
    this
  }

}

object AlertRequestBuilder {
  def apply(): AlertRequestBuilder = new AlertRequestBuilder()
}

class AlertRequestBuilder extends RequestBuilder {

  override val model = "AlertModel"
  reqType = GET_ALERT_FOR_ALERT_ID.value
  override val requestId = UUID.randomUUID().toString

  def withAlertProps(a: AlertProps): AlertRequestBuilder = {
    alertProps = a
    this
  }

}

object ufAlarmsRequestBuilder {
  def apply(): ufAlarmsRequestBuilder = new ufAlarmsRequestBuilder()
}

class ufAlarmsRequestBuilder extends RequestBuilder {

  override val model = "UFAlarmModel"
  reqType = GET_ALL_UF_ALARMS.value
  override val requestId = UUID.randomUUID().toString

  def withUfAlarmProps(u: UfAlarmProps): ufAlarmsRequestBuilder = {
    ufAlarmProps = u
    this
  }

}
