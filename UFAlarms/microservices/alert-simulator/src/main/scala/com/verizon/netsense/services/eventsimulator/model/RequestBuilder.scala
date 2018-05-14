package com.verizon.netsense.services.eventsimulator.model

import java.util.UUID

import com.verizon.netsense.services.eventsimulator.model.CaselType._

import scala.util.Random

abstract class RequestBuilder {

  protected val instanceId    = ""
  protected var responseTopic = ""
  protected val requestId     = ""
  protected val timeStamp     = ""
  protected val model         = ""
  protected val action        = ""

  protected var orgId                         = ""
  protected var siteId                        = ""
  protected var reqType                       = ""
  protected var nodeId                        = ""
  protected var user: Option[String]          = None
  protected var transactionId: Option[String] = None

  var notificationProps = NotificationProps(None)
  var alertProps        = AlertProps(None, None, None)

  def withTransactionId(transactionId: Option[String]): RequestBuilder = {
    this.transactionId = transactionId
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

  def withUser(user: Option[String]): RequestBuilder = {
    this.user = user
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
      Some(alertProps),
      Option(OrgProps(orgId)),
      Option(SiteProps(siteId)),
      Option(NodeProps(nodeId)),
      Option(notificationProps),
      transactionId,
      instanceId,
      timeStamp
    )
    val appReq = AppRequest(Random.nextInt.toString, responseTopic, requestBody)
    appReq.toJSON
  }
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

object NotificationRequestBuilder {
  def apply(): NotificationRequestBuilder = new NotificationRequestBuilder()
}
