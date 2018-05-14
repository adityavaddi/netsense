package com.verizon.netsense.businessalertservice.data

import java.util.{Calendar, UUID}

import com.verizon.netsense.businessalertservice.model._
import com.verizon.netsense.businessalertservice.util.ObjectMapperUtil

/**
 * Created by jittara on 02/28/18.
 */
object TestDataFeed {

  val name                   = "name"
  val messageId              = UUID.randomUUID().toString
  val instanceId             = UUID.randomUUID().toString
  val requestId              = UUID.randomUUID().toString
  val siteId                 = UUID.randomUUID().toString
  val orgId                  = UUID.randomUUID().toString
  val userId                 = UUID.randomUUID().toString
  val triggerId              = UUID.randomUUID().toString
  val resourceId             = UUID.randomUUID().toString
  val businessAlertId        = UUID.randomUUID().toString
  val businessAlertId2       = UUID.randomUUID().toString
  val triggerName            = "triggerName"
  val triggerCategory        = "triggerCategory"
  val triggerSubcategory     = "triggerName"
  val resourceName           = "triggerName"
  val active                 = true
  val severity               = "Minor"
  val triggerUserId          = userId
  val timestamp              = Calendar.getInstance().toInstant.toString
  val lastClearedAt          = timeInMillis
  val lastClearedBy          = userId
  val lastUpdated            = timeInMillis
  val message                = "Minor alert"
  val createdOn              = timeInMillis
  val timeInMilliSecondsLong = System.currentTimeMillis()
  val limit                  = 120
  val `type`                 = "getTag"
  val model                  = "NodeModel"
  val action                 = "CAN_READ"
  val user                   = "d48dcd46-da6e-49b0-b76e-e3cbc79c4565"
  val date1                  = "2017-06-03T22:54:46.670Z"
  val date2                  = "2099-06-19T22:55:16.737Z"
  val period                 = "15min"
  val timeInMillis           = Calendar.getInstance().getTimeInMillis
  val orgProps               = OrgProps(orgId)
  val siteProps              = SiteProps(siteId)

  def businessAlertProps = BusinessAlertProps(Some(businessAlertId), Some("active eq true"))

  val requestBody = RequestBody(
    instanceId,
    requestId,
    timestamp,
    `type`,
    model,
    action,
    Some(userId),
    orgProps,
    siteProps,
    Some(businessAlertProps)
  )

  def businessAlertPropsSearch(search: String = "active eq true") =
    BusinessAlertProps(Some(businessAlertId), Some(search))

  def requestBodyWithType(apiType: String) = RequestBody(
    instanceId,
    requestId,
    timestamp,
    `apiType`,
    model,
    action,
    Some(userId),
    orgProps,
    siteProps,
    Some(businessAlertProps)
  )

  def appRequestSearch(responseTopic: String) = AppRequest(messageId, messageId, requestBody)

  def generateAppRequest(responseTopic: String, reqtype: String, businessAlertId: String = ""): AppRequest =
    AppRequest(messageId, responseTopic, requestBodyWithType(reqtype))

  def generateAppRequestSearch(responseTopic: String, reqtype: String, search: String = ""): AppRequest =
    AppRequest(
      messageId,
      responseTopic,
      RequestBody(
        instanceId,
        requestId,
        timestamp,
        `reqtype`,
        model,
        action,
        Some(userId),
        orgProps,
        siteProps,
        Some(BusinessAlertProps(Some(businessAlertId), Some(search)))
      )
    )

  def generateAppRequestForSys(businessAlertId: String,
                               responseTopic: String = "responsetopic",
                               userid: String = "root"): AppRequest =
    AppRequest(
      messageId,
      responseTopic,
      RequestBody(
        instanceId,
        requestId,
        timestamp,
        "getBusinessAlertSys",
        model,
        action,
        Some(userid),
        orgProps,
        siteProps,
        Some(BusinessAlertProps(Some(businessAlertId), None))
      )
    )

  def appRequest =
    AppRequest(messageId, messageId, requestBody)

  def businessAlert(reqBusinessAlertId: String = businessAlertId,
                    severityVal: String,
                    reqcreatedOn: Long = timeInMillis) = BusinessAlert(
    orgId,
    siteId,
    Some(triggerId),
    Some(resourceId),
    reqBusinessAlertId,
    Some(triggerName),
    Some(triggerCategory),
    Some(triggerSubcategory),
    Some(resourceName),
    Some(active),
    Some(reqcreatedOn),
    Some(message),
    Some(severityVal),
    Some(lastClearedAt),
    Some(lastClearedBy),
    Some(lastUpdated),
    Some(triggerUserId)
  )

  def businessAlert(alertId: String, triggerId: String, resourceId: String) = BusinessAlert(
    orgId,
    siteId,
    Some(triggerId),
    Some(resourceId),
    alertId,
    Some(triggerName),
    Some(triggerCategory),
    Some(triggerSubcategory),
    Some(resourceName),
    Some(active),
    Some(timeInMillis),
    Some(message),
    Some(severity),
    Some(lastClearedAt),
    Some(lastClearedBy),
    Some(lastUpdated),
    Some(triggerUserId)
  )

  def businessAlertWithNull(alertId: String, triggerId: String, resourceId: String) = BusinessAlert(
    orgId = orgId,
    siteId = siteId,
    triggerId = Some(triggerId),
    resourceId = Some(resourceId),
    businessAlertId = alertId,
    triggerName = None,
    triggerCategory = None,
    triggerSubcategory = None,
    resourceName = None,
    active = None,
    createdOn = None,
    message = None,
    severity = None,
    lastClearedAt = None,
    lastClearedBy = None,
    lastUpdated = None,
    triggerUserId = None
  )

  def businessAlertHistory(alertId: String, severity: String) = BusinessAlertHistory(
    orgId,
    siteId,
    Some(triggerId),
    Some(resourceId),
    Some(alertId),
    Some(triggerName),
    Some(triggerCategory),
    Some(triggerSubcategory),
    Some(resourceName),
    Some(active),
    timeInMillis,
    Some(message),
    Some(severity),
    Some(lastClearedAt),
    Some(lastClearedBy),
    Some(triggerUserId)
  )

  def buildTrigger(triggerId: String = triggerId, resourceId: List[String]) =
    DBTrigger(
      triggerid = triggerId,
      orgid = orgId,
      siteid = siteId,
      resourcelistjson = ObjectMapperUtil.toJson {
        val r = resourceId.map(TriggerResource(_, resourceName))
        r
      },
      isdeleted = false
    )

}
