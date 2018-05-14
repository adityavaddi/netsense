package com.vz.nsp.trigger.data




import java.util.{Calendar, UUID}

import com.vz.nsp.trigger.model.app.{TriggerResource, TriggerRequest}
import com.vz.nsp.trigger.model.casel._
import com.vz.nsp.trigger.model.db.DBTrigger


/**
  * Created by maleva on 2/20/18.
  */
trait TestData {

  lazy val triggerSampleEvent: DBTrigger = DBTrigger("test1","12342123","456767",2019,2019,"test","test","test",siteId,orgId,"t44est","tes44t","t44est","t4","r4","v4","g4","o4","test",false)
  lazy val triggerSampleEvent1: DBTrigger = DBTrigger("test2","12342123","456767",2019,2019,"tes654t","t44est","44test",siteId,orgId,"t44est","tes44t","t44est","t4","r4","v4","g4","o4","test",false)
  lazy  val messageId = UUID.randomUUID().toString
  lazy val resTopic: String = "responseTopic"
  lazy val instanceId = UUID.randomUUID().toString
  lazy val requestId = UUID.randomUUID().toString
  lazy val siteId = "test"
  lazy val orgId ="test"
  lazy val triggerId= "test1"



  val resourceListProps = List(TriggerResource("resourceId", "resourceName"))

  val triggerReq = TriggerRequest("triggerName", "triggerCategory", "triggerSubcategory", resourceListProps,
    "timePeriod", "triggerVariable", Some("statisticalFunction"), Some("comparisonVariable"),
    "comparisonOperator", "comparisionvalue", "userMessage", "severity")

  val triggerProps = TriggerProps(Some("triggerId"), Some(triggerReq),None)

  val appRequestTriggerByID = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
    "getTrigger", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
    Some(triggerProps)))

  val appRequestAllTriggers = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
    "getAllTriggers", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
    Some(triggerProps)))

  val appRequestTrigger1 = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
    "getAllTriggers", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
    Some(triggerProps)))

  val createTriggerReq = TriggerRequest(
    triggerName =  "average,occupancy,15,>,80",
    triggerCategory = "Business Alert - Parking",
    triggerSubcategory = "group",
    resourceList = resourceListProps,
    timePeriod = "15min",
    triggerVariable = "TotalViolationCount",
    comparisonVariableOperator = Some("Percentage"),
    comparisonVariable = Some("Turnover"),
    comparisonOperator = "GT",
    comparisonValue  = "60.0",
    userMessage = "The {{trigger.triggerVariable}} percentage of the parking group {{trigger.resourceName}}  over time period {{trigger.timePeriod}} is {{trigger.comparisonOperator}} {{trigger.comparisonValue}}",
    severity = "Major",
    additionalUserMessage = None
  )
  val createTriggerProps = TriggerProps(None, Some(createTriggerReq), None)

  val appCreateTrigger = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
    "createTrigger", "TriggerModel", "CAN_CREATE", Some("user"), OrgProps(orgId), SiteProps(siteId),
    Some(createTriggerProps)))

  val userid=appRequestTriggerByID.request.user.getOrElse("")


  val dbTrigger = DBTrigger("triggerId", "triggername", "userid", 2018, 2018, "triggercategory",
    "triggersubcategory", "[{\"resourceId\":\"resourceId\",\"resourceName\":\"resourceName\"}]", siteId, orgId, "timePeriod", "triggervariable",
    "statisticalfunction", "comparisonvariable", "comparisonoperator", "comparisionvalue",
    "usermessage","additionalUserMessage", "severity", false)

  val dbTrigger2= DBTrigger("triggerId1", "triggername", "userid", 2018, 2018, "triggercategory",
    "triggersubcategory", "[{\"resourceId\":\"resourceId\",\"resourceName\":\"resourceName\"}]", siteId, orgId, "timePeriod", "triggervariable",
    "statisticalfunction", "comparisonvariable", "comparisonoperator", "comparisionvalue",
    "usermessage","additionalUserMessage", "severity", false)


  def BusinessTrigger(reqBusinessTriggertId: String) = DBTrigger(
    "triggerId", "triggername", "userid", 2018, 2018, "triggercategory",
    "triggersubcategory", "resourcelistjson", siteId, orgId, "timePeriod", "triggervariable",
    "statisticalfunction", "comparisonvariable", "comparisonoperator", "comparisionvalue",
    "usermessage","additionalUserMessage", "severity", false
  )


}
