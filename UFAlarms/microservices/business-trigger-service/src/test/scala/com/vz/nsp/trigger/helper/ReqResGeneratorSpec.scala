package com.vz.nsp.trigger.helper

import java.util.{Calendar, UUID}

import com.vz.nsp.trigger.model.app.{TriggerResource, TriggerRequest}
import com.vz.nsp.trigger.model.casel._
import com.vz.nsp.trigger.model.db.DBTrigger
import com.vz.nsp.trigger.util.BaseSpec
import com.vz.nsp.triggerservice.helper.ReqResGenerator

/**
  * Created by maleva on 2/28/18.
  */
class ReqResGeneratorSpec extends BaseSpec {


  val reqResGenerator= spy(new ReqResGenerator())

  private implicit val ec = scala.concurrent.ExecutionContext.Implicits.global



  val messageId = UUID.randomUUID().toString
  val resTopic: String = "responseTopic"
  val instanceId = UUID.randomUUID().toString
  val requestId = UUID.randomUUID().toString
  val siteId = UUID.randomUUID().toString
  val orgId = UUID.randomUUID().toString

  val resultBody: Int = 1

  val resourceListProps = List(TriggerResource("resourceId", "resourceName"))

  val triggerReq = TriggerRequest("triggerName", "triggerCategory", "triggerSubcategory", resourceListProps,
    "timePeriod", "triggerVariable", Some("statisticalFunction"), Some("comparisonVariable"),
    "comparisonOperator", "comparedTo", "userMessage", "severity")

  val triggerProps = TriggerProps(Some("triggerId"), Some(triggerReq),None)

  val dbTrigger = DBTrigger("triggerId", "triggername", "userid", 2018, 2018, "triggercategory",
    "triggersubcategory", "resourcelistjson", siteId, orgId, "timePeriod", "triggervariable",
    "statisticalfunction", "comparisonvariable", "comparisonoperator", "comparedto",
    "usermessage", "additionalUserMessage", "severity", true)

  val appRequestTrigger = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
    "deleteTriggerCategory", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId), Some(triggerProps)))





  "failurecaseResponse" should "Return Success Message" in {

    val output= reqResGenerator.failurecaseResponse(appRequestTrigger,500,"Error: failurecaseResponse!!!")

    assert(output.isInstanceOf[SuccessMessage].!=(true))
  }


  "generateAppFailureResponse" should "Return Failure Response" in {

    val output = reqResGenerator.generateAppFailureResponse(appRequestTrigger, 500, "Error: generateAppFailureResponse!!!")

    assert(output.isInstanceOf[AppFailureResponse].!=(false))
    assert(output.isInstanceOf[AppFailureResponse].== (true))

  }


  "failurecaseResponseForIrrelevantRequestType" should "Return Success \"AppFailureResponse\"" in {

    val output = reqResGenerator.failurecaseResponseForIrrelevantRequestType(500, "Error: failurecaseResponseForIrrelevantRequestType!!!")

    assert(output.isInstanceOf[SuccessMessage].!=(true))

  }

  "successResponse" should "Return Success \"AppSuccessResponse\"" in {

    val output = reqResGenerator.successResponse(appRequestTrigger, resultBody)

    assert(output.isInstanceOf[SuccessMessage].!=(true))

  }

  "failurecaseResponseWithoutFuture" should "Return Success \"AppSuccessResponse\"" in {

    val output = reqResGenerator.failurecaseResponseWithoutFuture(appRequestTrigger, 500, "Error: failurecaseResponseWithoutFuture!!!")

    assert(output.isInstanceOf[AppFailureResponse].!=(false))

  }

  "successResponseWithoutFuture" should "Return Success Response" in {

    val output = reqResGenerator.successResponseWithoutFuture(appRequestTrigger, resultBody)

    assert(output.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId))

  }


  "generateTriggerObject" should "Return Trigger Object" in {

    val uid = UUID.randomUUID().toString
//    val triggerRequest = TriggerRequest("name",resourceListProps,"time",Some(),Some("TriggerId"),
//      Some(appRequestTrigger))

   // val tagOutput = reqResGenerator.generateTriggerObject(triggerRequest, orgId, siteId)

   // assert(tagOutput.asInstanceOf[DBTrigger].siteid.equals(siteId))

  }




  }
