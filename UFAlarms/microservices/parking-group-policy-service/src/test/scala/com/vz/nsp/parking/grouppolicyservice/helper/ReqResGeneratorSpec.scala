package com.vz.nsp.parking.grouppolicyservice.helper

import java.util.{Calendar, UUID}

import com.vz.nsp.parking.grouppolicyservice.BaseSpec
import com.vz.nsp.parking.grouppolicyservice.constants.TestDataFeed._
import com.vz.nsp.parking.model._

/**
  * Created by nagarna on 9/27/17.
  */

class ReqResGeneratorSpec extends BaseSpec {

  val messageId = UUID.randomUUID().toString
  val requestId = UUID.randomUUID().toString
  val policyId = UUID.randomUUID().toString

  val resultBody: Int = 1

  val appRequest = AppRequest(messageId, responseTopic,
    new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString, "associatedParkingGroups",
      "NoneModel", "CAN_READ", Some("user"), OrgProps(orgid), SiteProps(siteid),
      ConfigProps(Some(policyId), None, None, Some(parkinggroupid), None, None, None, None, None, None,
        None, None, None, None), AppUserDataProps(None, "", "", None)))

  private implicit val ec = scala.concurrent.ExecutionContext.Implicits.global


  "failurecaseResponse" should "Return Success Message" in {

    val output = ReqResGenerator.failureCaseResponse(appRequest, 500, "Error: failurecaseResponse!!!")

    assert(output.isInstanceOf[SuccessMessage].!=(true))

  }

  "generateAppFailureResponse" should "Return Failure Response" in {

    val output = ReqResGenerator.generateAppFailureResponse(appRequest, 500, "Error: generateAppFailureResponse!!!")

    assert(output.isInstanceOf[AppFailureResponse].!=(false))

  }

  "failurecaseResponseForIrrelevantRequestType" should "Return Success \"AppFailureResponse\"" in {

    val output = ReqResGenerator.failureCaseResponseForIrrelevantRequestType(500, "Error: failurecaseResponseForIrrelevantRequestType!!!")

    assert(output.isInstanceOf[SuccessMessage].!=(true))

  }

  "successResponse" should "Return Success \"AppSuccessResponse\"" in {

    val output = ReqResGenerator.successResponse(appRequest, resultBody)

    assert(output.isInstanceOf[SuccessMessage].!=(true))

  }

  "failurecaseResponseWithoutFuture" should "Return Success \"AppSuccessResponse\"" in {

    val output = ReqResGenerator.failureCaseResponseWithoutFuture(appRequest, 500, "Error: failurecaseResponseWithoutFuture!!!")

    assert(output.isInstanceOf[AppFailureResponse].!=(false))

  }

  "successResponseWithoutFuture" should "Return Success Response" in {

    val output = ReqResGenerator.successResponseWithoutFuture(appRequest, resultBody)

    assert(output.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId))

  }

}
