package com.verizon.netsense.businessalertservice.util

import com.verizon.netsense.businessalertservice.data.TestDataFeed._
import com.verizon.netsense.businessalertservice.model._

/**
 * Created by jittara on 02/28/18.
 */
class BAReqResGeneratorSpec extends BaseSpec {

  val reqResGenerator = spy(new BARequestResponseGenerator())

  private implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  "failureCaseResponse" should "Return Success Message" in {

    val output = reqResGenerator.failureCaseResponse(appRequest, 500, "Error: failureCaseResponse!!!")

    assert(output.isInstanceOf[SuccessMessage].!=(true))

  }

  "generateAppFailureResponse" should "Return Failure Response" in {

    val output = reqResGenerator.generateAppFailureResponse(appRequest, 500, "Error: generateAppFailureResponse!!!")

    assert(output.isInstanceOf[AppFailureResponse].!=(false))

  }

  "successResponse" should "Return Success \"AppSuccessResponse\"" in {

    val output = reqResGenerator.successResponse(appRequest, "1")

    assert(output.isInstanceOf[SuccessMessage].!=(true))

  }

  "failurecaseResponseWithoutFuture" should "Return Success \"AppSuccessResponse\"" in {

    val output =
      reqResGenerator.failureCaseResponseWithoutFuture(appRequest, 500, "Error: failurecaseResponseWithoutFuture!!!")

    assert(output.isInstanceOf[AppFailureResponse].!=(false))

  }

  "successResponseWithoutFuture" should "Return Success Response" in {

    val output = reqResGenerator.successResponseWithoutFuture(appRequest, "1")

    assert(output.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId))

  }

  "BAReqResGeneratorSpec" should "generate Business Alert History object" in {

    val businessAlertHistory =
      reqResGenerator.generateBusinessAlertHistory(businessAlert(businessAlertId, "Minor"), "Minor", userId, 110L)

    assert(businessAlertHistory.asInstanceOf[BusinessAlertHistory].siteId.equals(siteId))

  }

  "BAReqResGeneratorSpec" should "generate Business Alert response object" in {

    val tagOutput = reqResGenerator.generateBusinessAlertResponse(businessAlert(businessAlertId, "Minor"))

    assert(tagOutput.asInstanceOf[BusinessAlertResponse].businessAlertId.equals(businessAlertId))

  }

}
