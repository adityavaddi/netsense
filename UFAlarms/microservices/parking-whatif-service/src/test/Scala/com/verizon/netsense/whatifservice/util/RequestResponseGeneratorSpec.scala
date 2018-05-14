package com.verizon.netsense.whatifservice.util

import com.verizon.netsense.whatifservice.model.{SuccessMessage, WhatIfJobResponse, WhatIfResponse}
import com.verizon.netsense.whatifservice.data.TestDataFeed._
import com.verizon.netsense.whatifservice.model.casel.{AppFailureResponse, AppRequest, AppSuccessResponse}

/**
  * Created by maleva on 4/21/18.
  */
class RequestResponseGeneratorSpec extends BaseSpec {


  val reqResGenerator = spy(new RequestResponseGenerator())

  private implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  "failureCaseResponse" should "Return Success Message" in {

    val output = reqResGenerator.failureCaseResponse(appRequest("getAllWhatIfJobs"), 500, "Error: failureCaseResponse!!!")

    assert(output.isInstanceOf[SuccessMessage].!=(true))
  }

  "generateAppFailureResponse" should "Return Failure Response" in {

    val output = reqResGenerator.generateAppFailureResponse(appRequest("getAllWhatIfJobs"), 500, "Error: generateAppFailureResponse!!!")

    assert(output.isInstanceOf[AppFailureResponse].!=(false))

  }

  "successResponse" should "Return Success \"AppSuccessResponse\"" in {

    val output = reqResGenerator.successResponse(appRequest("getAllWhatIfJobs"), "1")

    assert(output.isInstanceOf[SuccessMessage].!=(true))

  }

  "failurecaseResponseWithoutFuture" should "Return Success \"AppSuccessResponse\"" in {

    val output =
      reqResGenerator.failureCaseResponseWithoutFuture(appRequest("getAllWhatIfJobs"), 500, "Error: failurecaseResponseWithoutFuture!!!")
    assert(output._1.isInstanceOf[AppFailureResponse].!=(false))

  }

  "successResponseWithoutFuture" should "Return Success Response" in {

    val output = reqResGenerator.successResponseWithoutFuture(appRequest("getAllWhatIfJobs"), "1")

    assert(output._1.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId))
  }

  "ReqResGeneratorSpec" should "generate What If response object" in {

    val output = WhatIfResponse(whatIf)

    assert(output.asInstanceOf[WhatIfJobResponse].jobId.equals(jobId))
  }
}