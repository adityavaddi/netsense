package com.verizon.netsense.whatifservice.util

import java.time.Instant
import java.util.UUID

import com.verizon.netsense.whatifservice.model.{WhatIfJob, WhatIfJobRequest, WhatIfJobResponse, WhatIfSparkRequest}
import com.verizon.netsense.whatifservice.model.casel._
import com.verizon.netsense.whatifservice.model.WhatIfConstants.EntirePeriod

import scala.concurrent.{ExecutionContext, Future}

/**
 * Created by Rajitha on 15/02/18.
 */
class RequestResponseGenerator {
  implicit val ec = ExecutionContext.Implicits.global

  def failureCaseResponse(tagGetByIdRequest: AppRequest,
                          status: Int,
                          error: String): Future[(AppFailureResponse, WhatIfSparkRequest)] =
    Future.successful((generateAppFailureResponse(tagGetByIdRequest, status, error), WhatIfSparkRequest()))

  def generateAppFailureResponse(tagGetByIdRequest: AppRequest, status: Int, error: String): AppFailureResponse =
    AppFailureResponse(
      tagGetByIdRequest.messageid,
      FailureResponseBody(tagGetByIdRequest.request.requestid, Instant.now.toString, false, error, status)
    )

  def successResponse[T](
      headers: AppRequest,
      resultBody: T,
      whatIfSparkRequest: WhatIfSparkRequest = WhatIfSparkRequest()
  ): Future[(AppSuccessResponse[T], WhatIfSparkRequest)] =
    Future.successful {
      (AppSuccessResponse[T](headers.messageid,
                             ResponseBody(headers.request.requestid, Instant.now.toString, true, resultBody)),
       whatIfSparkRequest)
    }

  def failureCaseResponseWithoutFuture(tagGetByIdRequest: AppRequest,
                                       status: Int,
                                       error: String): (AppFailureResponse, WhatIfSparkRequest) =
    (generateAppFailureResponse(tagGetByIdRequest, status, error), WhatIfSparkRequest())

  def successResponseWithoutFuture[T](
      headers: AppRequest,
      resultBody: T,
      whatIfSparkRequest: WhatIfSparkRequest = WhatIfSparkRequest()
  ): (AppSuccessResponse[T], WhatIfSparkRequest) =
    (AppSuccessResponse[T](headers.messageid,
                           ResponseBody(headers.request.requestid, Instant.now.toString, true, resultBody)),
     whatIfSparkRequest)

  def sparkRequest(appRequest: AppRequest, whatIfJobResponse: WhatIfJobResponse)(
      implicit ec: ExecutionContext
  ): SparkRequest =
    SparkRequest(
      appRequest.messageid,
      appRequest.responsetopic,
      SparkRequestBody(
        appRequest.request.instanceid,
        appRequest.request.requestid,
        appRequest.request.timestamp,
        appRequest.request.`type`,
        appRequest.request.model,
        appRequest.request.action,
        appRequest.request.user,
        appRequest.request.orgprops,
        appRequest.request.siteprops,
        whatIfJobResponse
      )
    )

  def getPolicyListWithOutVersion(policyListWithVersion: List[String]): List[String] =
    policyListWithVersion
      .map(eachPolicyWithVersion => eachPolicyWithVersion.split(":").toList)
      .map(policy => policy.head)
}

object RequestResponseGenerator {
  def apply(): RequestResponseGenerator = new RequestResponseGenerator()
}
