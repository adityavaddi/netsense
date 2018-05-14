package com.verizon.netsense.services.alert.services

import com.verizon.netsense.services.alert.model.StatusCodes._
import com.verizon.netsense.services.alert.model._

/**
 * Created by vermri5 on 8/4/17.
 */
trait JsonUtils {

  type AppFailureRequest = (AppRequest, Int, String)

  type MakeResponse[OP]    = (OP, AppRequest) => AppResponse
  type LAZY_INPUT_EVAL[OP] = (MakeResponse[OP], OP)

  type CrudResponse = (String, AppResponse)

  val failureResponse: MakeResponse[String] = appFailureResponse(NOTFOUNDERROR.id, _, _)
  val errorResponse: MakeResponse[String]   = appFailureResponse(BADREQUESTERROR.id, _, _)

  def successResponse[T](): MakeResponse[T] = appSuccessResponse[T](_)(_)

  def appFailureResponse(status: Int, error: String, appRequest: AppRequest): AppFailureResponse =
    AppFailureResponse(
      appRequest.messageid,
      FailureResponseBody(appRequest.request.requestid, appRequest.request.timestamp, false, error, status)
    )

  def appSuccessResponse[T](resultBody: T)(implicit appRequest: AppRequest): AppSuccessResponse[T] =
    AppSuccessResponse(
      appRequest.messageid,
      SuccessResponseBody(appRequest.request.requestid, appRequest.request.timestamp, true, resultBody)
    )

}
