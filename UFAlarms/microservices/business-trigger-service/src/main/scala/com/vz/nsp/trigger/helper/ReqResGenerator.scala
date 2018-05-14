package com.vz.nsp.triggerservice.helper

import java.time.Instant
import java.util.{Calendar, UUID}

import com.fasterxml.jackson.annotation.JsonProperty
import com.vz.nsp.trigger.model.casel._
import com.vz.nsp.trigger.model.app._
import com.vz.nsp.trigger.model.db.DBTrigger

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by maleva on 2/9/18.
  */
class ReqResGenerator {

  def failurecaseResponse(triggerGetByIdRequest: AppRequest, status: Int, error: String)(
    implicit ec: ExecutionContext
  ): Future[AppFailureResponse] =
    Future.successful(generateAppFailureResponse(triggerGetByIdRequest, status, error))

  def generateAppFailureResponse(triggerGetByIdRequest: AppRequest, status: Int, error: String)(
    implicit ec: ExecutionContext
  ): AppFailureResponse =
    AppFailureResponse(
      triggerGetByIdRequest.messageid,
      FailureResponseBody(triggerGetByIdRequest.request.requestid, Instant.now.toString, false, error, status)
    )

  def failurecaseResponseForIrrelevantRequestType(status: Int, error: String)(
    implicit ec: ExecutionContext
  ): Future[AppFailureResponse] =
    Future.successful(AppFailureResponse("", FailureResponseBody("", Instant.now.toString, false, error, status)))

  def successResponse[T](headers: AppRequest,
                         resultBody: T)(implicit ec: ExecutionContext): Future[AppSuccessResponse[T]] =
    Future.successful(
      AppSuccessResponse[T](headers.messageid,
        ResponseBody(headers.request.requestid, Instant.now.toString, true, resultBody))
    )

  def failurecaseResponseWithoutFuture(triggerGetByIdRequest: AppRequest, status: Int, error: String)(
    implicit ec: ExecutionContext
  ): AppFailureResponse =
    generateAppFailureResponse(triggerGetByIdRequest, status, error)

  def successResponseWithoutFuture[T](headers: AppRequest,
                                      resultBody: T)(implicit ec: ExecutionContext): AppSuccessResponse[T] =
    AppSuccessResponse[T](headers.messageid,
      ResponseBody(headers.request.requestid, Instant.now.toString, true, resultBody))

}

object ReqResGenerator {
  def apply(): ReqResGenerator = new ReqResGenerator()
}
