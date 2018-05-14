package com.verizon.netsense.businessalertservice.util

import java.time.Instant
import java.util.Calendar

import com.verizon.netsense.businessalertservice.model._

import scala.concurrent.{ExecutionContext, Future}

/**
 * Created by Rajitha on 15/02/18.
 */
class BARequestResponseGenerator {
  implicit val ec = ExecutionContext.Implicits.global
  def failureCaseResponse(tagGetByIdRequest: AppRequest, status: Int, error: String): Future[AppFailureResponse] =
    Future.successful(generateAppFailureResponse(tagGetByIdRequest, status, error))

  def generateAppFailureResponse(tagGetByIdRequest: AppRequest, status: Int, error: String): AppFailureResponse =
    AppFailureResponse(
      tagGetByIdRequest.messageid,
      FailureResponseBody(tagGetByIdRequest.request.requestid, Instant.now.toString, false, error, status)
    )

  def successResponse[T](headers: AppRequest, resultBody: T): Future[AppSuccessResponse[T]] =
    Future.successful(
      AppSuccessResponse[T](headers.messageid,
                            ResponseBody(headers.request.requestid, Instant.now.toString, true, resultBody))
    )

  def failureCaseResponseWithoutFuture(tagGetByIdRequest: AppRequest, status: Int, error: String): AppFailureResponse =
    generateAppFailureResponse(tagGetByIdRequest, status, error)

  def successResponseWithoutFuture[T](headers: AppRequest, resultBody: T): AppSuccessResponse[T] =
    AppSuccessResponse[T](headers.messageid,
                          ResponseBody(headers.request.requestid, Instant.now.toString, true, resultBody))

  def isEmpty(x: String): Boolean = x == null || x.trim.isEmpty

  def generateBusinessAlertResponse(businessAlert: BusinessAlert): BusinessAlertResponse =
    BusinessAlertResponse(
      businessAlert.orgId,
      businessAlert.siteId,
      businessAlert.triggerId.getOrElse(""),
      businessAlert.resourceId.getOrElse(""),
      businessAlert.businessAlertId,
      businessAlert.triggerName.getOrElse(""),
      businessAlert.triggerCategory.getOrElse(""),
      businessAlert.triggerSubcategory.getOrElse(""),
      businessAlert.resourceName.getOrElse(""),
      businessAlert.active.getOrElse(false),
      convertEpochToDate(businessAlert.createdOn.getOrElse(0l)),
      businessAlert.message.getOrElse(""),
      businessAlert.severity.getOrElse(""),
      convertEpochToDate(businessAlert.lastClearedAt.getOrElse(0l)),
      businessAlert.lastClearedBy.getOrElse(""),
      convertEpochToDate(businessAlert.lastUpdated.getOrElse(0l)),
      businessAlert.triggerUserId.getOrElse("")
    )

  def generateBusinessAlertSysResponse(businessAlert: BusinessAlert): BusinessAlertSysResponse =
    BusinessAlertSysResponse(
      businessAlert.orgId,
      businessAlert.siteId,
      businessAlert.triggerId.getOrElse(""),
      businessAlert.resourceId.getOrElse(""),
      businessAlert.businessAlertId,
      businessAlert.triggerName.getOrElse(""),
      businessAlert.triggerCategory.getOrElse(""),
      businessAlert.triggerSubcategory.getOrElse(""),
      businessAlert.resourceName.getOrElse(""),
      businessAlert.active.getOrElse(false),
      convertEpochToDate(businessAlert.createdOn.getOrElse(0l)),
      businessAlert.message.getOrElse(""),
      businessAlert.severity.getOrElse(""),
      convertEpochToDate(businessAlert.lastClearedAt.getOrElse(0l)),
      businessAlert.lastClearedBy.getOrElse(""),
      convertEpochToDate(businessAlert.lastUpdated.getOrElse(0l)),
      businessAlert.triggerUserId.getOrElse("")
    )

  def generateBusinessAlertHistory(businessAlert: BusinessAlert,
                                   severity: String,
                                   userId: String,
                                   currentTime: Long): BusinessAlertHistory =
    BusinessAlertHistory(
      businessAlert.orgId,
      businessAlert.siteId,
      businessAlert.triggerId,
      businessAlert.resourceId,
      Some(businessAlert.businessAlertId),
      businessAlert.triggerName,
      businessAlert.triggerCategory,
      businessAlert.triggerSubcategory,
      businessAlert.resourceName,
      businessAlert.active,
      currentTime,
      businessAlert.message,
      Some(severity),
      Some(currentTime),
      Some(userId),
      businessAlert.triggerUserId
    )

  def convertEpochToDate(epoch: Long): String =
    if (epoch == 0) "" else Instant.ofEpochMilli(epoch).toString

}

object BARequestResponseGenerator {
  def apply(): BARequestResponseGenerator = new BARequestResponseGenerator()
}
