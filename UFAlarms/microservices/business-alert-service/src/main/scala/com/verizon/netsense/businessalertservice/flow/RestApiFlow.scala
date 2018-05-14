package com.verizon.netsense.businessalertservice.flow

import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import com.verizon.netsense.businessalertservice.model.BusinessAlertConstants._
import com.verizon.netsense.businessalertservice.model.ResponseMessage._
import com.verizon.netsense.businessalertservice.model.StatusCodes._
import com.verizon.netsense.businessalertservice.model.{AppRequest, AppResponse}
import com.verizon.netsense.businessalertservice.service.BusinessAlertService
import com.verizon.netsense.businessalertservice.util.BARequestResponseGenerator
import nl.grons.metrics.scala.Timer

import scala.concurrent.{ExecutionContext, Future}
class RestApiFlow(businessAlertHelper: BusinessAlertService, reqResGenerator: BARequestResponseGenerator)
    extends Logging
    with Instrumented {

  private[this] val ApiSearchBusinessAlertTimer: Timer  = metrics.timer("api-search-business-alert")
  private[this] val ApiDismissBusinessAlertTimer: Timer = metrics.timer("api-dismiss-business-alert-by-id")
  private[this] val ApiGetBusinessAlertTimer: Timer     = metrics.timer("api-get-business-alert-by-id")
  private[this] val ApiGetBusinessAlertSysTimer: Timer  = metrics.timer("api-get-business-alert-sys")
  private[this] val ApiGetAllBusinessAlertsTimer: Timer = metrics.timer("api-all-business-alerts")
  private[this] val nullPointerExceptionTimer: Timer    = metrics.timer("business-alert-nullpointer-exception")
  private[this] val exceptionTimer: Timer               = metrics.timer("business-alert-exception")
  private[this] val invalidTypeTimer: Timer             = metrics.timer("business-alert-invalid-type")

  implicit val ec = ExecutionContext.Implicits.global

  def processRequest(appRequest: AppRequest): Future[AppResponse] = {
    val orgId  = appRequest.request.orgprops.orgid
    val siteId = appRequest.request.siteprops.siteid
    log.debug("###### appRequest " + appRequest)
    try {
      appRequest.request.`type` match {
        case getBusinessAlert.value =>
          ApiGetBusinessAlertTimer.time(
            businessAlertHelper.getBusinessAlertByIdProcess(orgId, siteId, appRequest)
          )
        case getAllBusinessAlerts.value =>
          ApiGetAllBusinessAlertsTimer.time(
            businessAlertHelper.getAllBusinessAlertsProcess(orgId, siteId, appRequest)
          )
        case dismissBusinessAlert.value =>
          ApiDismissBusinessAlertTimer.time(
            businessAlertHelper.dismissBusinessAlertProcess(orgId, siteId, appRequest)
          )
        case filterBusinessAlert.value =>
          ApiSearchBusinessAlertTimer.time(
            businessAlertHelper
              .searchBusinessAlert(orgId, siteId, appRequest)
          )
        case getBusinessAlertSys.value =>
          ApiSearchBusinessAlertTimer.time(
            businessAlertHelper
              .getBusinessAlertSys(orgId, siteId, appRequest)
          )
        case _ =>
          invalidTypeTimer.time(reqResGenerator.failureCaseResponse(appRequest, NOTFOUND.id, IrrelevantType.value))
      }
    } catch {
      case np: NullPointerException => {
        log.error("NullPointerException found for request: " + appRequest)
        log.error("Exception: " + np.getMessage)
        nullPointerExceptionTimer.time(
          reqResGenerator.failureCaseResponse(appRequest, INTERNALSERVERERROR.id, InternalServerError.value)
        )
      }
      case e: Exception => {
        log.error("Exception found in the queryFlow: " + e.getMessage)
        exceptionTimer.time(
          reqResGenerator.failureCaseResponse(appRequest, INTERNALSERVERERROR.id, InternalServerError.value)
        )
      }
    }

  }

}

object RestApiFlow {
  def apply(tagHelper: BusinessAlertService, reqResGenerator: BARequestResponseGenerator): RestApiFlow =
    new RestApiFlow(tagHelper, reqResGenerator)
}
