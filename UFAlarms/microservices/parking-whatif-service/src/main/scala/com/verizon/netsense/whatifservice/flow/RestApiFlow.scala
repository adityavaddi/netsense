package com.verizon.netsense.whatifservice.flow

import akka.stream.StreamTcpException
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import com.verizon.netsense.whatifservice.model._
import com.verizon.netsense.whatifservice.model.casel.{AppRequest, AppResponse}
import com.verizon.netsense.whatifservice.service.WhatIfService
import nl.grons.metrics.scala.Timer
import com.verizon.netsense.whatifservice.model.StatusCodes._
import com.verizon.netsense.whatifservice.model.WhatIfConstants._
import ResponseMessage.IrrelevantType
import com.verizon.netsense.whatifservice.util.RequestResponseGenerator

import scala.util.Try
import scala.concurrent.{ExecutionContext, Future}

class RestApiFlow(whatIfHelper: WhatIfService, reqResGenerator: RequestResponseGenerator)
    extends Logging
    with Instrumented {

  private[this] val ApiSearchWhatIfTimer: Timer = metrics.timer("api-search-what-If")
  private[this] val ApiGetWhatIfTimer: Timer    = metrics.timer("api-get-what-if-by-id")
  private[this] val ApiGetAllWhatIfTimer: Timer = metrics.timer("api-all-what-if")
  private[this] val ApiStoreWhatIfTimer: Timer  = metrics.timer("api-store-all-what-if")
  private[this] val ApiUpdateWhatIfTimer: Timer = metrics.timer("api-update-all-what-if")
  private[this] val ApiDeleteWhatIfTimer: Timer = metrics.timer("api-delete-all-what-if")
  private[this] val ApiAbortWhatIfTimer: Timer  = metrics.timer("api-abort-all-what-if")

  private[this] val nullPointerExceptionTimer: Timer = metrics.timer("what-if-nullpointer-exception")
  private[this] val exceptionTimer: Timer            = metrics.timer("what-if-exception")
  private[this] val invalidTypeTimer: Timer          = metrics.timer("what-if-invalid-type")

  implicit val ec = ExecutionContext.Implicits.global

  def processRequest(appRequest: AppRequest): Future[(AppResponse, WhatIfSparkRequest)] = {
    val orgId  = appRequest.request.orgprops.orgid
    val siteId = appRequest.request.siteprops.siteid
    log.debug("appRequest " + appRequest)
    Try {
      appRequest.request.`type` match {
        case createWhatIfJob.value =>
          ApiStoreWhatIfTimer.time(
            whatIfHelper.postJobStatusProcess(orgId, siteId, appRequest)
          )
        case getAllWhatIfJobs.value =>
          ApiGetAllWhatIfTimer.time(
            whatIfHelper.getAllWhatIfProcess(orgId, siteId, appRequest)
          )
        case getWhatIfJob.value =>
          ApiGetWhatIfTimer.time(
            whatIfHelper.getWhatIfByIdProcess(orgId, siteId, appRequest)
          )
        case searchWhatIfJob.value =>
          ApiSearchWhatIfTimer.time(
            whatIfHelper.searchWhatIf(orgId, siteId, appRequest)
          )
        case updateWhatIfJob.value =>
          ApiUpdateWhatIfTimer.time(
            whatIfHelper.updateWhatIfProcess(orgId, siteId, appRequest)
          )
        case deleteWhatIfJob.value =>
          ApiDeleteWhatIfTimer.time(
            whatIfHelper.deleteWhatIfProcess(orgId, siteId, appRequest)
          )
        case abortWhatIfJob.value =>
          ApiAbortWhatIfTimer.time(
            whatIfHelper.abortWhatIfProcess(orgId, siteId, appRequest)
          )
        case _ =>
          invalidTypeTimer.time(reqResGenerator.failureCaseResponse(appRequest, NOTFOUND.id, IrrelevantType.value))
      }
    }.toEither match {
      case Left(l) =>
        log.error("Error:" + l.getMessage, l)
        reqResGenerator.failureCaseResponse(appRequest, INTERNALSERVERERROR.id, l.getMessage)
      case Right(r) =>
        r.recoverWith {
          case ex: StreamTcpException =>
            log.error("Error:" + ex.getMessage, ex)
            reqResGenerator.failureCaseResponse(appRequest,
                                                INTERNALSERVERERROR.id,
                                                "Something wrong with spark job URL")
          case ex: Exception =>
            log.error("Error:" + ex.getMessage, ex)
            reqResGenerator.failureCaseResponse(appRequest, INTERNALSERVERERROR.id, ex.getMessage)
        }
    }
  }
}

object RestApiFlow {
  def apply(tagHelper: WhatIfService, reqResGenerator: RequestResponseGenerator): RestApiFlow =
    new RestApiFlow(tagHelper, reqResGenerator)
}
