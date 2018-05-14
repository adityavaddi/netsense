package com.vz.nsp.trigger.service

import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import com.vz.nsp.trigger.model.casel.{AppRequest, AppResponse}
import com.vz.nsp.trigger.model.casel.CaselType._
import com.vz.nsp.trigger.model.casel.ResponseMessage._
import com.vz.nsp.trigger.model.casel.StatusCodes._
import com.vz.nsp.trigger.helper.CommonHelper.isEmpty
import com.vz.nsp.triggerservice.helper.ReqResGenerator
import nl.grons.metrics.scala.Timer

import scala.concurrent.{ExecutionContext, Future}

class StreamService(triggerHelper: TriggerProcessor, reqResGenerator: ReqResGenerator) extends Logging with Instrumented {

  private[this] val ApiPosttriggerTimer: Timer = metrics.timer("api-create-trigger")
  private[this] val ApiUpdatetriggerTimer: Timer = metrics.timer("api-update-trigger-by-id")
  private[this] val ApiDeletetriggerTimer: Timer = metrics.timer("api-delete-trigger-by-id")
  private[this] val ApiGettriggerTimer: Timer = metrics.timer("api-get-trigger-by-id")
  private[this] val ApiGetAlltriggersTimer: Timer = metrics.timer("api-get-all-triggers")
  private[this] val ApiSearchTriggerTimer: Timer = metrics.timer("api-search-trigger")
  private[this] val nullPointerExceptionTimer: Timer = metrics.timer("tag-nullpointer-exception")
  private[this] val exceptionTimer: Timer = metrics.timer("tag-exception")
  private[this] val siteOrgMissingTimer: Timer = metrics.timer("tag-casel-site-org-missing")
  private[this] val invalidJsonTimer: Timer = metrics.timer("tag-casel-invalid-json")
  private[this] val invalidTypeTimer: Timer = metrics.timer("tag-invalid-type")
  private[this] val validationFailedTimer: Timer = metrics.timer("tag-payload-validation-failure")


  implicit val ec = ExecutionContext.Implicits.global

  def processRequest(appRequest: AppRequest): Future[AppResponse] = {
    log.debug("###### appRequest " + appRequest)
    appRequest match {
      case AppRequest(null, _, _) => {
        log.error("### message id is null")
        invalidJsonTimer.time(reqResGenerator.failurecaseResponseForIrrelevantRequestType(INTERNALSERVERERROR.id, ISR_WrongRequestJson.value))
      }
      case AppRequest(_, null, _) => {
        log.error("### response topic  is null")
        invalidJsonTimer.time(reqResGenerator.failurecaseResponseForIrrelevantRequestType(INTERNALSERVERERROR.id, ISR_WrongRequestJson.value))
      }
      case AppRequest(_, _, null) => {
        log.error("### request object is null")
        invalidJsonTimer.time(reqResGenerator.failurecaseResponseForIrrelevantRequestType(INTERNALSERVERERROR.id, ISR_WrongRequestJson.value))
      }
        case _ => processJsonValidation(appRequest)

    }
  }

  def processJsonValidation(appRequest: AppRequest): Future[AppResponse] = {
    val requestType = appRequest.request.`type`
    if (isEmpty(requestType)) invalidTypeTimer.time(reqResGenerator.failurecaseResponse(appRequest, NOTFOUND.id, IrrelevantType.value))
    else validateAllRequiredFields(appRequest, requestType)
  }

  def validateAllRequiredFields(appRequest: AppRequest, requestType: String): Future[AppResponse] = {
    val orgprops = appRequest.request.orgprops
    val siteprops = appRequest.request.siteprops
    if (orgprops == null) siteOrgMissingTimer.time(reqResGenerator.failurecaseResponseForIrrelevantRequestType(INTERNALSERVERERROR.id, ISR_WrongRequestJson.value))
    else if (siteprops == null) siteOrgMissingTimer.time(reqResGenerator.failurecaseResponseForIrrelevantRequestType(INTERNALSERVERERROR.id, ISR_WrongRequestJson.value))
    else if (isEmpty(orgprops.orgid)) siteOrgMissingTimer.time(reqResGenerator.failurecaseResponseForIrrelevantRequestType(NOTFOUND.id, NotFound_Orgid.value))
    else if (isEmpty(siteprops.siteid)) siteOrgMissingTimer.time(reqResGenerator.failurecaseResponseForIrrelevantRequestType(NOTFOUND.id, NotFound_Siteid.value))
    else matchCaselTypeAndProcessRequest(appRequest, orgprops.orgid, siteprops.siteid, requestType)
  }

  def matchCaselTypeAndProcessRequest(appRequest: AppRequest, orgid: String, siteid: String, requestType: String): Future[AppResponse] =
    try {
      requestType match {
        case getTrigger.value => ApiGettriggerTimer.time(triggerHelper.getTriggerProcess(orgid, siteid, appRequest))
        case getAllTriggers.value => ApiGetAlltriggersTimer.time(triggerHelper.getAllTriggersProcess(orgid, siteid, appRequest))
        case postTrigger.value => ApiPosttriggerTimer.time(triggerHelper.postTriggerProcess(orgid, siteid, appRequest))
        case deleteTrigger.value => ApiDeletetriggerTimer.time(triggerHelper.deleteTriggerProcess(orgid, siteid, appRequest))
        case updateTrigger.value => ApiUpdatetriggerTimer.time(triggerHelper.updateTriggerProcess(orgid, siteid, appRequest))
        case filterTrigger.value=> ApiSearchTriggerTimer.time(triggerHelper.searchTrigger(orgid,siteid,appRequest))
        case _ => invalidTypeTimer.time(reqResGenerator.failurecaseResponse(appRequest, NOTFOUND.id, IrrelevantType.value))
      }
    } catch {
      case np: NullPointerException => {
        log.error("NullPointerException found for request: " + appRequest)
        log.error("Exception: " + np.getMessage)
        nullPointerExceptionTimer.time(reqResGenerator.failurecaseResponse(appRequest, INTERNALSERVERERROR.id, InternalServerError.value))
      }
      case e: Exception => {
        log.error("Exception found in the queryFlow: " + e.getMessage)
        exceptionTimer.time(reqResGenerator.failurecaseResponse(appRequest, INTERNALSERVERERROR.id, InternalServerError.value))
      }
    }

}

object StreamService {
  def apply(triggerHelper: TriggerProcessor, reqResGenerator: ReqResGenerator): StreamService = new StreamService(triggerHelper, reqResGenerator)
}
