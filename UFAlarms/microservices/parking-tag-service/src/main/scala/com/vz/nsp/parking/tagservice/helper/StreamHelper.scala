package com.vz.nsp.parking.tagservice.helper

import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import com.vz.nsp.parking.CommonHelper._
import com.vz.nsp.parking.model.CaselType._
import com.vz.nsp.parking.model.ResponseMessage._
import com.vz.nsp.parking.model.StatusCodes._
import com.vz.nsp.parking.model.{AppRequest, AppResponse}
import nl.grons.metrics.scala.Timer

import scala.concurrent.{ExecutionContext, Future}

class StreamHelper(tagHelper: TagHelper, reqResGenerator: ReqResGenerator) extends Logging with Instrumented {

  private[this] val ApiPostTagTimer: Timer = metrics.timer("api-create-tag")
  private[this] val ApiUpdateTagTimer: Timer = metrics.timer("api-update-tag-by-id")
  private[this] val ApiDeleteTagTimer: Timer = metrics.timer("api-delete-tag-by-id")
  private[this] val ApiGetTagTimer: Timer = metrics.timer("api-get-tag-by-id")
  private[this] val ApiGetAllTagsTimer: Timer = metrics.timer("api-get-all-tags")
  private[this] val nullPointerExceptionTimer: Timer = metrics.timer("tag-nullpointer-exception")
  private[this] val exceptionTimer: Timer = metrics.timer("tag-exception")
  private[this] val siteOrgMissingTimer: Timer = metrics.timer("tag-casel-site-org-missing")
  private[this] val invalidJsonTimer: Timer = metrics.timer("tag-casel-invalid-json")
  private[this] val invalidTypeTimer: Timer = metrics.timer("tag-invalid-type")
  private[this] val validationFailedTimer: Timer = metrics.timer("tag-payload-validation-failure")


  implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  def processRequest(appRequest: AppRequest): Future[AppResponse] = {
    log.debug("###### appRequest " + appRequest)
    appRequest match {
      case AppRequest(null, _, _) =>
        log.error("### message id is null")
        invalidJsonTimer.time(reqResGenerator.failurecaseResponseForIrrelevantRequestType(INTERNALSERVERERROR.id, ISR_WrongRequestJson.value))

      case AppRequest(_, null, _) =>
        log.error("### response topic  is null")
        invalidJsonTimer.time(reqResGenerator.failurecaseResponseForIrrelevantRequestType(INTERNALSERVERERROR.id, ISR_WrongRequestJson.value))

      case AppRequest(_, _, null) =>
        log.error("### request object is null")
        invalidJsonTimer.time(reqResGenerator.failurecaseResponseForIrrelevantRequestType(INTERNALSERVERERROR.id, ISR_WrongRequestJson.value))

      case _ => processJsonValidation(appRequest)
    }
  }

  def processJsonValidation(appRequest: AppRequest): Future[AppResponse] = {
    val requestType = appRequest.request.`type`
    if (isEmpty(requestType)) invalidTypeTimer.time(reqResGenerator.failurecaseResponse(appRequest, BADREQUEST.id, IrrelevantType.value))
    else if (requestType.equalsIgnoreCase(validationfailed.value))
      validationFailedTimer.time(reqResGenerator.failurecaseResponse(appRequest, BADREQUEST.id, appRequest.request.configprops.validationError.getOrElse("")))
    else validateAllRequiredFields(appRequest, requestType)
  }

  def validateAllRequiredFields(appRequest: AppRequest, requestType: String)(implicit ec: ExecutionContext): Future[AppResponse] = {
    val orgprops = appRequest.request.orgprops
    val siteprops = appRequest.request.siteprops
    if (orgprops == null) siteOrgMissingTimer.time(reqResGenerator.failurecaseResponseForIrrelevantRequestType(INTERNALSERVERERROR.id, ISR_WrongRequestJson.value))
    else if (siteprops == null) siteOrgMissingTimer.time(reqResGenerator.failurecaseResponseForIrrelevantRequestType(INTERNALSERVERERROR.id, ISR_WrongRequestJson.value))
    else if (isEmpty(orgprops.orgid)) siteOrgMissingTimer.time(reqResGenerator.failurecaseResponseForIrrelevantRequestType(BADREQUEST.id, NotFound_Orgid.value))
    else if (isEmpty(siteprops.siteid)) siteOrgMissingTimer.time(reqResGenerator.failurecaseResponseForIrrelevantRequestType(BADREQUEST.id, NotFound_Siteid.value))
    else matchCaselTypeAndProcessRequest(appRequest, orgprops.orgid, siteprops.siteid, requestType)
  }

  def matchCaselTypeAndProcessRequest(appRequest: AppRequest, orgid: String, siteid: String, requestType: String): Future[AppResponse] =
    try {
      requestType match {
        case getParkingTag.value => ApiGetTagTimer.time(tagHelper.getParkingTagProcess(orgid, siteid, productionApi.value, appRequest))
        case getAllParkingTags.value => ApiGetAllTagsTimer.time(tagHelper.getAllParkingTagProcess(orgid, siteid, productionApi.value,appRequest))
        case postParkingTag.value => ApiPostTagTimer.time(tagHelper.postParkingTagProcess(orgid, siteid,productionApi.value, appRequest))
        case deleteParkingTag.value => ApiDeleteTagTimer.time(tagHelper.deleteParkingTagProcess(orgid, siteid,productionApi.value, appRequest))
        case updateParkingTag.value => ApiUpdateTagTimer.time(tagHelper.updateParkingTagProcess(orgid, siteid,productionApi.value, appRequest))
        case getWhatIfTag.value => ApiGetTagTimer.time(tagHelper.getParkingTagProcess(orgid, siteid, syntheticApi.value, appRequest))
        case getAllWhatIfTags.value => ApiGetAllTagsTimer.time(tagHelper.getAllParkingTagProcess(orgid, siteid, syntheticApi.value, appRequest))
        case createWhatIfTag.value => ApiPostTagTimer.time(tagHelper.postParkingTagProcess(orgid, siteid,syntheticApi.value, appRequest))
        case deleteWhatIfTag.value => ApiDeleteTagTimer.time(tagHelper.deleteParkingTagProcess(orgid, siteid,syntheticApi.value, appRequest))
        case updateWhatIfTag.value => ApiUpdateTagTimer.time(tagHelper.updateParkingTagProcess(orgid, siteid,syntheticApi.value, appRequest))
        case _ => invalidTypeTimer.time(reqResGenerator.failurecaseResponse(appRequest, BADREQUEST.id, IrrelevantType.value))
      }
    } catch {
      case np: NullPointerException =>
        log.error("NullPointerException found for request: " + appRequest)
        log.error("Exception: " + np.getMessage)
        nullPointerExceptionTimer.time(reqResGenerator.failurecaseResponse(appRequest, INTERNALSERVERERROR.id, InternalServerError.value))
      case e: Exception =>
        log.error("Exception found in the queryFlow: " + e.getMessage)
        exceptionTimer.time(reqResGenerator.failurecaseResponse(appRequest, INTERNALSERVERERROR.id, InternalServerError.value))
    }

}

object StreamHelper {
  def apply(tagHelper: TagHelper, reqResGenerator: ReqResGenerator): StreamHelper = new StreamHelper(tagHelper, reqResGenerator)
}
