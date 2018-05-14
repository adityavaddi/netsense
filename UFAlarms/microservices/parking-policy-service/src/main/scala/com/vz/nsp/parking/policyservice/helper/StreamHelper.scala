package com.vz.nsp.parking.policyservice.helper

import com.verizon.netsense.utils.Logging
import com.vz.nsp.parking.model.CaselType._
import com.vz.nsp.parking.model.ResponseMessage._
import com.vz.nsp.parking.model.StatusCodes._
import com.vz.nsp.parking.model.{AppRequest, AppResponse}
import ReqResGenerator._
import com.verizon.netsense.metrics.Instrumented
import com.vz.nsp.parking.CommonHelper._
import nl.grons.metrics.scala.Timer

import scala.concurrent.{ExecutionContext, Future}

class StreamHelper(policyHelper: PolicyHelper) extends Logging with Instrumented{

  private[this] val ApiPostPolicyTimer: Timer = metrics.timer("api-create-policy")
  private[this] val ApiGetPolicyTimer: Timer = metrics.timer("api-get-policy-by-id")
  private[this] val ApiGetAllPoliciesTimer: Timer = metrics.timer("api-get-all-policies")
  private[this] val ApiDeletePolicyTimer: Timer = metrics.timer("api-delete-policy-by-id")
  private[this] val ApiUpdatePolicyTimer: Timer = metrics.timer("api-update-policy-by-id")
  private[this] val ApiGetPolicyVersionHistoryTimer: Timer = metrics.timer("api-get-policy-version-history-by-id")
  private[this] val ApiGetPolicyByVerionTimer: Timer = metrics.timer("api-get-policy-by-id-version")
  private[this] val ApiSearchPolicyTimer: Timer = metrics.timer("api-search-policy-by-name-tag")
  private[this] val ApiGetActivePolicyTimer: Timer = metrics.timer("api-get-active-policy")
  private[this] val ApiGetActivePoliciesTimer: Timer = metrics.timer("api-get-active-policies-within-timeline")
  private[this] val ApiAssociateTagPolicyTimer: Timer = metrics.timer("api-associate-tag-to-policy")
  private[this] val ApiDisassociateTagPoliciesTimer: Timer = metrics.timer("api-disassociate-tag-to-policy")
  private[this] val nullPointerExceptionTimer: Timer = metrics.timer("policy-nullpointer-exception")
  private[this] val exceptionTimer: Timer = metrics.timer("policy-exception")
  private[this] val siteOrgMissingTimer: Timer = metrics.timer("policy-casel-site-org-missing")
  private[this] val invalidJsonTimer: Timer = metrics.timer("policy-casel-invalid-json")
  private[this] val invalidTypeTimer: Timer = metrics.timer("policy-invalid-type")
  private[this] val validationFailedTimer: Timer = metrics.timer("policy-payload-validation-failure")

  implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  def processRequest(appRequest: AppRequest): Future[AppResponse] = {
    log.debug("###### appRequest " + appRequest)
    appRequest match {
      case AppRequest(null, _, _) =>
        log.error("### message id is null")
        invalidJsonTimer.time(ReqResGenerator.failurecaseResponseForIrrelevantRequestType(INTERNALSERVERERROR.id, ISR_WrongRequestJson.value))
      case AppRequest(_, null,_) =>
        log.error("### response topic  is null")
        invalidJsonTimer.time(ReqResGenerator.failurecaseResponseForIrrelevantRequestType(INTERNALSERVERERROR.id, ISR_WrongRequestJson.value))
      case AppRequest(_,_,null) =>
        log.error("### request object is null")
        invalidJsonTimer.time(ReqResGenerator.failurecaseResponseForIrrelevantRequestType(INTERNALSERVERERROR.id, ISR_WrongRequestJson.value))

      case _ => processJsonValidation(appRequest)
    }
  }

  def processJsonValidation(appRequest: AppRequest): Future[AppResponse] = {
    val requestType = appRequest.request.`type`
    if (isEmpty(requestType)) invalidTypeTimer.time(failurecaseResponse(appRequest, BADREQUEST.id, IrrelevantType.value))
    else if (requestType.equalsIgnoreCase(validationfailed.value))
      validationFailedTimer.time(failurecaseResponse(appRequest, BADREQUEST.id, appRequest.request.configprops.validationError.getOrElse("")))
    else validateAllRequiredFields(appRequest, requestType)
  }

  def validateAllRequiredFields(appRequest: AppRequest, requestType: String): Future[AppResponse] = {
    val orgprops = appRequest.request.orgprops
    val siteprops = appRequest.request.siteprops
    if (orgprops == null) siteOrgMissingTimer.time(failurecaseResponseForIrrelevantRequestType(INTERNALSERVERERROR.id, ISR_WrongRequestJson.value))
    else if (siteprops == null) siteOrgMissingTimer.time(failurecaseResponseForIrrelevantRequestType(INTERNALSERVERERROR.id, ISR_WrongRequestJson.value))
    else if (isEmpty(orgprops.orgid)) siteOrgMissingTimer.time(failurecaseResponseForIrrelevantRequestType(BADREQUEST.id, NotFound_Orgid.value))
    else if (isEmpty(siteprops.siteid)) siteOrgMissingTimer.time(failurecaseResponseForIrrelevantRequestType(BADREQUEST.id, NotFound_Siteid.value))
    else matchCaselTypeAndProcessRequest(appRequest, orgprops.orgid, siteprops.siteid, requestType)
  }

  def matchCaselTypeAndProcessRequest(appRequest: AppRequest, orgid: String, siteid: String, requestType: String): Future[AppResponse] =
    try {
      appRequest.request.`type` match {
        case postParkingPolicy.value               => ApiPostPolicyTimer.time(policyHelper.postParkingPolicyProcess(orgid, siteid,appRequest,productionApi.value))
        case getParkingPolicy.value                => ApiGetPolicyTimer.time(policyHelper.getParkingProcess(orgid, siteid,appRequest,productionApi.value))
        case getAllParkingPolicies.value           => ApiGetAllPoliciesTimer.time(policyHelper.getAllParkingProcess(orgid, siteid,appRequest,productionApi.value))
        case deleteParkingPolicy.value             => ApiDeletePolicyTimer.time(policyHelper.deletePolicyProcess(orgid, siteid,appRequest,productionApi.value))
        case updateParkingPolicy.value             => ApiUpdatePolicyTimer.time(policyHelper.updateParkingProcess(orgid, siteid,appRequest,productionApi.value))
        case policyTagsAssociation.value           => ApiAssociateTagPolicyTimer.time(policyHelper.updatePolicyTags(orgid, siteid,appRequest,productionApi.value))
        case policyTagsDisassociation.value        => ApiDisassociateTagPoliciesTimer.time(policyHelper.updatePolicyTags(orgid, siteid,appRequest,productionApi.value))
        case getPolicyVersionHistory.value         => ApiGetPolicyVersionHistoryTimer.time(policyHelper.getVersionHistoryProcess(orgid, siteid,appRequest))
        case getPolicyByVersionNumber.value        => ApiGetPolicyByVerionTimer.time(policyHelper.getPolicyByVersionNumberAndId(orgid, siteid,appRequest))
        case postToSearchPolicy.value              => ApiSearchPolicyTimer.time(policyHelper.searchPolicyByNameOrTagProcess(orgid, siteid,appRequest))
        case getActivePolicy.value                 => ApiGetActivePolicyTimer.time(policyHelper.getActivePolicyProcess(orgid, siteid,appRequest))
        case getActivePoliciesWithInTimeline.value => ApiGetActivePoliciesTimer.time(policyHelper.getActivePoliciesInTimeline(appRequest))
        case createWhatIfPolicy.value              => ApiPostPolicyTimer.time(policyHelper.postParkingPolicyProcess(orgid, siteid,appRequest,syntheticApi.value))
        case getWhatIfPolicy.value                 => ApiGetPolicyTimer.time(policyHelper.getWhatIfParkingProcess(orgid, siteid,appRequest,syntheticApi.value))
        case getAllWhatIfPolicies.value            => ApiGetAllPoliciesTimer.time(policyHelper.getAllWhatIfParkingProcess(orgid, siteid,appRequest,syntheticApi.value))
        case deleteWhatIfPolicy.value              => ApiDeletePolicyTimer.time(policyHelper.deleteWhatIfPolicyProcess(orgid, siteid,appRequest,syntheticApi.value))
        case updateWhatIfPolicy.value              => ApiUpdatePolicyTimer.time(policyHelper.updateWhatIfParkingProcess(orgid, siteid,appRequest,syntheticApi.value))
        case whatIfPolicyTagsAssociation.value     => ApiAssociateTagPolicyTimer.time(policyHelper.updatePolicyTags(orgid, siteid,appRequest,syntheticApi.value))
        case whatIfPolicyTagsDisassociation.value  => ApiDisassociateTagPoliciesTimer.time(policyHelper.updatePolicyTags(orgid, siteid,appRequest,syntheticApi.value))
        case _                                     => invalidTypeTimer.time(failurecaseResponse(appRequest, BADREQUEST.id, IrrelevantType.value))
      }
    } catch {
      case np: NullPointerException =>
        log.error("NullPointerException found for request: " + appRequest)
        log.error("Exception: " + np.getMessage)
        nullPointerExceptionTimer.time(failurecaseResponse(appRequest, INTERNALSERVERERROR.id, InternalServerError.value))
      case e: Exception =>
        log.error("Exception found in the queryFlow: " + e.getMessage)
        exceptionTimer.time(failurecaseResponse(appRequest, INTERNALSERVERERROR.id, InternalServerError.value))
    }

}

object StreamHelper {
  def apply(policyHelper: PolicyHelper): StreamHelper = new StreamHelper(policyHelper)
}
