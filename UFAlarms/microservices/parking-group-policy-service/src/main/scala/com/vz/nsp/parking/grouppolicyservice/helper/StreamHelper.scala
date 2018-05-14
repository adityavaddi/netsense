package com.vz.nsp.parking.grouppolicyservice.helper

import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import com.vz.nsp.parking.CommonHelper.isEmpty
import com.vz.nsp.parking.grouppolicyservice.helper.ReqResGenerator._
import com.vz.nsp.parking.grouppolicyservice.service.{GroupPolicyService, ParkingSpaceService}
import com.vz.nsp.parking.model.CaselType.{getAssociatedParkingGroups, _}
import com.vz.nsp.parking.model.ResponseMessage._
import com.vz.nsp.parking.model.StatusCodes._
import com.vz.nsp.parking.model.{AppRequest, AppResponse}
import nl.grons.metrics.scala.Timer

import scala.concurrent.{ExecutionContext, Future}

class StreamHelper(groupPolicyHelper: GroupPolicyService, parkingSpotHelper: ParkingSpaceService) extends Logging  with Instrumented{

  private[this] val ApiAssociatePolicyToGroupTimer: Timer = metrics.timer("api-associate-policy-to-parkinggroup")
  private[this] val ApiDisAssociatePolicyToGroupTimer: Timer = metrics.timer("api-disassociate-policy-from-parkinggroup")
  private[this] val ApiGetAssociatedGroupToPolicyTimer: Timer = metrics.timer("api-get-associated-parkinggroups-to-policy")
  private[this] val ApiUpdateParkingspotTimer: Timer = metrics.timer("api-update-parkingspot-spotattributes-by-id")
  private[this] val ApiDeleteParkingspotTimer: Timer = metrics.timer("api-delete-parkingspot-spotattributes-by-id")
  private[this] val ApiGetAllParkingspotsTimer: Timer = metrics.timer("api-get-all-parkingspot-spotattributes")
  private[this] val nullPointerExceptionTimer: Timer = metrics.timer("group-policy-nullpointer-exception")
  private[this] val exceptionTimer: Timer = metrics.timer("group-policy-exception")
  private[this] val siteOrgMissingTimer: Timer = metrics.timer("group-policy-casel-site-org-missing")
  private[this] val invalidJsonTimer: Timer = metrics.timer("group-policy-casel-invalid-json")
  private[this] val invalidTypeTimer: Timer = metrics.timer("group-policy-invalid-type")
  private[this] val validationFailedTimer: Timer = metrics.timer("group-policy-payload-validation-failure")

  implicit val ec = ExecutionContext.Implicits.global
  def processRequest(appRequest: AppRequest): Future[AppResponse] =
    appRequest match {
      case AppRequest(null, _, _) => {
        log.error("### message id is null")
        invalidJsonTimer.time(ReqResGenerator.failureCaseResponseForIrrelevantRequestType(INTERNALSERVERERROR.id, ISR_WrongRequestJson.value))
      }
      case AppRequest(_, null, _) => {
        log.error("### responsetopic is null")
        invalidJsonTimer.time(ReqResGenerator.failureCaseResponseForIrrelevantRequestType(INTERNALSERVERERROR.id, ISR_WrongRequestJson.value))
      }
      case AppRequest(_, _, null) => {
        log.error("### request object is null")
        invalidJsonTimer.time(ReqResGenerator.failureCaseResponseForIrrelevantRequestType(INTERNALSERVERERROR.id, ISR_WrongRequestJson.value))
      }

      case _ => processJsonValidation(appRequest)
    }

  def processJsonValidation(appRequest: AppRequest): Future[AppResponse] = {
    val requestType = appRequest.request.`type`
    if (isEmpty(requestType)) invalidTypeTimer.time(failureCaseResponse(appRequest, BADREQUEST.id, IrrelevantType.value))
    else if (requestType.equalsIgnoreCase(validationfailed.value))
      validationFailedTimer.time(failureCaseResponse(appRequest, BADREQUEST.id, appRequest.request.configprops.validationError.getOrElse("")))
    else validateAllRequiredFields(appRequest, requestType)
  }

  def validateAllRequiredFields(appRequest: AppRequest, requestType: String): Future[AppResponse] = {
    val orgprops = appRequest.request.orgprops
    val siteprops = appRequest.request.siteprops
    if (orgprops == null) siteOrgMissingTimer.time(failureCaseResponseForIrrelevantRequestType(INTERNALSERVERERROR.id, ISR_WrongRequestJson.value))
    else if (siteprops == null) siteOrgMissingTimer.time(failureCaseResponseForIrrelevantRequestType(INTERNALSERVERERROR.id, ISR_WrongRequestJson.value))
    else if (isEmpty(orgprops.orgid)) siteOrgMissingTimer.time(failureCaseResponseForIrrelevantRequestType(BADREQUEST.id, NotFound_Orgid.value))
    else if (isEmpty(siteprops.siteid)) siteOrgMissingTimer.time(failureCaseResponseForIrrelevantRequestType(BADREQUEST.id, NotFound_Siteid.value))
    else matchCaselTypeAndProcessRequest(appRequest, orgprops.orgid, siteprops.siteid, requestType)
  }
  def matchCaselTypeAndProcessRequest(appRequest: AppRequest, orgid: String, siteid: String, requestType: String): Future[AppResponse] =
    try {
      appRequest.request.`type` match {
        case policyassociation.value => ApiAssociatePolicyToGroupTimer.time(groupPolicyHelper.groupPolicyAssociationProcess(orgid, siteid, appRequest))
        case policydisassociation.value => ApiDisAssociatePolicyToGroupTimer.time(groupPolicyHelper.groupPolicyDisassociationProcess(orgid, siteid, appRequest))
        case getAssociatedParkingGroups.value => ApiGetAssociatedGroupToPolicyTimer.time(groupPolicyHelper.getAssociatedParkingGroupsOfActivePolicy(orgid, siteid, appRequest))
        case getAllParkingSpaces.value => ApiGetAllParkingspotsTimer.time(parkingSpotHelper.getAllParkingSpacesProcess(appRequest))
        case updateParkingSpace.value => ApiUpdateParkingspotTimer.time(parkingSpotHelper.updateParkingSpaceProcess(appRequest))
        case deleteParkingSpace.value => ApiDeleteParkingspotTimer.time(parkingSpotHelper.deleteParkingSpaceProcess(appRequest))
        case _ => invalidTypeTimer.time(failureCaseResponse(appRequest, BADREQUEST.id, IrrelevantType.value))
      }
    } catch {
      case np: NullPointerException => {
        log.error("NullPointerException found for request: " + appRequest)
        log.error("Exception: " + np.getMessage)
        nullPointerExceptionTimer.time(failureCaseResponse(appRequest, INTERNALSERVERERROR.id, InternalServerError.value))
      }
      case e: Exception => {
        log.error("Exception found in the queryFlow: " + e.getMessage)
        exceptionTimer.time(failureCaseResponse(appRequest, INTERNALSERVERERROR.id, InternalServerError.value))
      }
    }

}

object StreamHelper {
  def apply(groupPolicyHelper: GroupPolicyService, parkingSpotHelper: ParkingSpaceService): StreamHelper = new StreamHelper(groupPolicyHelper, parkingSpotHelper)
}