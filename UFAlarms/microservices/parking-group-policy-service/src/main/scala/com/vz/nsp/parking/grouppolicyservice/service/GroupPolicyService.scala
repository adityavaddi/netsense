package com.vz.nsp.parking.grouppolicyservice.service

import java.util.{Calendar, UUID}

import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import com.vz.nsp.parking.CommonHelper.convertEpochToDate
import com.vz.nsp.parking.dblayer.DbLayer
import com.vz.nsp.parking.grouppolicyservice.helper.ReqResGenerator._
import com.vz.nsp.parking.model.CaselType._
import com.vz.nsp.parking.model.ResponseMessage.{policyidMissingInReq, _}
import com.vz.nsp.parking.model.StatusCodes._
import com.vz.nsp.parking.model.{AppRequest, AppResponse, _}
import nl.grons.metrics.scala.Timer

import scala.concurrent.{ExecutionContext, Future}

class GroupPolicyService(dbLayer: DbLayer) extends Logging with Instrumented{

  /**
    * GROUP POLICY SERVICE CASSANDRA METRICS
    */
  private[this] val cassandraUpdatePolicyStateTimer: Timer = metrics.timer("cassandra-update-policy-state")
  private[this] val cassandraReadPolicyUidOrgSIteTimer: Timer = metrics.timer("cassandra-read-policy-uidorgid siteid")
  private[this] val cassandraReadPolicyIdVersionTimer: Timer = metrics.timer("cassandra-read-policy-version")
  private[this] val cassandraReadPolicyWithMaxVersionTimer: Timer = metrics.timer("cassandra-read-policy-maxversion")
  private[this] val cassandraPostGroupPolicyTimer: Timer = metrics.timer("cassandra-create-grouppolicylink")
  private[this] val cassandraUpdateGroupPolicyTimer: Timer = metrics.timer("cassandra-update-grouppolicylink-policyid")
  private[this] val cassandraReadGroupPoliciesGroupidTimer: Timer = metrics.timer("cassandra-read-all-grouppolicylinks-groupid-live")
  private[this] val cassandraReadGroupPolicyGroupidLiveTimer: Timer = metrics.timer("cassandra-read-grouppolicylink-groupid-live")
  private[this] val cassandraReadGroupPolicyGroupidPolicyidTimer: Timer = metrics.timer("cassandra-read-grouppolicylink-groupidpolicyid-live")
  private[this] val cassandraReadGroupPolicyActiveGroups: Timer = metrics.timer("cassandra-read-grouppolicylink-activegroups-policyid")
  private[this] val cassandraReadGroupPolicyHistoryTimer: Timer = metrics.timer("cassandra-read-policygrouphistory")
  implicit val ec = ExecutionContext.Implicits.global

  def groupPolicyAssociationProcess(
      orgid: String, siteid: String, groupPolicyGetByIdRequest: AppRequest
  ): Future[AppResponse] = {
    log.debug(s"Entered groupPolicyAssociationProcess to associate a policy to parkinggroup and orgid: $orgid , siteid: $siteid")
    (groupPolicyGetByIdRequest.request.configprops.policyid,
     groupPolicyGetByIdRequest.request.configprops.ParkingGroupPolicyLink) match {
      case (None, _) => failureCaseResponse(groupPolicyGetByIdRequest, BADREQUEST.id, policyidMissingInReq.value)
      case (_, None) => failureCaseResponse(groupPolicyGetByIdRequest, BADREQUEST.id, groupLinkObjectMissing.value)
      case (Some(policyid), Some(parkinggroup)) => {
        cassandraReadPolicyWithMaxVersionTimer.time(dbLayer
          .getMaxVersionById(policyid))
          .flatMap(
            v =>
              cassandraReadPolicyIdVersionTimer.time(dbLayer
                .getPolicyByIdAndVersion(policyid, v, orgid, siteid))
                .flatMap(_ match {
                  case Nil =>
                    failureCaseResponse(
                      groupPolicyGetByIdRequest,
                      BADREQUEST.id,
                      NotFound_PolicyId_Table.value + groupPolicyGetByIdRequest.request.configprops.policyid.get + Does_Not_Exist_OrgSite.value + orgid + Siteid.value + siteid
                    )
                  case listOfPC => log.debug(s" policyId : $policyid")
                    associatingPolicyToGroup(policyid, parkinggroup.parkinggroupid, groupPolicyGetByIdRequest)
                })
          )
      }
    }
  }

  def associatingPolicyToGroup(policyid: String, parkinggroupid: String, groupPolicyGetByIdRequest: AppRequest): Future[AppResponse] =
    cassandraReadGroupPoliciesGroupidTimer.time(dbLayer
      .checkGroupLinkedWithPolicy(parkinggroupid))
      .flatMap(_.size > 0 match {
        case false =>
          val groupPolicyassociation = generateGroupPolicyObject(policyid, parkinggroupid)
          updateStateOfPolicy(groupPolicyassociation, groupPolicyGetByIdRequest)

        case true =>
          failureCaseResponse(groupPolicyGetByIdRequest, BADREQUEST.id, groupAssociatedWithOtherPolicy.value)
      })

  def updateStateOfPolicy(groupPolicyassociation: ParkingGroupPolicyLink,
                          groupPolicyGetByIdRequest: AppRequest): Future[AppResponse] =
    cassandraReadPolicyWithMaxVersionTimer.time(dbLayer
      .getMaxVersionById(groupPolicyassociation.policyid))
      .flatMap(
        a =>
          cassandraUpdatePolicyStateTimer.time(dbLayer
            .updateStateOfPolicy(groupPolicyassociation.policyid, a, Some(activeValue.value)))
            .flatMap(
              v =>
                v.success match {
                  case true => getMaxVersionAndInsertRecord(groupPolicyassociation, groupPolicyGetByIdRequest)
                  case false =>
                    failureCaseResponse(groupPolicyGetByIdRequest,
                                        INTERNALSERVERERROR.id,
                                        InternalServerError_associating.value)
              }
          )
      )

  def getMaxVersionAndInsertRecord(
      groupPolicyassociation: ParkingGroupPolicyLink,
      groupPolicyGetByIdRequest: AppRequest
  ): Future[AppResponse] =
    cassandraReadPolicyWithMaxVersionTimer.time(dbLayer
      .getMaxVersionById(groupPolicyassociation.policyid))
      .flatMap(
        a =>
          cassandraPostGroupPolicyTimer.time(dbLayer
            .applyGroupPolicyLink(groupPolicyassociation, a))
            .map(
              v =>
                v.success match {
                  case true => log.debug(s"Successfully associated the policy to parkinggroupid and policyId: ${groupPolicyassociation.policyid} and parkinggroupid: ${groupPolicyassociation.parkinggroupid}")
                    successResponseWithoutFuture(groupPolicyGetByIdRequest, SuccessMessage(true))
                  case false =>
                    failureCaseResponseWithoutFuture(groupPolicyGetByIdRequest,
                                                     INTERNALSERVERERROR.id,
                                                     InternalServerError_associating.value)
              }
          )
      )
  def generateGroupPolicyObject(policyid: String, parkinggroupid: String): ParkingGroupPolicyLink =
    ParkingGroupPolicyLink(
      uid = UUID.randomUUID().toString,
      parkinggroupid = parkinggroupid,
      policyid = policyid,
      version = 1,
      startTime = Calendar.getInstance().getTimeInMillis,
      endTime = -1
    )

  def groupPolicyDisassociationProcess(
                                        orgid: String, siteid: String, groupPolicyGetByIdRequest: AppRequest)
  : Future[AppResponse] =
 {
    log.debug(s"Entered groupPolicyDisassociationProcess to disassociate a policy to parkinggroup and orgid: $orgid siteid: $siteid")
    (groupPolicyGetByIdRequest.request.configprops.policyid,
      groupPolicyGetByIdRequest.request.configprops.ParkingGroupPolicyLink) match {
      case (None, _) => failureCaseResponse(groupPolicyGetByIdRequest, BADREQUEST.id, policyidMissingInReq.value)
      case (_, None) => failureCaseResponse(groupPolicyGetByIdRequest, BADREQUEST.id, groupLinkObjectMissing.value)
      case (Some(policyid), Some(parkinggroup)) => {
        cassandraReadPolicyWithMaxVersionTimer.time(dbLayer
          .getMaxVersionById(policyid))
          .flatMap(
            v =>
              cassandraReadPolicyIdVersionTimer.time(dbLayer
                .getPolicyByIdAndVersion(policyid, v, orgid, siteid))
                .flatMap(_ match {
                  case Nil =>
                    failureCaseResponse(
                      groupPolicyGetByIdRequest,
                      BADREQUEST.id,
                      NotFound_PolicyId_Table.value + groupPolicyGetByIdRequest.request.configprops.policyid.get + Does_Not_Exist_OrgSite.value + orgid + Siteid.value + siteid
                    )
                  case listOfPC =>
                    cassandraReadGroupPolicyGroupidPolicyidTimer.time(dbLayer
                      .checkGroupAssociatedWithPolicy(parkinggroup.parkinggroupid, policyid))
                      .flatMap(_.size > 0 match {
                        case false =>
                          failureCaseResponse(groupPolicyGetByIdRequest,
                            BADREQUEST.id,
                            groupNotAssociatedWithThisPolicy.value + policyid + " or any")
                        case true =>
                          getMaxVersionAndUpdateRecord(policyid, parkinggroup.parkinggroupid, v, groupPolicyGetByIdRequest)
                      })
                })
          )
      }
    }
  }

  def getMaxVersionAndUpdateRecord(
      policyid: String,
      parkinggroupid: String,
      version: Int,
      groupPolicyGetByIdRequest: AppRequest
  ): Future[AppResponse] =
    cassandraReadGroupPolicyGroupidLiveTimer.time(dbLayer
      .getGroupPolicyByGroupId(parkinggroupid))
      .flatMap(
        a =>
          cassandraUpdateGroupPolicyTimer.time(dbLayer
            .updateGroupPolicyLink(a.get.uid))
            .flatMap(
              v =>
                v.success match {
                  case false =>
                    failureCaseResponse(groupPolicyGetByIdRequest,
                                        INTERNALSERVERERROR.id,
                                        InternalServerError_disassociating.value)
                  case true => checkPolicyAssociatedtoAnyGroup(policyid, version, groupPolicyGetByIdRequest)
              }
          )
      )


  def checkPolicyAssociatedtoAnyGroup(policyid: String, version: Int, groupPolicyGetByIdRequest: AppRequest): Future[AppResponse] =
    cassandraReadGroupPolicyActiveGroups.time(dbLayer
      .getAllActicveGroupsByPolicyId(policyid, version))
      .flatMap(
        v =>
          v.success match {
            case true  => log.debug(s"Successfully disassociated the policy to parkinggroupid and policyId: $policyid")
              successResponse(groupPolicyGetByIdRequest, SuccessMessage(true))
            case false => updateStatusToInactive(policyid, version, groupPolicyGetByIdRequest)
        }
      )

  def updateStatusToInactive(policyid: String, version: Int, groupPolicyGetByIdRequest: AppRequest): Future[AppResponse] =
    cassandraUpdatePolicyStateTimer.time(dbLayer
      .updateStateOfPolicy(policyid, version, Option(inactiveValue.value)))
      .map(
        v =>
          v.success match {
            case true => successResponseWithoutFuture(groupPolicyGetByIdRequest, SuccessMessage(true))
            case false =>
              failureCaseResponseWithoutFuture(groupPolicyGetByIdRequest,
                                               INTERNALSERVERERROR.id,
                                               InternalServerError_inactiveState.value)
        }
      )

  def getAssociatedParkingGroupsOfActivePolicy(orgid: String, siteid: String, getAssignedParkingGroups: AppRequest):Future[AppResponse] ={
   log.debug(s"Entered getAssociatedParkingGroupsOfActivePolicy to get associated parkinggroups to the policy and orgid: $orgid , siteid: $siteid")
    (getAssignedParkingGroups.request.configprops.policyid) match {

      case (None) => log.debug("policyid not found in request")
        failureCaseResponse(getAssignedParkingGroups, BADREQUEST.id, policyidMissingInReq.value)
      case (Some(policyId)) => {
        cassandraReadPolicyUidOrgSIteTimer.time(dbLayer.getAllLivePoliciesById(policyId, orgid, siteid))
          .flatMap(policies => policies.size > 0 match {
          case false => log.debug("policyid not found in DB")
            failureCaseResponse(getAssignedParkingGroups, BADREQUEST.id, NotFound_PolicyId_Table.value + policyId + Does_Not_Exist_OrgSite.value + orgid + Siteid.value + siteid)
          case true => getAssignedParkingGroupsOfLivePolicy(getAssignedParkingGroups, policyId)

        })
      }
    }
  }

  def getAssignedParkingGroupsOfLivePolicy(getAssignedParkingGroups: AppRequest, policyId: String)
                                          : Future[AppResponse] = {
    cassandraReadGroupPolicyHistoryTimer.time(dbLayer.getAssociatedParkingGroupsOfPolicyId(policyId))
      .flatMap(_ match {
        case listOfParkingGroup => {
          log.debug(s"Successfully got the list of parkinggroups for policyId: $policyId")
          successResponse(getAssignedParkingGroups, convertToUTCDateTimeAtParkingGroup(listOfParkingGroup))
        }
      })
  }

  def convertToUTCDateTimeAtParkingGroup(lst : List[ParkingGroupInputList]) : List[ParkingGroupOutputList] = {
    lst.map(pginput => {
      val startTime = convertEpochToDate(pginput.stTime)
      val endTime = if (pginput.edTime == -1) "" else convertEpochToDate(pginput.edTime)
      ParkingGroupOutputList(pginput.gpId, startTime, endTime, pginput.version)
    })
  }

}

object GroupPolicyService {
  def apply(dbLayer: DbLayer): GroupPolicyService = new GroupPolicyService(dbLayer)
}
