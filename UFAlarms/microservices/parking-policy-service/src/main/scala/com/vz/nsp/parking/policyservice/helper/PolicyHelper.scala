package com.vz.nsp.parking.policyservice.helper

import java.time.Instant

import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import com.vz.nsp.parking.CommonHelper.validateTimeRangeConflicts
import com.vz.nsp.parking.dblayer.DbLayer
import com.vz.nsp.parking.model.CaselType._
import com.vz.nsp.parking.model.ResponseMessage.{NotFound_TagIds, _}
import com.vz.nsp.parking.model.StatusCodes._
import com.vz.nsp.parking.model._
import com.vz.nsp.parking.policyservice.helper.ReqResGenerator.{successResponseWithoutFuture, _}
import nl.grons.metrics.scala.Timer

import scala.concurrent.{ExecutionContext, Future}

class PolicyHelper(dbLayer: DbLayer) extends Logging with Instrumented {

  implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  /**
   * POLICY SERVICE CASSANDRA METRICS
   */
  private[this] val cassandraPostPolicyTimer: Timer               = metrics.timer("cassandra-create-policy")
  private[this] val cassandraUpdatePolicyVersionTimer: Timer      = metrics.timer("cassandra-update-policy-version")
  private[this] val cassandraUpdatePolicyStateTimer: Timer        = metrics.timer("cassandra-update-policy-state")
  private[this] val cassandraDeletePolicyVersionTimer: Timer      = metrics.timer("cassandra-delete-policy-version")
  private[this] val cassandraReadPolicyUidOrgSIteTimer: Timer     = metrics.timer("cassandra-read-policy-uidorgid siteId")
  private[this] val cassandraReadAllPoliciesTimer: Timer          = metrics.timer("cassandra-read-all-policies")
  private[this] val cassandraReadPolicyIdVersionTimer: Timer      = metrics.timer("cassandra-read-policy-version")
  private[this] val cassandraReadPolicyIdStateTimer: Timer        = metrics.timer("cassandra-read-policy-state")
  private[this] val cassandraReadPolicyWithMaxVersionTimer: Timer = metrics.timer("cassandra-read-policy-maxversion")
  private[this] val cassandraReadPolicyByOnlyIdTimer: Timer       = metrics.timer("cassandra-read-policy-policyid")
  private[this] val cassandraReadPolicyByUidLiveTimer: Timer      = metrics.timer("cassandra-read-policy-onlyuid-live")
  private[this] val cassandraReadPoliciesNameTimer: Timer         = metrics.timer("cassandra-read-policies-name")
  private[this] val cassandraReadPoliciesTagidTimer: Timer        = metrics.timer("cassandra-read-policies-tagid")
  private[this] val cassandraReadPoliciesOnlyTagidTimer: Timer    = metrics.timer("cassandra-read-policies-onlytagid")
  private[this] val cassandraReadPoliciesVersionHistory: Timer    = metrics.timer("cassandra-read-policies-versionhistory")
  private[this] val cassandraReadAllTagsByListTimer: Timer        = metrics.timer("cassandra-read-all-tagidlist")
  private[this] val cassandraReadGroupPolicyPolicyidTimer: Timer =
    metrics.timer("cassandra-read-grouppolicylink-policyid")
  private[this] val cassandraReadGroupPolicyGroupidLiveTimer: Timer =
    metrics.timer("cassandra-read-grouppolicylink-groupid-live")
  private[this] val cassandraReadGroupPolicyGroupidTimer: Timer =
    metrics.timer("cassandra-read-grouppolicylink-groupid")

  def postParkingPolicyProcess(orgid: String,
                               siteid: String,
                               policyGetByIdRequest: AppRequest,
                               requestType: String): Future[AppResponse] = {
    log.info(s"Entered postParkingPolicyProcess to store policy with orgId: $orgid and siteId: $siteid and requestType: $requestType")
    policyGetByIdRequest.request.configprops.policy match {
      case None =>
        log.error(s"policy not found in request and messageid: ${policyGetByIdRequest.messageid}")
        failurecaseResponse(policyGetByIdRequest, BADREQUEST.id, NotFound_Policy.value)
      case Some(policyFromUser) =>
        log.debug(s" policy from user: $policyFromUser")
        val conflictingTimeRange = validateTimeRangesWithinPolicyRule(policyFromUser)
        if (conflictingTimeRange.nonEmpty) {
          failurecaseResponse(policyGetByIdRequest,
                              BADREQUESTINPAYLOAD.id,
                              TimeRangeConflictedEntries.value
                              + conflictingTimeRange.toArray.mkString(" , "))
        } else {
          val policy = generatePolicyObject(policyFromUser, orgid, siteid, policyGetByIdRequest.request.user)
          dbLayer
            .policyExistWithSameNameInSite(orgid, siteid, policy.name, requestType)
            .flatMap(
              exist =>
                if (exist) {
                  log.debug(s"Policy already exist with name ${policy.name}")
                  failurecaseResponse(policyGetByIdRequest, BADREQUEST.id, PolicyAlreadyExists.value + policy.name)
                } else {
                  cassandraPostPolicyTimer
                    .time(
                      dbLayer
                        .storePolicy(policy, requestType)
                    )
                    .map(
                      a =>
                        if (a.success) {
                          log.info("Persisted successfully policy " + policy)
                          successResponseWithoutFuture(policyGetByIdRequest, generatePolicyResponse(policy))
                        } else {
                          log.error("Failed while storing policy")
                          failurecaseResponseWithoutFuture(policyGetByIdRequest,
                                                           INTERNALSERVERERROR.id,
                                                           InternalError_StorePolicy.value)
                      }
                    )
              }
            )
        }
    }
  }

  def getParkingProcess(orgid: String,
                        siteid: String,
                        policyGetByIdRequest: AppRequest,
                        requestType: String): Future[AppResponse] = {
    log.info(s"Entered getParkingProcess to get policy  with orgId: $orgid and siteId: $siteid and requestType: $requestType")
    policyGetByIdRequest.request.configprops.policyid match {
      case None =>
        log.error(s" policyid not found in request and messageid: ${policyGetByIdRequest.messageid}")
        failurecaseResponse(policyGetByIdRequest, BADREQUEST.id, NotFound_PolicyId.value)
      case Some(policyId) =>
        log.debug(s" policyId : $policyId")
        getPolicyIdAndVersion(
          policyId,
          cassandraReadPolicyWithMaxVersionTimer.time(
            dbLayer.getMaxVersionPolicyByIdOrgSite(policyId, orgid, siteid, requestType)
          ),
          orgid,
          siteid,
          requestType
        ).flatMap {
          case Nil =>
            log.debug(
              NotFound_PolicyId_Table.value + policyId + Does_Not_Exist_OrgSite.value + orgid + Siteid.value + siteid
            )
            failurecaseResponse(
              policyGetByIdRequest,
              BADREQUEST.id,
              NotFound_PolicyId_Table.value + policyId + Does_Not_Exist_OrgSite.value + orgid + Siteid.value + siteid
            )
          case listOfPC =>
            log.info("Got the policy By policyid " + listOfPC.head)
            appendAssociatedParkingGroupsForPolicy(listOfPC.head, policyGetByIdRequest)
        }
    }
  }

  private def appendAssociatedParkingGroupsForPolicy(
      policy: Policy,
      policyGetByIdRequest: AppRequest
  ): Future[AppSuccessResponse[PolicyResponse]] =
    dbLayer
      .getAssociatedParkingGroupsForPolicy(policy.uid)
      .map(res => {
        val associatedParkingGroups = if (res.isEmpty) None else Some(res)
        successResponseWithoutFuture(policyGetByIdRequest,
                                     generatePolicyResponse(policy, associatedParkingGroups = associatedParkingGroups))
      })

  def getWhatIfParkingProcess(orgId: String,
                              siteId: String,
                              policyGetByIdRequest: AppRequest,
                              requestType: String): Future[AppResponse] = {
    log.info(s"Entered getParkingProcess to get policy  with orgId: $orgId and siteId: $siteId and requestType: $requestType")
    policyGetByIdRequest.request.configprops.policyid match {
      case None =>
        log.error(s" policyid not found in request and messageid: ${policyGetByIdRequest.messageid}")
        failurecaseResponse(policyGetByIdRequest, BADREQUEST.id, NotFound_PolicyId.value)
      case Some(policyId) =>
        log.debug(s" policyId : $policyId")
        getPolicyIdAndVersion(
          policyId,
          dbLayer.getMaxVersionPolicyByIdOrgSite(policyId, orgId, siteId, requestType),
          orgId,
          siteId,
          requestType
        ).flatMap(listOfPolicies => {
          if (listOfPolicies.isEmpty) {
            log.debug("Failed while getting the policy by id")
            failurecaseResponse(
              policyGetByIdRequest,
              BADREQUEST.id,
              NotFound_PolicyId_Table.value + policyId + Does_Not_Exist_OrgSite.value + orgId + Siteid.value + siteId
            )
          } else {
            log.info("Got the policy By policyid " + listOfPolicies.head)
            getWhatIfJobsByPolicyById(listOfPolicies.head, orgId, siteId, policyGetByIdRequest)
          }
        })
    }
  }

  def getWhatIfJobsByPolicyById(policy: Policy,
                                orgId: String,
                                siteId: String,
                                policyGetByIdRequest: AppRequest): Future[AppResponse] =
    dbLayer
      .getAllJobs(orgId, siteId)
      .map(whatIfList => {

        val filteredWhatIfProjects =
          whatIfList.filter(whatIfProject => whatIfProject.policieswithversion.getOrElse(List()).contains(policy.uid))

        successResponseWithoutFuture(policyGetByIdRequest, generatePolicyResponse(policy, filteredWhatIfProjects))

      })

  def getPolicyIdAndVersion(policyId: String,
                            versionVal: Future[Int],
                            orgid: String,
                            siteid: String,
                            requestType: String): Future[List[Policy]] = {
    val y = for {
      v <- versionVal
    } yield
      cassandraReadPolicyIdVersionTimer.time(dbLayer.getPolicyByIdAndVersion(policyId, v, orgid, siteid, requestType))
    y.flatten
  }

  def getAllParkingProcess(orgid: String,
                           siteid: String,
                           policyGetByIdRequest: AppRequest,
                           requestType: String): Future[AppSuccessResponse[List[PolicyResponse]]] = {
    log.info(s"Entered getAllParkingProcess with orgId: $orgid and siteId: $siteid and requestType: $requestType")
    cassandraReadAllPoliciesTimer
      .time(
        dbLayer
          .getAllPolicies(orgid, siteid, requestType)
      )
      .flatMap(listOfPolicies => {
        log.info("Successfully got parking policies and searching for their associated groups")
        val policiesWithAssociation = Future.traverse(listOfPolicies)(
          policy =>
            dbLayer
              .getAssociatedParkingGroupsForPolicy(policy.uid)
              .map(res => {
                val associatedParkingGroups = if (res.isEmpty) None else Some(res)
                generatePolicyResponse(policy, associatedParkingGroups = associatedParkingGroups)
              })
        )
        log.info("Successfully appended parkinggroups associated to policies")
        policiesWithAssociation
          .map(result => successResponseWithoutFuture[List[PolicyResponse]](policyGetByIdRequest, result))
      })
  }

  def getAllWhatIfPolicies(orgId: String, siteId: String, requestType: String)(
      implicit ec: ExecutionContext
  ): Future[List[PolicyResponse]] = {
    log.info(s"Entered getAllParkingProcess with orgId: $orgId and siteId: $siteId and requestType: $requestType")

    cassandraReadAllPoliciesTimer
      .time(
        dbLayer
          .getAllPolicies(orgId, siteId, requestType)
      )
      .flatMap(listOfPolicies => {
        dbLayer
          .getAllJobs(orgId, siteId)
          .map(whatIfList => {
            listOfPolicies.map(eachPolicy => {
              val filteredWhatIfProjects =
                whatIfList.filter(whatIfProject => whatIfProject.policieswithversion.getOrElse(List()).contains(eachPolicy.uid))
              generatePolicyResponse(eachPolicy, filteredWhatIfProjects)
            })
          })
      })

  }

  def getAllWhatIfParkingProcess(
      orgId: String,
      siteId: String,
      policyGetByIdRequest: AppRequest,
      requestType: String
  ): Future[AppSuccessResponse[List[PolicyResponse]]] = {
    log.info(s"Entered  getBusinessAlertsProcess with orgId: $orgId and siteId: $siteId and requestType: $requestType")
    getAllWhatIfPolicies(orgId, siteId, requestType)
      .map(policies => {
        log.info(s"Successfully got the BusinessAlerts for siteId: $siteId and orgId: $orgId")
        successResponseWithoutFuture(
          policyGetByIdRequest,
          policies
        )
      })
  }

  def updateParkingProcess(orgid: String,
                           siteid: String,
                           policyGetByIdRequest: AppRequest,
                           requestType: String): Future[AppResponse] = {
    log.info(s"Entered updateParkingProcess to update the policy with orgId: $orgid siteId:$siteid and requestType: $requestType")
    (policyGetByIdRequest.request.configprops.policyid, policyGetByIdRequest.request.configprops.policy) match {
      case (None, _) =>
        log.error(s" policyid not found in request and messageid: ${policyGetByIdRequest.messageid}")
        failurecaseResponse(policyGetByIdRequest, BADREQUEST.id, policyidMissingInReq.value)
      case (_, None) =>
        log.error(" policy not found in request")
        failurecaseResponse(policyGetByIdRequest, BADREQUEST.id, policyMissingInReq.value)
      case (Some(policyId), Some(policy)) =>
        log.debug(s" policyId : $policyId")
        log.debug(s" policy : $policy")
        val conflictingTimeRange = validateTimeRangesWithinPolicyRule(policy)
        if (conflictingTimeRange.nonEmpty) {
          failurecaseResponse(policyGetByIdRequest,
                              BADREQUESTINPAYLOAD.id,
                              TimeRangeConflictedEntries.value
                              + conflictingTimeRange.toArray.mkString(" , "))
        } else checkTimelineForPolicy(policyId, policyGetByIdRequest, policy, orgid, siteid, requestType)
    }
  }

  def checkTimelineForPolicy(policyid: String,
                             policyGetByIdRequest: AppRequest,
                             policyFromUser: PolicyRequest,
                             orgId: String,
                             siteid: String,
                             requestType: String)(
      implicit ec: ExecutionContext
  ): Future[AppResponse] =
    cassandraReadGroupPolicyPolicyidTimer
      .time(
        dbLayer
          .getPolicyByIdInTimeline(policyid, requestType)
      )
      .flatMap(
        f = policyGroupLink =>
          if (policyGroupLink.nonEmpty) {
            failurecaseResponse(policyGetByIdRequest, BADREQUEST.id, policyAssociatedToGroup.value)
          } else {
            getMaxVerionOfPolicyById(policyid, policyGetByIdRequest, policyFromUser, orgId, siteid, requestType)
        }
      )

  def updateWhatIfParkingProcess(orgId: String,
                                 siteId: String,
                                 policyGetByIdRequest: AppRequest,
                                 requestType: String): Future[AppResponse] = {
    log.info(s"Entered updateParkingProcess to update the policy with orgId: $orgId siteId:$siteId and requestType: $requestType")
    (policyGetByIdRequest.request.configprops.policyid, policyGetByIdRequest.request.configprops.policy) match {
      case (None, _) =>
        log.error(s" policyid not found in request and messageid: ${policyGetByIdRequest.messageid}")
        failurecaseResponse(policyGetByIdRequest, BADREQUEST.id, policyidMissingInReq.value)
      case (_, None) =>
        log.error(s" policy not found in request and messageid: ${policyGetByIdRequest.messageid}")
        failurecaseResponse(policyGetByIdRequest, BADREQUEST.id, policyMissingInReq.value)
      case (Some(policyId), Some(policy)) =>
        log.debug(s" policyId : $policyId")
        log.debug(s" policy : $policy")
        val conflictingTimeRange = validateTimeRangesWithinPolicyRule(policy)
        if (conflictingTimeRange.nonEmpty) {
          failurecaseResponse(policyGetByIdRequest,
                              BADREQUESTINPAYLOAD.id,
                              TimeRangeConflictedEntries.value
                              + conflictingTimeRange.toArray.mkString(" , "))
        } else checkWhatIfJobTable(policyId, policyGetByIdRequest, policy, orgId, siteId, requestType)
    }
  }

  def checkWhatIfJobTable(policyId: String,
                          policyGetByIdRequest: AppRequest,
                          policyFromUser: PolicyRequest,
                          orgId: String,
                          siteId: String,
                          requestType: String)(
      implicit ec: ExecutionContext
  ): Future[AppResponse] =
    cassandraReadGroupPolicyPolicyidTimer
      .time(
        dbLayer
          .checkTheStatusOfJobsById(policyId, orgId, siteId)
      )
      .flatMap { jobs =>
        if (jobs.nonEmpty) {
          failurecaseResponse(policyGetByIdRequest,
                              BADREQUEST.id,
                              policyAssociatedToWhatIfProject.value + jobs.map(job => job.jobid))
        } else {
          getMaxVerionOfPolicyById(policyId, policyGetByIdRequest, policyFromUser, orgId, siteId, requestType)
        }
      }

  def getMaxVerionOfPolicyById(policyid: String,
                               policyGetByIdRequest: AppRequest,
                               policyFromUser: PolicyRequest,
                               orgid: String,
                               siteid: String,
                               requestType: String)(
      implicit ec: ExecutionContext
  ): Future[AppResponse] =
    cassandraReadPolicyUidOrgSIteTimer
      .time(
        dbLayer
          .getAllLivePoliciesById(policyid, orgid, siteid, requestType)
      )
      .flatMap(
        listOfPolicies =>
          if (listOfPolicies.nonEmpty) {
            val maxVersionedPolicy = listOfPolicies.maxBy(_.version)
            dbLayer
              .policyExistWithSameNameInSite(orgid,
                                             siteid,
                                             policyFromUser.name,
                                             requestType,
                                             Some(maxVersionedPolicy.name))
              .flatMap(
                exist =>
                  if (exist) {
                    log.info(s"Policy already exist with name ${policyFromUser.name}")
                    failurecaseResponse(policyGetByIdRequest,
                                        BADREQUEST.id,
                                        PolicyAlreadyExists.value + policyFromUser.name)
                  } else {
                    updateDBPolicyAndGenerateResponse1(policyGetByIdRequest,
                                                       maxVersionedPolicy,
                                                       policyFromUser,
                                                       policyid,
                                                       requestType)
                }
              )
          } else {
            failurecaseResponse(
              policyGetByIdRequest,
              BADREQUEST.id,
              NotFound_PolicyId_Table.value + policyid + Does_Not_Exist_OrgSite.value + orgid + Siteid.value + siteid
            )
        }
      )

  def updateDBPolicyAndGenerateResponse1(policyGetByIdRequest: AppRequest,
                                         policyFromDB: Policy,
                                         policyFromUser: PolicyRequest,
                                         policyId: String,
                                         requestType: String): Future[AppResponse] = {
    val updatedpolicy = updatePolicyObject(policyFromDB, policyFromUser, policyGetByIdRequest.request.user)
    cassandraUpdatePolicyVersionTimer
      .time(
        dbLayer
          .updateByPolicyId(policyId, policyFromDB.version + 1, updatedpolicy, requestType)
      )
      .map(
        s =>
          if (s.success) {
            log.info("Successfully updated the policy with " + updatedpolicy)
            successResponseWithoutFuture(policyGetByIdRequest, generatePolicyResponse(updatedpolicy))
          } else {
            failurecaseResponseWithoutFuture(policyGetByIdRequest,
                                             INTERNALSERVERERROR.id,
                                             InternalError_UpdatePolicy.value)
        }
      )

  }

  def updatePolicyTags(orgid: String,
                       siteid: String,
                       addTagsToPolicyRequest: AppRequest,
                       requestType: String): Future[AppResponse] = {
    log.info(s"Entered updatePolicyTags to update the the policy with orgId: $orgid and siteId: $siteid and requestType: $requestType")
    addTagsToPolicyRequest.request.configprops.policyid match {
      case (None) =>
        log.error(s" policyid not found in request and messageid: ${addTagsToPolicyRequest.messageid}")
        failurecaseResponse(addTagsToPolicyRequest, BADREQUEST.id, NotFound_PolicyId.value)
      case Some(policyid) =>
        log.debug(s" policyid : $policyid")
        getLatestPolicyAndUpdateTags(addTagsToPolicyRequest, policyid, orgid, siteid, requestType)
    }
  }

  def getLatestPolicyAndUpdateTags(addTagsToPolicyRequest: AppRequest,
                                   policyid: String,
                                   orgid: String,
                                   siteid: String,
                                   requestType: String): Future[AppResponse] =
    cassandraReadPolicyUidOrgSIteTimer
      .time(dbLayer.getAllLivePoliciesById(policyid, orgid, siteid, requestType))
      .flatMap(
        listOfPolices =>
          if (listOfPolices.nonEmpty) {
            val latestPolicy = listOfPolices.maxBy(_.version)
            updatePolicyWithTags(addTagsToPolicyRequest: AppRequest, latestPolicy, policyid, orgid, siteid, requestType)
          } else {
            failurecaseResponse(
              addTagsToPolicyRequest,
              BADREQUEST.id,
              NotFound_PolicyId_Table.value + policyid + Does_Not_Exist_OrgSite.value + orgid + Siteid.value + siteid
            )
        }
      )

  def updatePolicyWithTags(addTagsToPolicyRequest: AppRequest,
                           latestVersionPolicyDB: Policy,
                           policyid: String,
                           orgId: String,
                           siteId: String,
                           requestType: String): Future[AppResponse] =
    addTagsToPolicyRequest.request.`type` match {
      case (policyTagsAssociation.value | whatIfPolicyTagsAssociation.value) =>
        addTagsToPolicy(addTagsToPolicyRequest, latestVersionPolicyDB, policyid, orgId, siteId, requestType)
      case (policyTagsDisassociation.value | whatIfPolicyTagsDisassociation.value) =>
        removeTagsFromPolicy(addTagsToPolicyRequest, latestVersionPolicyDB, policyid, orgId, siteId, requestType)
    }

  def addTagsToPolicy(addTagsToPolicyRequest: AppRequest,
                      latestVersionPolicyDB: Policy,
                      policyid: String,
                      orgId: String,
                      siteId: String,
                      requestType: String): Future[AppResponse] = {
    val tagids = addTagsToPolicyRequest.request.configprops.tagspolicylink.get.tagids.filter(_.trim.nonEmpty)
    log.info(s"tagids : $tagids")
    if (tagids.nonEmpty) {
      cassandraReadAllTagsByListTimer
        .time(dbLayer.getExistingTagIds(tagids.toList, orgId, siteId, requestType))
        .flatMap(
          tagsExistInDb =>
            if (tagsExistInDb.nonEmpty) {
              val cumulativeTags = latestVersionPolicyDB.tags ++ tagsExistInDb
              val updatedPolicy  = associateTagToPolicyObject(latestVersionPolicyDB, cumulativeTags)
              val invalidTagids  = tagids -- tagsExistInDb
              val diffBwActualAndInserted: Option[Set[String]] =
                if (invalidTagids.isEmpty) None else Some(invalidTagids)
              upsertPolicyTags(addTagsToPolicyRequest, updatedPolicy, policyid, diffBwActualAndInserted, requestType)
            } else {
              failurecaseResponse(
                addTagsToPolicyRequest,
                BADREQUEST.id,
                NotFound_TagIds.value + Does_Not_Exist_OrgSite.value + orgId + Siteid.value + siteId
              )
          }
        )
    } else {
      failurecaseResponse(addTagsToPolicyRequest, BADREQUEST.id, NoContent_RequestBody.value)
    }

  }

  def removeTagsFromPolicy(addTagsToPolicyRequest: AppRequest,
                           latestVersionPolicyDB: Policy,
                           policyid: String,
                           orgId: String,
                           siteId: String,
                           requestType: String): Future[AppResponse] = {
    val removingTagids = addTagsToPolicyRequest.request.configprops.tagspolicylink.get.tagids.filter(_.trim.nonEmpty)
    log.info(s"removingTagids : $removingTagids")
    if (removingTagids.nonEmpty) {
      val diffTags: Set[String] = latestVersionPolicyDB.tags -- removingTagids
      val updatedPolicy         = associateTagToPolicyObject(latestVersionPolicyDB, diffTags)
      val invalidTagids         = removingTagids -- latestVersionPolicyDB.tags
      if (invalidTagids == removingTagids)
        failurecaseResponse(
          addTagsToPolicyRequest,
          BADREQUEST.id,
          NotAssociated_TagIds.value
        )
      else {
        val diffBwActualAndInserted: Option[Set[String]] =
          if (invalidTagids.isEmpty) None else Some(invalidTagids)
        upsertPolicyTags(addTagsToPolicyRequest, updatedPolicy, policyid, diffBwActualAndInserted, requestType)
      }
    } else {
      failurecaseResponse(addTagsToPolicyRequest, BADREQUEST.id, NoContent_RequestBody.value)
    }

  }

  def upsertPolicyTags(addTagsToPolicyRequest: AppRequest,
                       updatedPolicy: Policy,
                       policyid: String,
                       invalidTagIds: Option[Set[String]],
                       requestType: String): Future[AppResponse] =
    cassandraUpdatePolicyVersionTimer
      .time(dbLayer.updateByPolicyId(policyid, updatedPolicy.version, updatedPolicy, requestType))
      .map(
        s =>
          if (s.success) {
            log.info("successfully updated policy with tagids")
            successResponseWithoutFuture(addTagsToPolicyRequest, SuccessMessageWithInvalidTags(true, invalidTagIds))
          } else {
            log.error("Failed to update policy with tagids")
            failurecaseResponseWithoutFuture(addTagsToPolicyRequest,
                                             INTERNALSERVERERROR.id,
                                             InternalError_UpdatePolicy.value)
        }
      )

  def deletePolicyProcess(orgid: String,
                          siteid: String,
                          policyGetByIdRequest: AppRequest,
                          requestType: String): Future[AppResponse] = {
    log.info(s"Entered deletePolicyProcess to delete policy with  orgId: $orgid siteId: $siteid and requestType: $requestType")
    policyGetByIdRequest.request.configprops.policyid match {
      case None =>
        log.error(s" policyid not found in request and messageid: ${policyGetByIdRequest.messageid}")
        failurecaseResponse(policyGetByIdRequest, BADREQUEST.id, NotFound_PolicyId.value)
      case Some(policyId) =>
        log.debug(s" policyId : $policyId")
        cassandraReadGroupPolicyPolicyidTimer
          .time(
            dbLayer
              .getPolicyByIdInTimeline(policyId, requestType)
          )
          .flatMap(
            policyGroupLink =>
              if (policyGroupLink.nonEmpty) {
                failurecaseResponse(policyGetByIdRequest, BADREQUEST.id, policyAssociatedToGroup.value)
              } else {
                getPolicyIdAndVersion(
                  policyId,
                  cassandraReadPolicyWithMaxVersionTimer
                    .time(dbLayer.getMaxVersionPolicyByIdOrgSite(policyId, orgid, siteid, requestType)),
                  orgid,
                  siteid,
                  requestType
                ).flatMap {
                  case Nil =>
                    failurecaseResponse(
                      policyGetByIdRequest,
                      BADREQUEST.id,
                      NotFound_PolicyId_Table.value + policyId + Does_Not_Exist_OrgSite.value + orgid + Siteid.value + siteid
                    )
                  case listOfPC =>
                    deletePolicyFromDBandGenerateResponse(policyGetByIdRequest, policyId, orgid, siteid, requestType)
                }
            }
          )
    }
  }

  def deleteWhatIfPolicyProcess(orgid: String,
                                siteid: String,
                                policyGetByIdRequest: AppRequest,
                                requestType: String): Future[AppResponse] = {
    log.info(s"Entered deletePolicyProcess to delete policy with  orgId: $orgid siteId: $siteid and requestType: $requestType")
    policyGetByIdRequest.request.configprops.policyid match {
      case None =>
        log.error(s" policyid not found in request and messageid: ${policyGetByIdRequest.messageid} ")
        failurecaseResponse(policyGetByIdRequest, BADREQUEST.id, NotFound_PolicyId.value)
      case Some(policyId) =>
        log.debug(s" policyId : $policyId")
        cassandraReadGroupPolicyPolicyidTimer
          .time(
            dbLayer
              .getWhatIfJobsByPolicyId(policyId, orgid, siteid)
          )
          .flatMap(
            jobs =>
              if (jobs.isEmpty)
                getPolicyIdAndVersion(
                  policyId,
                  cassandraReadPolicyWithMaxVersionTimer
                    .time(dbLayer.getMaxVersionPolicyByIdOrgSite(policyId, orgid, siteid, requestType)),
                  orgid,
                  siteid,
                  requestType
                ).flatMap {
                  case Nil =>
                    failurecaseResponse(
                      policyGetByIdRequest,
                      BADREQUEST.id,
                      NotFound_PolicyId_Table.value + policyId + Does_Not_Exist_OrgSite.value + orgid + Siteid.value + siteid
                    )
                  case listOfPC =>
                    deletePolicyFromDBandGenerateResponse(policyGetByIdRequest, policyId, orgid, siteid, requestType)
                } else {
                failurecaseResponse(policyGetByIdRequest,
                                    BADREQUEST.id,
                                    policyAssociatedToWhatIfProject.value + jobs.map(_.jobid))
            }
          )
    }
  }

  def deletePolicyFromDBandGenerateResponse(policyGetByIdRequest: AppRequest,
                                            policyId: String,
                                            orgId: String,
                                            siteId: String,
                                            requestType: String): Future[AppSuccessResponse[SuccessMessage]] =
    cassandraReadPolicyByOnlyIdTimer
      .time(
        dbLayer
          .getAllPoliciesById(policyId, orgId, siteId, requestType)
      )
      .map(
        e =>
          e.foreach(policy => {
            cassandraDeletePolicyVersionTimer
              .time(dbLayer.deleteByPolicyIdAndVersion(policyId, policy.version, orgId, siteId, requestType))
          })
      )
      .map(x => successResponseWithoutFuture(policyGetByIdRequest, SuccessMessage(true)))

  def getVersionHistoryProcess(orgid: String, siteid: String, policyGetByIdRequest: AppRequest): Future[AppResponse] = {
    log.info(s"Entered getVersionHistoryProcess of policy with  orgId: $orgid siteId: $siteid")
    policyGetByIdRequest.request.configprops.policyid match {
      case None =>
        log.error(s" policyid not found in request and messageid: ${policyGetByIdRequest.messageid}")
        failurecaseResponse(policyGetByIdRequest, BADREQUEST.id, NotFound_PolicyId.value)
      case Some(policyId) =>
        log.debug(s" policyid: $policyId")
        cassandraReadPoliciesVersionHistory
          .time(
            dbLayer
              .getVersionHistoryById(policyId, orgid, siteid)
          )
          .map {
            case Nil =>
              failurecaseResponseWithoutFuture(
                policyGetByIdRequest,
                BADREQUEST.id,
                NotFound_PolicyId_Table.value + policyId + Does_Not_Exist_OrgSite.value + orgid + Siteid.value + siteid
              )
            case listOfPC =>
              log.info(s"Successfully got the getVersionHistoryProcess of policy by policyid $policyId")
              successResponseWithoutFuture(policyGetByIdRequest, listOfPC.map(policy => generatePolicyResponse(policy)))
          }
    }
  }

  def getPolicyByVersionNumberAndId(orgid: String,
                                    siteid: String,
                                    policyByIdAndVersionRequest: AppRequest): Future[AppResponse] = {
    log.debug(s"Entered the getPolicyByVersionNumberAndId  with orgId: $orgid and siteId: $siteid")
    (policyByIdAndVersionRequest.request.configprops.policyid, policyByIdAndVersionRequest.request.configprops.version) match {
      case (None, _) =>
        log.error(s"policyid not found in request and messageid: ${policyByIdAndVersionRequest.messageid}")
        failurecaseResponse(policyByIdAndVersionRequest, BADREQUEST.id, policyidMissingInReq.value)
      case (_, None) =>
        log.error("version not found in request")
        failurecaseResponse(policyByIdAndVersionRequest, BADREQUEST.id, versionMissingInReq.value)
      case (Some(policyId), Some(version)) =>
        log.debug(s"policyId: $policyId version: $version")
        cassandraReadPolicyIdVersionTimer
          .time(
            dbLayer
              .getPolicyByIdAndVersion(policyId, version, orgid, siteid, productionApi.value)
          )
          .map {
            case Nil =>
              failurecaseResponseWithoutFuture(
                policyByIdAndVersionRequest,
                BADREQUEST.id,
                NotFound_PolicyId_Table.value + policyId + Does_Not_Exist_OrgSite.value + orgid +
                Siteid.value + siteid + " and version number: " + version
              )
            case listOfPC =>
              log.info("Successfully got the policy by policyid and version number " + listOfPC.head)
              successResponseWithoutFuture(policyByIdAndVersionRequest, generatePolicyResponse(listOfPC.head))
          }
    }
  }

  def searchPolicyByNameOrTagProcess(orgid: String,
                                     siteid: String,
                                     policyGetByIdRequest: AppRequest): Future[AppResponse] = {
    log.info(s"Entered searchPolicyByNameOrTagProcess with orgId: $orgid and siteId: $siteid")
    policyGetByIdRequest.request.configprops.searchPayload match {
      case None =>
        log.error(s"Required policyName/tagId missing in request and messageid: ${policyGetByIdRequest.messageid}")
        failurecaseResponse(policyGetByIdRequest, BADREQUEST.id, "Required policyName/tagId missing in request")
      case Some(searchPolicyByNameOrTagid) =>
        searchDBWithNameOrTagid(searchPolicyByNameOrTagid, orgid, siteid).map(listOfPolicies => {
          log.info("Successfully got the policies by name or tagid" + listOfPolicies)
          successResponseWithoutFuture(policyGetByIdRequest,
                                       listOfPolicies.map(policy => generatePolicyResponse(policy)))
        })
    }
  }

  def searchDBWithNameOrTagid(searchPolicyByNameOrTagid: SearchPolicyByNameOrTagid,
                              orgid: String,
                              siteid: String): Future[List[Policy]] =
    searchPolicyByNameOrTagid match {
      case SearchPolicyByNameOrTagid(Some(name), _) =>
        log.debug(s"Name: $name")
        cassandraReadPoliciesNameTimer.time(dbLayer.getAllPoliciesByNameWithinOrg(name, orgid, siteid))
      case SearchPolicyByNameOrTagid(_, Some(tagid)) =>
        log.debug(s"tagid: $tagid")
        cassandraReadPoliciesTagidTimer.time(dbLayer.getAllPoliciesBytagidWithinOrg(tagid, orgid, siteid))
    }

  def getActivePolicyProcess(orgid: String, siteid: String, groupPolicyGetByIdRequest: AppRequest)(
      implicit ec: ExecutionContext
  ): Future[AppResponse] = {
    log.info(s"Entered getActivePolicyProcess with orgId: $orgid and siteId: $siteid")
    groupPolicyGetByIdRequest.request.configprops.parkinggroupid match {
      case None => failurecaseResponse(groupPolicyGetByIdRequest, BADREQUEST.id, NotFound_parkinggroupid_version.value)
      case Some(parkinggroupid) =>
        log.debug(s"parkinggroupid: $parkinggroupid")
        cassandraReadGroupPolicyGroupidLiveTimer
          .time(
            dbLayer
              .getGroupPolicyByGroupId(parkinggroupid)
          )
          .flatMap {
            case None              => failurecaseResponse(groupPolicyGetByIdRequest, BADREQUEST.id, groupNotAssociatedPolicy.value)
            case Some(groupPolicy) => getPolicyByParkingGroupId(orgid, siteid, groupPolicy, groupPolicyGetByIdRequest)
          }
    }
  }

  def getPolicyByParkingGroupId(orgid: String,
                                siteid: String,
                                groupPolicy: ParkingGroupPolicyLink,
                                groupPolicyGetByIdRequest: AppRequest): Future[AppResponse] =
    cassandraReadPolicyIdVersionTimer
      .time(
        dbLayer
          .getPolicyByIdAndVersion(groupPolicy.policyid, groupPolicy.version, orgid, siteid, productionApi.value)
      )
      .map {
        case Nil =>
          failurecaseResponseWithoutFuture(
            groupPolicyGetByIdRequest,
            BADREQUEST.id,
            NotFound_PolicyId_Table.value + groupPolicy.policyid + Does_Not_Exist_OrgSite.value + orgid + Siteid.value + siteid
            + " and version number: " + groupPolicy.version
          )
        case listOfPC =>
          log.info("Successfully got the Active policy by parkinggroupid")
          successResponseWithoutFuture(groupPolicyGetByIdRequest, generatePolicyResponse(listOfPC.head))
      }

  def getActivePoliciesInTimeline(
      policyGetByIdRequest: AppRequest
  ): Future[AppResponse] =
    (policyGetByIdRequest.request.configprops.parkinggroupid,
     policyGetByIdRequest.request.configprops.fromtime,
     policyGetByIdRequest.request.configprops.totime) match {
      case (Some(parkinggroupid), Some(fromtime), Some(totime)) =>
        log.info(s"parkinggroupid: $parkinggroupid fromtime: $fromtime totime: $totime")
        queryDbForActivePoliciesInTimeline(parkinggroupid, fromtime.toLong, totime.toLong, policyGetByIdRequest)
      case _ =>
        failurecaseResponse(policyGetByIdRequest, BADREQUEST.id, timelineRequestMissingFields.value)
    }

  def queryDbForActivePoliciesInTimeline(
      parkinggroupid: String,
      fromtime: Long,
      totime: Long,
      policyGetByIdRequest: AppRequest
  ): Future[AppResponse] =
    cassandraReadGroupPolicyGroupidTimer
      .time(
        dbLayer
          .getAllPoliciesWithGroupId(parkinggroupid)
      )
      .map(
        list =>
          if (list.nonEmpty) {
            log.info("Successfully got the active policies with in timeline by parkinggroupid")
            successResponseWithoutFuture(policyGetByIdRequest, filterTimelinePolicies(list, fromtime, totime))
          } else {
            failurecaseResponseWithoutFuture(policyGetByIdRequest, BADREQUEST.id, groupNotAssociatedPolicy.value)
        }
      )

  def filterTimelinePolicies(parkingGroupPolicyLinklist: List[ParkingGroupPolicyLink],
                             fromtime: Long,
                             totime: Long): List[ActivePolicyResponse] = {
    val sortedOrder = parkingGroupPolicyLinklist.sortBy(_.startTime)
    sortedOrder.foldLeft(List[ActivePolicyResponse]())(
      (a, b) =>
        if (logicToFindPolicyWithinTimeline(fromtime, totime, b.startTime, b.endTime)) {
          ActivePolicyResponse(b.policyid, b.version, convertEpochToUTC(b.startTime), convertEpochToUTC(b.endTime)) :: a
        } else {
          a
      }
    )
  }

  def convertEpochToUTC(epoch: Long): String =
    if (epoch == -1) "" else Instant.ofEpochMilli(epoch).toString

  def logicToFindPolicyWithinTimeline(Queryfromtime: Long,
                                      Querytotime: Long,
                                      linkstarttime: Long,
                                      linkendtime: Long): Boolean =
    (Queryfromtime, Querytotime, linkstarttime, linkendtime) match {
      case x if linkendtime == -1 && linkstarttime <= Querytotime            => true
      case x if linkendtime == -1 && linkstarttime >= Querytotime            => false
      case x if linkstarttime >= Queryfromtime && linkendtime <= Querytotime => true
      case x if linkstarttime <= Queryfromtime && (linkendtime <= Querytotime && linkendtime >= Queryfromtime) =>
        true
      case x if (linkstarttime >= Queryfromtime && linkstarttime <= Querytotime) && linkendtime > Querytotime => true
      case x if linkstarttime < Queryfromtime && linkendtime > Querytotime                                    => true
      case _                                                                                                  => false
    }

  def validateTimeRangesWithinPolicyRule(policyRequest: PolicyRequest): List[String] = {

    val policyRules: Option[Set[PolicyRule]] = policyRequest.policyRule
    policyRules match {
      case Some(policyRules) =>
        policyRules.foldLeft(List[String]())(
          (xs: List[String], x: PolicyRule) => xs ++ validateTimeRangeConflicts(x.parkingSchedule.timeRange, x.name)
        )
      case None => List()
    }
  }

}

object PolicyHelper {
  def apply(dbLayer: DbLayer): PolicyHelper = new PolicyHelper(dbLayer)
}
