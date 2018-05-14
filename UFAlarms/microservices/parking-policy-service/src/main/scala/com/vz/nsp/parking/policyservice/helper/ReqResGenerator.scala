package com.vz.nsp.parking.policyservice.helper

import java.time.Instant
import java.util.{Calendar, UUID}

import com.vz.nsp.parking.model.CaselType._
import com.vz.nsp.parking.model._

import scala.concurrent.{ExecutionContext, Future}

object ReqResGenerator {

  def failurecaseResponse(tagGetByIdRequest: AppRequest, status: Int, error: String)(
      implicit ec: ExecutionContext
  ): Future[AppFailureResponse] =
    Future.successful(generateAppFailureResponse(tagGetByIdRequest, status, error))

  def generateAppFailureResponse(tagGetByIdRequest: AppRequest, status: Int, error: String)(
      implicit ec: ExecutionContext
  ): AppFailureResponse =
    AppFailureResponse(
      tagGetByIdRequest.messageid,
      FailureResponseBody(tagGetByIdRequest.request.requestid, Instant.now.toString, false, error, status)
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

  def failurecaseResponseWithoutFuture(tagGetByIdRequest: AppRequest, status: Int, error: String)(
      implicit ec: ExecutionContext
  ): AppFailureResponse =
    generateAppFailureResponse(tagGetByIdRequest, status, error)

  def successResponseWithoutFuture[T](headers: AppRequest,
                                      resultBody: T)(implicit ec: ExecutionContext): AppSuccessResponse[T] =
    AppSuccessResponse[T](headers.messageid,
                          ResponseBody(headers.request.requestid, Instant.now.toString, true, resultBody))

  def generatePolicyObject(policyFromClient: PolicyRequest,
                           orgid: String,
                           siteid: String,
                           userId: Option[String] = None): Policy =
    Policy(
      uid = UUID.randomUUID().toString,
      description = policyFromClient.description,
      policyAuthorizerid = userId.getOrElse(""),
      siteid = siteid,
      orgid = orgid,
      tags = Set(),
      policyLevelViolations = policyFromClient.policyLevelViolations.getOrElse(Set()),
      isLibraryPolicy = false,
      hashValue = "",
      createdOn = Calendar.getInstance().getTimeInMillis,
      lastUpdated = 0,
      isDeleted = false,
      version = 1,
      name = policyFromClient.name,
      timeZone = policyFromClient.timeZone.getOrElse(""),
      state = Some(unassignedValue.value),
      policyRule = addUUIDToPolicyruleset(policyFromClient.policyRule.getOrElse(Set()))
    )

  def generatePolicyResponse(policy: Policy): PolicyResponse =
    PolicyResponse(
      policyId = policy.uid,
      description = policy.description,
      userId = policy.policyAuthorizerid,
      siteId = policy.siteid,
      orgId = policy.orgid,
      tags = policy.tags,
      policyLevelViolations = policy.policyLevelViolations,
      isLibraryPolicy = policy.isLibraryPolicy,
      hashValue = policy.hashValue,
      createdOn = convertEpochToDate(policy.createdOn),
      lastUpdated = convertEpochToDate(policy.lastUpdated),
      version = policy.version,
      name = policy.name,
      timeZone = policy.timeZone,
      state = policy.state,
      policyRule = policy.policyRule
    )

  def generatePolicyResponse(policy: Policy,
                             whatIfJobs: List[WhatIfJob] = List(),
                             associatedParkingGroups: Option[List[String]] = None): PolicyResponse = {

    val whatIfJobDetails: Option[List[WhatIfJobDetails]] =
      if (whatIfJobs.isEmpty) None
      else Some(whatIfJobs.map(job => WhatIfJobDetails(job.jobid, job.jobstatus.getOrElse(""))))
    PolicyResponse(
      policyId = policy.uid,
      description = policy.description,
      userId = policy.policyAuthorizerid,
      siteId = policy.siteid,
      orgId = policy.orgid,
      tags = policy.tags,
      policyLevelViolations = policy.policyLevelViolations,
      isLibraryPolicy = policy.isLibraryPolicy,
      hashValue = policy.hashValue,
      createdOn = convertEpochToDate(policy.createdOn),
      lastUpdated = convertEpochToDate(policy.lastUpdated),
      version = policy.version,
      name = policy.name,
      timeZone = policy.timeZone,
      state = policy.state,
      policyRule = policy.policyRule,
      whatIfJob = whatIfJobDetails,
      associatedParkingGroups = associatedParkingGroups
    )
  }

  def convertEpochToDate(epoch: Long): String =
    if (epoch == 0) "" else Instant.ofEpochMilli(epoch).toString

  def addUUIDToPolicyruleset(policyRuleSet: Set[PolicyRule]): Set[PolicyRule] =
    policyRuleSet.map(pr => pr.copy(policyRuleId = Some(UUID.randomUUID().toString)))

  def addRuleIdsToPolicyRules(userPolicyrules: Set[PolicyRule], dbPolicyrules: Set[PolicyRule]): Set[PolicyRule] = {
    val partionedLists: (Set[PolicyRule], Set[PolicyRule]) =
      userPolicyrules.partition(policyrule => dbPolicyrules.contains(policyrule))
    partionedLists._1 union addUUIDToPolicyruleset(partionedLists._2)
  }

  def updatePolicyObject(policyFromDB: Policy, policyFromClient: PolicyRequest, userId: Option[String] = None): Policy =
    Policy(
      uid = policyFromDB.uid,
      policyAuthorizerid = userId.getOrElse(""),
      siteid = policyFromDB.siteid,
      orgid = policyFromDB.orgid,
      tags = policyFromDB.tags,
      policyLevelViolations = policyFromClient.policyLevelViolations.getOrElse(Set()),
      isLibraryPolicy = policyFromDB.isLibraryPolicy,
      hashValue = policyFromDB.hashValue,
      createdOn = policyFromDB.createdOn,
      lastUpdated = Calendar.getInstance().getTimeInMillis,
      isDeleted = false,
      version = policyFromDB.version + 1,
      name = policyFromClient.name,
      description = policyFromClient.description,
      timeZone = policyFromClient.timeZone.getOrElse(""),
      state = policyFromDB.state,
      policyRule = addRuleIdsToPolicyRules(policyFromClient.policyRule.getOrElse(Set()), policyFromDB.policyRule)
    )

  def generateTagObject(tagFromUser: TagRequest, orgid: String, siteid: String): Tag =
    Tag(
      uid = UUID.randomUUID().toString,
      name = tagFromUser.name,
      description = tagFromUser.description.getOrElse(""),
      orgid = orgid,
      siteid = siteid,
      createdon = Calendar.getInstance().getTimeInMillis,
      lastupdated = 0,
      isdeleted = false
    )

  def updateTagObject(tagFromUser: TagRequest, tagFromDB: Tag): Tag =
    Tag(
      uid = tagFromDB.uid,
      name = tagFromUser.name,
      description = tagFromUser.description.getOrElse(""),
      orgid = tagFromDB.orgid,
      siteid = tagFromDB.siteid,
      createdon = tagFromDB.createdon,
      lastupdated = Calendar.getInstance().getTimeInMillis,
      isdeleted = false
    )

  def associateTagToPolicyObject(policyFromDB: Policy, tagsid: Set[String]): Policy =
    Policy(
      uid = policyFromDB.uid,
      policyAuthorizerid = policyFromDB.policyAuthorizerid,
      siteid = policyFromDB.siteid,
      orgid = policyFromDB.orgid,
      tags = tagsid,
      policyLevelViolations = policyFromDB.policyLevelViolations,
      isLibraryPolicy = policyFromDB.isLibraryPolicy,
      hashValue = policyFromDB.hashValue,
      createdOn = policyFromDB.createdOn,
      lastUpdated = Calendar.getInstance().getTimeInMillis,
      isDeleted = false,
      version = policyFromDB.version,
      name = policyFromDB.name,
      description = policyFromDB.description,
      timeZone = policyFromDB.timeZone,
      state = policyFromDB.state,
      policyRule = policyFromDB.policyRule
    )
}
