package com.verizon.netsense.whatifservice.model

import java.io.Serializable
import java.time.Instant
import java.util.UUID

import com.verizon.netsense.whatifservice.model.WhatIfConstants.EntirePeriod

/**
 * Created by maleva on 3/22/18.
 */
case class WhatIfJob(createdbyuserid: Option[String] = None,
                     lastupdatedbyuserid: Option[String] = None,
                     createdat: Option[Long] = None,
                     updatedat: Option[Long] = None,
                     orgid: String="",
                     siteid: String="",
                     jobid: String="",
                     name: Option[String] = None,
                     fromtime: Option[Long] = None,
                     totime: Option[Long] = None,
                     parkinggroups: Option[List[String]]=None,
                     starttime: Option[Long] = None,
                     endtime: Option[Long] = None,
                     submittime: Option[Long] = None,
                     aborttime: Option[Long] = None,
                     jobstatus: Option[String] = None,
                     batchid: Option[Int] = None,
                     appid: Option[String] = None,
                     mode: Option[String] = None,
                     additionalmessage: Option[String] = None,
                     policieswithversion: Option[List[String]]=None,
                     email: Option[String] = None,
                     intervalperiod: Option[String] = None)

case class WhatIfJobRequest(jobId: Option[String],
                            name: String,
                            fromTime: String,
                            toTime: String,
                            parkingPolicies: List[String],
                            parkingGroups: List[String],
                            mode: String,
                            intervalPeriod: Option[String] = None,
                            userEmail: Option[String] = None)

case class WhatIfJobResponse(createdByUserId: String,
                             createdByUserName: String,
                             lastUpdatedByUserId: String,
                             lastUpdatedByUserName: String,
                             createdAt: String,
                             lastUpdatedAt: String,
                             orgId: String,
                             siteId: String,
                             jobId: String,
                             name: String,
                             fromTime: String,
                             toTime: String,
                             parkingPolicies: List[String],
                             parkingGroups: List[String],
                             startTime: String,
                             endTime: String,
                             submitTime: String,
                             abortTime: String,
                             jobStatus: String,
                             timeToComplete: Option[Long] = None,
                             percentageComplete: Option[Double] = None,
                             mode: String,
                             additionalMessage: String)

object WhatIfResponse {
  def apply(jobStatusObject: WhatIfJob, invalidPoliciesList: Option[List[String]] = None): WhatIfJobResponse =
    WhatIfJobResponse(
      createdByUserId = jobStatusObject.createdbyuserid.getOrElse(""),
      createdByUserName = "",
      lastUpdatedByUserId = jobStatusObject.lastupdatedbyuserid.getOrElse(""),
      lastUpdatedByUserName = "",
      createdAt = convertEpochToDate(jobStatusObject.createdat), // ?
      lastUpdatedAt = convertEpochToDate(jobStatusObject.updatedat),
      orgId = jobStatusObject.orgid,
      siteId = jobStatusObject.siteid,
      jobId = jobStatusObject.jobid,
      name = jobStatusObject.name.getOrElse(""),
      fromTime = convertEpochToDate(jobStatusObject.fromtime),
      toTime = convertEpochToDate(jobStatusObject.totime),
      parkingGroups = jobStatusObject.parkinggroups.getOrElse(List("")),
      parkingPolicies = getPolicyListWithOutVersion(jobStatusObject.policieswithversion.getOrElse(List(""))),
      startTime = convertEpochToDate(jobStatusObject.starttime),
      endTime = convertEpochToDate(jobStatusObject.endtime),
      submitTime = convertEpochToDate(jobStatusObject.submittime),
      abortTime = convertEpochToDate(jobStatusObject.aborttime),
      jobStatus = jobStatusObject.jobstatus.getOrElse(""),
      mode = jobStatusObject.mode.getOrElse(""),
      additionalMessage = jobStatusObject.additionalmessage.getOrElse("")
    )
  def convertEpochToDate(epoch: Long) =
    if (epoch == 0) "" else Instant.ofEpochMilli(epoch).toString

  def convertEpochToDate(epoch: Option[Long]) =
    if (epoch.isEmpty | epoch == Some(0)) "" else Instant.ofEpochMilli(epoch.get).toString

  def getPolicyListWithOutVersion(policyListWithVersion: List[String]): List[String] =
    policyListWithVersion
      .map(eachPolicyWithVersion => eachPolicyWithVersion.split(":").toList)
      .map(policy => policy.head)

}

object WhatIfJobDB {
  def apply(jobStatusRequest: WhatIfJobRequest,
            orgid: String,
            siteid: String,
            jobId: String,
            createdbyuserid: String,
            createdat: Option[Long] = None,
            lastupdatedbyuserid: Option[String] = None,
            updatedat: Option[Long] = None,
            jobstatus: String,
            appid: Option[String] = None,
            endtime: Option[Long],
            submittime: Option[Long]=None,
            batchid: Int,
            aborttime: Option[Long] = None,
            parkingPoliciesWithVersion: List[String],
            starttime:Option[Long] = None,
            userEmail: Option[String] = None): WhatIfJob =
    WhatIfJob(
      createdbyuserid = buildStringOption(Option(createdbyuserid)),
      lastupdatedbyuserid = buildStringOption(lastupdatedbyuserid),
      updatedat = buildLongOption(updatedat),
      createdat = buildCreatedTime(createdat),
      orgid = orgid,
      siteid = siteid,
      jobid = jobId,
      name = buildStringOption(Option(jobStatusRequest.name)),
      fromtime = convertIsoToEpoch(jobStatusRequest.fromTime),
      totime = convertIsoToEpoch(jobStatusRequest.toTime),
      parkinggroups = Option(jobStatusRequest.parkingGroups),
      policieswithversion = Option(parkingPoliciesWithVersion),
      starttime = buildLongOption(starttime),
      endtime = endtime,
      submittime = buildCreatedTime(submittime),
      jobstatus = Option(jobstatus),
      batchid = Option(batchid),
      appid = buildStringOption(appid),
      mode = buildStringOption(Option(jobStatusRequest.mode)),
      aborttime = buildLongOption(aborttime),
      intervalperiod = Some(jobStatusRequest.intervalPeriod.getOrElse(EntirePeriod.value)),
      email = userEmail,
      additionalmessage = Option("")
    )

  def getCurrentTime: Long = System.currentTimeMillis()

  def buildCreatedTime(_createdon: Option[Long]) =
    _createdon match {
      case Some(created) => Some(created)
      case None          => Some(getCurrentTime)
    }

  def buildLongOption(inputValue: Option[Long]) =
    inputValue match {
      case Some(outPutValue) => Some(outPutValue)
      case None              => Some(0.toLong)
    }

  def buildStringOption(inputValue: Option[String]) =
    inputValue match {
      case Some(outPutValue) => Some(outPutValue)
      case None              => Some("")
    }

  def convertIsoToEpoch(time: String): Option[Long] =
    Option(Instant.parse(time).toEpochMilli)
}

case class SuccessMessage(success: Boolean)
