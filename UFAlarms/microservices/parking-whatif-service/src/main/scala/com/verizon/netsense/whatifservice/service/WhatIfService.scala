package com.verizon.netsense.whatifservice.service

import java.util.UUID

import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import com.verizon.netsense.whatifservice.config.WhatIfConfigLoader._
import com.verizon.netsense.whatifservice.db.WhatIfDbLayer
import com.verizon.netsense.whatifservice.model.ResponseMessage._
import com.verizon.netsense.whatifservice.model.StatusCodes._
import com.verizon.netsense.whatifservice.model.WhatIfConstants.{apply => _, _}
import com.verizon.netsense.whatifservice.model.casel.{AppFailureResponse, AppRequest, AppResponse, AppSuccessResponse}
import com.verizon.netsense.whatifservice.model.{WhatIfJobDB, _}
import com.verizon.netsense.whatifservice.util.RequestResponseGenerator
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.producer.ProducerRecord
import com.verizon.netsense.whatifservice.model.WhatIfConstants._

import scala.concurrent.{ExecutionContext, Future}

/**
 * What IF Apis
 * Created by Rajitha on 15/02/18.
 */
class WhatIfService(dbLayer: WhatIfDbLayer,
                    reqResGenerator: RequestResponseGenerator,
                    sparkJobRequestAndResponse: SparkJobRESTService)
    extends Logging
    with Instrumented {

  implicit val ec = ExecutionContext.Implicits.global

  private[this] val cassandraPostWhatIfTimer: Timer    = metrics.timer("cassandra-create-whatif")
  private[this] val cassandraReadWhatIfTimer: Timer    = metrics.timer("cassandra-read-whatif")
  private[this] val cassandraReadAllWhatIfTimer: Timer = metrics.timer("cassandra-read-all-whatif")
  private[this] val cassandraUpdateWhatIfTimer: Timer  = metrics.timer("cassandra-update-whatif")
  private[this] val cassandraDeleteWhatIfTimer: Timer  = metrics.timer("cassandra-delete-whatif")

  /**
   * Get What If by jobid
   *
   * @param orgid
   * @param siteid
   * @param whatIfRequest
   * @return
   */
  def getWhatIfByIdProcess(orgid: String,
                           siteid: String,
                           whatIfRequest: AppRequest): Future[(AppResponse, WhatIfSparkRequest)] = {
    log.info(
      s"Entered the getWhatIfByIdProcess to get the whatIf job with orgid: $orgid and siteid: $siteid"
    )
    val jobId = whatIfRequest.request.whatifprops.get.jobid.get
    log.debug(s"jobid: $jobId")
    cassandraReadWhatIfTimer
      .time(
        dbLayer
          .getJobById(orgid, siteid, jobId)
      )
      .map {
        case Some(whatIfJob) =>
          log.info("Successfully got the What if job by jobid " + whatIfJob)
          reqResGenerator.successResponseWithoutFuture(whatIfRequest, WhatIfResponse(whatIfJob))
        case None =>
          reqResGenerator.failureCaseResponseWithoutFuture(
            whatIfRequest,
            NOTFOUND.id,
            NotFound_WhatIfJobId_In_db.value + jobId + Does_Not_Exist_OrgSite.value + orgid + Siteid.value + siteid
          )
      }

  }

  /**
   * Get all What if jobs with in org and site
   *
   * @param orgid
   * @param siteid
   * @param getAllWhatIfRequest
   * @return
   */
  def getAllWhatIfProcess(
      orgid: String,
      siteid: String,
      getAllWhatIfRequest: AppRequest
  ): Future[(AppSuccessResponse[List[WhatIfJobResponse]], WhatIfSparkRequest)] = {
    log.info(s"Entered  getAllWhatIfProcess with orgid: $orgid and siteid: $siteid")
    dbLayer
      .getAllJobs(orgid = orgid, siteid = siteid)
      .map(listOfWhatIf => {
        log.info("Successfully got the What If jobs for siteid and orgid")
        reqResGenerator.successResponseWithoutFuture(
          getAllWhatIfRequest,
          listOfWhatIf.map(whatIfJob => WhatIfResponse(whatIfJob))
        )
      })
  }

  def postCassandraDB(jobStatusObject: WhatIfJob, whatIfRequest: AppRequest, whatIfSparkRequest: WhatIfSparkRequest) =
    cassandraPostWhatIfTimer.time(
      dbLayer
        .storeJobStatus(jobStatusObject)
        .map(
          {
            case true =>
              log.info("Successfully persisted whatIf job in cassandra " + jobStatusObject)
              reqResGenerator.successResponseWithoutFuture(
                whatIfRequest,
                WhatIfResponse(jobStatusObject),
                whatIfSparkRequest
              )
            case false =>
              log.debug("Failed to store WhatIf job in Cassandra " + jobStatusObject)
              reqResGenerator.failureCaseResponseWithoutFuture(whatIfRequest, BADREQUEST.id, NotFound_PolicyCatId.value)
          }
        )
    )

  def PostSparkAndUpdateDB(
      orgid: String,
      siteid: String,
      whatIfRequest: AppRequest,
      whatIfFromUser: WhatIfJobRequest,
      validatedPolicyTupleList: (List[String], List[String])
  ): Future[(AppResponse with Product with Serializable, WhatIfSparkRequest)] = {
    val jobStatusObject = WhatIfJobDB(
      whatIfFromUser,
      whatIfRequest.request.orgprops.orgid,
      whatIfRequest.request.siteprops.siteid,
      createdbyuserid = whatIfRequest.request.user.getOrElse(""),
      jobId = UUID.randomUUID().toString,
      jobstatus = Pending.value,
      endtime = Option(0),
      batchid = 0,
      parkingPoliciesWithVersion = validatedPolicyTupleList._1,
      userEmail = whatIfRequest.request.userprops.userEmail
    )

    val whatIfSparkRequest = WhatIfSparkRequest(
      _requestSpark = true,
      jobStatusObject.jobid,
      orgid,
      siteid,
      jobStatusObject.parkinggroups.getOrElse(List("")),
      validatedPolicyTupleList._1,
      jobStatusObject.fromtime.getOrElse(0),
      jobStatusObject.totime.getOrElse(0),
      jobStatusObject.mode.getOrElse(""),
      jobStatusObject.intervalperiod.getOrElse(""),
      jobStatusObject.email.getOrElse("")
    )
    postCassandraDB(jobStatusObject, whatIfRequest, whatIfSparkRequest)
  }

  def InvalidPoliciesResponse(
      whatIfUpdateRequest: AppRequest,
      orgid: String,
      siteid: String,
      validatedPolicyTupleList: (List[String], List[String])
  ): Future[(AppFailureResponse, WhatIfSparkRequest)] = {
    log.debug(s"Following Policies provided by user are invalid  ${validatedPolicyTupleList._2}")
    reqResGenerator
      .failureCaseResponse(
        whatIfUpdateRequest,
        BADREQUEST.id,
        Parking_Policies.value + (validatedPolicyTupleList._2 mkString ",") + All_Invalid_Jobs.value + orgid
        + "," + Siteid.value + siteid
      )
  }

  def ValidatePoliciesAndPostToDB(
      orgid: String,
      siteid: String,
      whatIfRequest: AppRequest,
      whatIfFromUser: WhatIfJobRequest
  ): Future[(AppResponse with Product with Serializable, WhatIfSparkRequest)] =
    validatePolicies(whatIfFromUser.parkingPolicies.distinct, orgid, siteid)
      .flatMap(validatedPolicyTupleList => {
        if (validatedPolicyTupleList._2.nonEmpty) {
          InvalidPoliciesResponse(whatIfRequest: AppRequest, orgid, siteid, validatedPolicyTupleList)
        } else {
          PostSparkAndUpdateDB(orgid, siteid, whatIfRequest, whatIfFromUser, validatedPolicyTupleList)
        }
      })

  def postJobStatusProcess(orgid: String,
                           siteid: String,
                           whatIfRequest: AppRequest): Future[(AppResponse, WhatIfSparkRequest)] = {

    log.info(s"Entered postJobStatusProcess with orgid: $orgid and siteid: $siteid ")

    val whatIfFromUser = whatIfRequest.request.whatifprops.get.whatIfJobRequest.get

    log.debug(s"whatIfFromUser: $whatIfFromUser")

    dbLayer
      .whatIfExistWithSameName(orgid, siteid, whatIfFromUser.name)
      .flatMap(
        exist =>
          if (exist) {
            log.info(s"What If already exist with name ${whatIfFromUser.name}")
            reqResGenerator
              .failureCaseResponse(whatIfRequest, BADREQUEST.id, WhatIf_Name_Exist.value + whatIfFromUser.name)
          } else {
            ValidatePoliciesAndPostToDB(orgid, siteid, whatIfRequest, whatIfFromUser)
        }
      )

  }

  def updateCompletedJob(updatedWhatIf: WhatIfJob,
                         whatIfUpdateRequest: AppRequest,
                         whatIfFromDB: WhatIfJob,
                         whatIfFromUser: WhatIfJobRequest): Future[(AppResponse, WhatIfSparkRequest)] = {

    val status = !(updatedWhatIf.mode.equals(whatIfFromDB.mode) & updatedWhatIf.parkinggroups
      .equals(whatIfFromDB.parkinggroups) &
    reqResGenerator
      .getPolicyListWithOutVersion(updatedWhatIf.policieswithversion.getOrElse(List("")))
      .equals(reqResGenerator.getPolicyListWithOutVersion(whatIfFromDB.policieswithversion.getOrElse(List(""))))
    & updatedWhatIf.fromtime
      .equals(whatIfFromDB.fromtime) &
    updatedWhatIf.totime.equals(whatIfFromDB.totime))
    status match {
      case true =>
        callSparkAndUpdateDB(whatIfUpdateRequest, whatIfFromUser, updatedWhatIf, whatIfFromDB)
      case false =>
        updateCassandra(updatedWhatIf, whatIfUpdateRequest)
    }
  }

  def updateAbortedOrAbnormallyEndedJob(updatedWhatIf: WhatIfJob,
                                        whatIfUpdateRequest: AppRequest,
                                        whatIfFromDB: WhatIfJob,
                                        whatIfFromUser: WhatIfJobRequest) =
    callSparkAndUpdateDB(whatIfUpdateRequest, whatIfFromUser, updatedWhatIf, whatIfFromDB)

  def validatePolicies(userPolicyIdList: List[String],
                       orgid: String,
                       siteid: String): Future[(List[String], List[String])] = {
    val dbPolicyIdFutureList =
      dbLayer.getAllPolicies(orgid, siteid).map(policies => policies.map(policy => (policy.policyid, policy.version)))
    dbPolicyIdFutureList.map(dbPolicyTupleList => {
      val validationResult =
        userPolicyIdList.foldLeft((List[(String, Int)](), List[String]()))((acc, policyIdByUser) => {
          dbPolicyTupleList.find(dpPolicyTuple => dpPolicyTuple._1 == policyIdByUser) match {
            case Some(policyVersionTuple) => (acc._1 :+ policyVersionTuple, acc._2)
            case None                     => (acc._1, acc._2 :+ policyIdByUser)
          }
        })
      val validPolicies: List[String]   = validationResult._1.map(e => e._1 + ":" + e._2)
      val invalidPolicies: List[String] = validationResult._2
      (validPolicies, invalidPolicies)
    })
  }

  def validatePoliciesAndUpdateWhatIfJob(whatIfUpdateRequest: AppRequest,
                                         whatIfFromDB: WhatIfJob,
                                         whatIfFromUser: WhatIfJobRequest): Future[(AppResponse, WhatIfSparkRequest)] =
    validatePolicies(whatIfFromUser.parkingPolicies.distinct,
                     whatIfFromDB.orgid,
                     whatIfFromDB.siteid)
      .flatMap(validatedPolicyTupleList => {
        if (validatedPolicyTupleList._2.nonEmpty) {
          InvalidPoliciesResponse(whatIfUpdateRequest,
                                  whatIfFromDB.orgid,
                                  whatIfFromDB.siteid,
                                  validatedPolicyTupleList)
        } else {
          val updatedWhatIf = WhatIfJobDB(
            whatIfFromUser,
            whatIfFromDB.orgid,
            whatIfFromDB.siteid,
            jobId = whatIfFromDB.jobid,
            createdbyuserid = whatIfFromDB.createdbyuserid.getOrElse(""),
            createdat = whatIfFromDB.createdat,
            lastupdatedbyuserid = whatIfUpdateRequest.request.user,
            updatedat = Some(System.currentTimeMillis()),
            jobstatus = whatIfFromDB.jobstatus.getOrElse(""),
            endtime = whatIfFromDB.endtime,
            batchid = whatIfFromDB.batchid.getOrElse(0),
            parkingPoliciesWithVersion = validatedPolicyTupleList._1,
            userEmail = whatIfUpdateRequest.request.userprops.userEmail,
            submittime= whatIfFromDB.submittime,
            starttime=whatIfFromDB.starttime
          )

          whatIfFromDB.jobstatus.getOrElse("").toLowerCase match {
            case Completed.value =>
              updateCompletedJob(updatedWhatIf, whatIfUpdateRequest, whatIfFromDB, whatIfFromUser)
            case Aborted.value | AbnormallyEnded.value =>
              updateAbortedOrAbnormallyEndedJob(updatedWhatIf, whatIfUpdateRequest, whatIfFromDB, whatIfFromUser)
            case _ =>
              reqResGenerator
                .failureCaseResponse(whatIfUpdateRequest, INTERNALSERVERERROR.id, Inavlid_Job_Status.value)
          }
        }
      })

  def updateDBAndGenerateResponse(whatIfUpdateRequest: AppRequest,
                                  whatIfFromDB: WhatIfJob,
                                  whatIfFromUser: WhatIfJobRequest): Future[(AppResponse, WhatIfSparkRequest)] =
    dbLayer
      .whatIfExistWithSameName(whatIfFromDB.orgid,
                               whatIfFromDB.siteid,
                               whatIfFromUser.name,
                               whatIfFromDB.name)
      .flatMap(
        exist =>
          if (exist) {
            log.info(s"What If already exists with name ${whatIfFromUser.name}")
            reqResGenerator
              .failureCaseResponse(whatIfUpdateRequest, BADREQUEST.id, WhatIf_Name_Exist.value + whatIfFromUser.name)
          } else if (whatIfFromDB.jobstatus.getOrElse("").equalsIgnoreCase(Pending.value) | whatIfFromDB.jobstatus
                       .getOrElse("")
                       .equalsIgnoreCase(Running.value)) {
            log.debug(s"What If Job cannot be edited, as job status is ${whatIfFromDB.jobstatus}")
            reqResGenerator
              .failureCaseResponse(
                whatIfUpdateRequest,
                BADREQUEST.id,
                Abort_Job.value + Job_Current.value + whatIfFromDB.jobstatus.getOrElse("") + status.value
              )
          } else {
            validatePoliciesAndUpdateWhatIfJob(whatIfUpdateRequest, whatIfFromDB, whatIfFromUser)
        }
      )

  def updateWhatIfProcess(orgid: String,
                          siteid: String,
                          whatIfUpdateRequest: AppRequest): Future[(AppResponse, WhatIfSparkRequest)] = {
    log.info(s"Entered updateWhatIfProcess with orgid: $orgid and siteid: $siteid ")
    val whatIfProps = whatIfUpdateRequest.request.whatifprops.get

    val jobId  = whatIfProps.jobid.get
    val whatIf = whatIfProps.whatIfJobRequest.get

    log.debug(s"JobId: $jobId whatIf: $whatIf")
    cassandraReadWhatIfTimer
      .time(
        dbLayer
          .getJobById(orgid, siteid, jobId)
      )
      .flatMap {
        case None =>
          log.info(s"Could not find whatIf in DB with id $jobId")
          reqResGenerator.failureCaseResponse(
            whatIfUpdateRequest,
            NOTFOUND.id,
            Missing_WhatIfJobId.value + jobId + Does_Not_Exist_OrgSite.value + orgid + Siteid.value + siteid
          )
        case Some(whatIfJob) =>
          updateDBAndGenerateResponse(whatIfUpdateRequest, whatIfJob, whatIf)
      }

  }

  def callSparkAndUpdateDB(whatIfUpdateRequest: AppRequest,
                           whatIfFromUser: WhatIfJobRequest,
                           updatedWhatIf: WhatIfJob,
                           whatIfFromDB: WhatIfJob): Future[(AppResponse, WhatIfSparkRequest)] = {

    val whatIfSparkRequest = WhatIfSparkRequest(
      _requestSpark = true,
      updatedWhatIf.jobid,
      updatedWhatIf.orgid,
      updatedWhatIf.siteid,
      updatedWhatIf.parkinggroups.getOrElse(List("")),
      updatedWhatIf.policieswithversion.getOrElse(List("")),
      updatedWhatIf.fromtime.getOrElse(0),
      updatedWhatIf.totime.getOrElse(0),
      updatedWhatIf.mode.getOrElse(""),
      updatedWhatIf.intervalperiod.getOrElse(""),
      whatIfUpdateRequest.request.userprops.userEmail.getOrElse("")
    )

    val DBupdatedWhatIf = WhatIfJobDB(
      whatIfFromUser,
      updatedWhatIf.orgid,
      updatedWhatIf.siteid,
      jobId = whatIfFromDB.jobid,
      createdbyuserid = whatIfFromDB.createdbyuserid.getOrElse(""),
      createdat = whatIfFromDB.createdat,
      lastupdatedbyuserid = whatIfUpdateRequest.request.user,
      updatedat = Some(System.currentTimeMillis()),
      jobstatus = Pending.value,
      endtime = Option(0),
      batchid = 0,
      parkingPoliciesWithVersion = updatedWhatIf.policieswithversion.getOrElse(List("")),
      userEmail = whatIfUpdateRequest.request.userprops.userEmail
    )
    updateCassandra(DBupdatedWhatIf, whatIfUpdateRequest).map(result => (result._1, whatIfSparkRequest))
  }

  def updateCassandra(updatedWhatIf: WhatIfJob,
                      whatIfUpdateRequest: AppRequest): Future[(AppResponse, WhatIfSparkRequest)] =
    cassandraUpdateWhatIfTimer
      .time(
        dbLayer.updateByJobId(updatedWhatIf.orgid,
                              updatedWhatIf.siteid,
                              updatedWhatIf.jobid,
                              updatedWhatIf)
      )
      .map(
        s =>
          if (s.success) {
            log.info("Successfully updated the whatIf in cassandra " + updatedWhatIf)
            val whatIfResponse = WhatIfResponse(updatedWhatIf)
            reqResGenerator.successResponseWithoutFuture(whatIfUpdateRequest, whatIfResponse)
          } else {
            log.debug(s"Failed to updated the whatif with jobid ${updatedWhatIf.jobid}")
            reqResGenerator.failureCaseResponseWithoutFuture(whatIfUpdateRequest,
                                                             INTERNALSERVERERROR.id,
                                                             InternalError_UpdateWhatIf.value)
        }
      )

  def searchWhatIf(orgid: String,
                   siteid: String,
                   whatIfIdRequest: AppRequest): Future[(AppResponse, WhatIfSparkRequest)] = {
    log.info(s"Entered the searchWhatIf to get the what if with orgid: $orgid and siteid: $siteid")
    whatIfIdRequest.request.whatifprops match {
      case Some(whatIfProps) =>
        whatIfProps.search match {
          case Some(search) =>
            log.debug(s"searchcondition: $search")
            validateFilterCondition(search, "eq") match {
              case None =>
                log.error(s"Validation failed for search string $search")
                reqResGenerator.failureCaseResponse(whatIfIdRequest, NOTFOUND.id, condition_correct.value)
              case Some(filterString) =>
                val searchList = filterString.split(" eq ").toList
                if (isValidKey(searchList.headOption))
                  dbLayer
                    .searchWhatIf(
                      orgid,
                      siteid,
                      searchList.head,
                      searchList.last
                    )
                    .map(filteredWhatIfList => {
                      log.debug("Successfully got the Whatif list")
                      reqResGenerator.successResponseWithoutFuture(
                        whatIfIdRequest,
                        filteredWhatIfList.map(whatIfResponse => WhatIfResponse(whatIfResponse))
                      )
                    })
                else {
                  reqResGenerator.failureCaseResponse(whatIfIdRequest, BADREQUEST.id, invalid_key.value)
                }
            }
          case None =>
            log.error(s"Search payload is missing in the appRequest $whatIfIdRequest")
            reqResGenerator.failureCaseResponse(whatIfIdRequest, NOTFOUND.id, Missing_Searchstring.value)
        }

      case None =>
        log.error(s"WhatIfprops is empty in the appRequest $whatIfIdRequest")
        reqResGenerator.failureCaseResponse(whatIfIdRequest, NOTFOUND.id, Missing_WhatIfprops.value)
    }
  }

  /**
   * This method used to delete whatif job
   * Enable calling spark in phase 3
   */
  def deleteWhatIfProcess(orgid: String,
                          siteid: String,
                          whatIfDeleteRequest: AppRequest): Future[(AppResponse, WhatIfSparkRequest)] = {
    log.info(s"Entered  deleteWhatIfProcess with orgid: $orgid and siteid: $siteid")
    whatIfDeleteRequest.request.whatifprops match {
      case Some(whatIfProps) =>
        whatIfProps.jobid match {
          case None =>
            log.error("jobid not found in request")
            reqResGenerator.failureCaseResponse(whatIfDeleteRequest, NOTFOUND.id, Missing_WhatIfJobId.value)
          case Some(jobId) =>
            log.debug(s"jobid: $jobId")
            cassandraReadWhatIfTimer
              .time(
                dbLayer
                  .getJobById(orgid, siteid, jobId)
              )
              .flatMap {
                case None =>
                  log.info(s"Could not find WhatIf in DB with id $jobId")
                  reqResGenerator.failureCaseResponse(
                    whatIfDeleteRequest,
                    NOTFOUND.id,
                    Missing_WhatIfJobId.value + jobId + Does_Not_Exist_OrgSite.value + orgid + Siteid.value + siteid
                  )
                case _ =>
                  cassandraDeleteWhatIfTimer
                    .time(
                      dbLayer
                        .deleteByJobId(orgid, siteid, jobId)
                    )
                    .map(
                      a =>
                        if (a.success) {
                          log.info(s"Successfully deleted the whatif by jobid $jobId")
                          reqResGenerator.successResponseWithoutFuture(whatIfDeleteRequest, a)
                        } else {
                          log.error(s"Error deleting the whatif in cassandra with id $jobId")
                          reqResGenerator.failureCaseResponseWithoutFuture(
                            whatIfDeleteRequest,
                            INTERNALSERVERERROR.id,
                            Invalid_Delete_Status.value + jobId
                          )
                      }
                    )
              }
        }
      case None =>
        log.error(s"WhatIfProps is empty in the appRequest $whatIfDeleteRequest")
        reqResGenerator.failureCaseResponse(whatIfDeleteRequest, NOTFOUND.id, Missing_WhatIfprops.value)
    }
  }

  /**
   * This method used to abort whatif job
   * Enable calling spark in phase 3
   */
  def abortWhatIfProcess(orgid: String,
                         siteid: String,
                         whatIfAbortRequest: AppRequest): Future[(AppResponse, WhatIfSparkRequest)] = {
    log.info(s"Entered  abortWhatIfProcess with orgid: $orgid and siteid: $siteid")
    val jobId = whatIfAbortRequest.request.whatifprops.get.jobid.get
    cassandraReadWhatIfTimer
      .time(
        dbLayer
          .getJobById(orgid, siteid, jobId)
      )
      .flatMap {
        case Some(whatIfJob) =>
          abortSparkCall(whatIfJob.appid.getOrElse(""))(whatIfAbortRequest)
        case None =>
          reqResGenerator.failureCaseResponse(
            whatIfAbortRequest,
            NOTFOUND.id,
            NotFound_WhatIfJobId_In_db.value + jobId + Does_Not_Exist_OrgSite.value + orgid + Siteid.value + siteid
          )
      }
  }

  def abortSparkCall(appId: String)(appRequest: AppRequest): Future[(AppResponse, WhatIfSparkRequest)] =
    sparkJobRequestAndResponse
      .getStatusCode(appId)
      .map(
        {
          case Some(statusCode) =>
            if (statusCode == 200)
              reqResGenerator.successResponseWithoutFuture(appRequest, SuccessMessage(true))
            else
              reqResGenerator.failureCaseResponseWithoutFuture(appRequest, NOTFOUND.id, AppIdNotFound.value + appId)
          case None =>
            reqResGenerator.failureCaseResponseWithoutFuture(appRequest, NOTFOUND.id, AppIdNotFound.value + appId)

        }
      )

  //checking the condition for format key eq value and now operator is just eq but in future it could be anything
  def validateFilterCondition(filterString: String, operatorKey: String): Option[String] = {
    val stringPattern = (patternPrefix + operatorKey + patternSuffix).r
    stringPattern.findFirstIn(filterString)
  }

  lazy val patternPrefix = "(^\\w+)\\s+"
  lazy val patternSuffix = "(\\s+.*)+"

  // checking key is valid one or notstringPattern
  def isValidKey(key: Option[String]): Boolean =
    key match {
      case Some(value) =>
        Set(name.value, fromTime.value, toTime.value).contains(value.trim)
      case None => false
    }

  def buildProducerRecord(messageid: String)(sqe: Any): ProducerRecord[Array[Byte], Array[Byte]] =
    new ProducerRecord[Array[Byte], Array[Byte]](sparkTopicName, messageid.getBytes, sqe.toString.getBytes)

}

object WhatIfService {
  def apply(dbLayer: WhatIfDbLayer,
            reqResGenerator: RequestResponseGenerator,
            sparkJobRequestAndResponse: SparkJobRESTService): WhatIfService =
    new WhatIfService(dbLayer, reqResGenerator, sparkJobRequestAndResponse)
}
