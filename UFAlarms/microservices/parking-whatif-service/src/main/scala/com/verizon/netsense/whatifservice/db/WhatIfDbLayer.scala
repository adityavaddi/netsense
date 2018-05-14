package com.verizon.netsense.whatifservice.db

import com.verizon.netsense.utils.Logging
import com.verizon.netsense.whatifservice.model.{Policy, SuccessMessage, WhatIfJob}
import com.verizon.netsense.whatifservice.exceptions.CustomExceptions.WhatifCassandraDBException
import com.verizon.netsense.whatifservice.model.WhatIfConstants.{JobId, name}

import scala.concurrent.{ExecutionContext, Future}

/**
 * Created by maleva on 3/17/18.
 */
class WhatIfDbLayer(phantomService: PhantomService) extends Logging {

  implicit lazy val ec = ExecutionContext.Implicits.global

  def storeJobStatus(jobStatus: WhatIfJob)(implicit ec: ExecutionContext): Future[Boolean] =
    phantomService
      .storeJobStatus(jobStatus)
      .map(_.wasApplied())
      .recover {
        case ex: Exception =>
          throw new WhatifCassandraDBException(ex.getMessage)
      }

  def getJobById(orgid: String, siteid: String, jobid: String)(
      implicit ec: ExecutionContext
  ): Future[Option[WhatIfJob]] =
    phantomService
      .getJobById(orgid, siteid, jobid)

  def getAllJobs(orgid: String, siteid: String): Future[List[WhatIfJob]] =
    phantomService.getAllJobs(orgid, siteid)

  def updateByJobId(orgid: String, siteid: String, jobid: String, jobStatus: WhatIfJob): Future[SuccessMessage] =
    phantomService
      .updateByJobId(orgid, siteid, jobid, jobStatus)
      .map(
        x =>
          if (x.wasApplied()) SuccessMessage(true)
          else SuccessMessage(false)
      )

  def updateWhatIfBatchId(orgid: String,
                          siteid: String,
                          jobid: String,
                          batchId: Option[Int],
                          updatedat: Option[Long]): Future[SuccessMessage] =
    phantomService
      .updateWhatIfBatchId(orgid, siteid, jobid, batchId, updatedat)
      .map(
        x =>
          if (x.wasApplied()) SuccessMessage(true)
          else SuccessMessage(false)
      )

  def updateWhatIfJobStatus(orgid: String,
                            siteid: String,
                            jobid: String,
                            jobStatus: String,
                            updatedat: Option[Long],
                            additionalmessage: String): Future[SuccessMessage] =
    phantomService
      .updateWhatIfJobStatus(orgid, siteid, jobid, jobStatus, updatedat, additionalmessage)
      .map(
        x =>
          if (x.wasApplied()) SuccessMessage(true)
          else SuccessMessage(false)
      )

  def deleteByJobId(orgid: String, siteid: String, jobid: String): Future[SuccessMessage] =
    phantomService
      .deleteByJobId(orgid, siteid, jobid)
      .map(
        x =>
          if (x.wasApplied()) SuccessMessage(true)
          else SuccessMessage(false)
      )

  def searchWhatIf(orgid: String, siteid: String, key: String, value: String): Future[List[WhatIfJob]] =
    key match {
      case JobId.value =>
        getAllJobs(orgid, siteid).map(
          ba => ba.filter(whatIfjob => whatIfjob.jobid.equalsIgnoreCase(value))
        )
      case name.value =>
        getAllJobs(orgid, siteid).map(
          ba => ba.filter(whatIfjob => whatIfjob.name.getOrElse("").equalsIgnoreCase(value))
        )
    }

  def whatIfExistWithSameName(orgid: String,
                              siteid: String,
                              nameFromUser: String,
                              nameFromDB: Option[String] = None): Future[Boolean] =
    nameFromDB match {
      case Some(dbName) =>
        if (nameFromUser.equalsIgnoreCase(dbName)) {
          Future {
            false
          }
        } else whatIfNameDuplicateCheck(orgid, siteid, nameFromUser)
      case None => whatIfNameDuplicateCheck(orgid, siteid, nameFromUser)
    }

  private[this] def whatIfNameDuplicateCheck(orgid: String, siteid: String, nameFromUser: String): Future[Boolean] =
    getAllJobs(orgid, siteid).map(whatIf => {
      val whatIfs = whatIf.filter(_.name.getOrElse("").equalsIgnoreCase(nameFromUser))
      if (whatIfs.isEmpty) false
      else true
    })

  def storePolicy(policy: Policy): Future[SuccessMessage] =
    phantomService
      .storePolicy(policy)
      .map(
        x =>
          x.wasApplied() match {
            case true  => SuccessMessage(true)
            case false => SuccessMessage(false)
        }
      )

  /**
   * This method is used to fetch each policy with max version
   * Result will be in descending order of their createdon timestamp
   */
  def getListOfPoliciesWithMaxVerison(list: List[Policy]): List[Policy] =
    list
      .filter(_.isdeleted == false)
      .groupBy(_.policyid)
      .mapValues(policyList => policyList.maxBy(_.version))
      .foldLeft(List[Policy]())(
        (maxVersionPolicyList, maxVersionPolicyTuple) => maxVersionPolicyTuple._2 :: maxVersionPolicyList
      )
      .sortWith(_.createdon > _.createdon)

  def getAllPolicies(orgid: String, siteid: String): Future[List[Policy]] =
    phantomService
      .getAllPolicies(orgid, siteid)
      .map(dbPolicies => getListOfPoliciesWithMaxVerison(dbPolicies))
}

object WhatIfDbLayer {
  def apply(): WhatIfDbLayer = new WhatIfDbLayer(new PhantomService with ProductionDatabase {})
}
