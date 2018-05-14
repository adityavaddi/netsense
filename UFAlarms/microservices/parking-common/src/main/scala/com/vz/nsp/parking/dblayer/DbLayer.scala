package com.vz.nsp.parking.dblayer

import com.verizon.netsense.utils.Logging
import com.vz.nsp.parking.db.{PhantomService, ProductionDatabase}
import com.vz.nsp.parking.model.CaselType.productionApi
import com.vz.nsp.parking.model.{SuccessMessage, UserData, _}

import scala.concurrent.{ExecutionContext, Future}

/**
 * Created by donthgo on 7/4/17.
 */
class DbLayer(phantomService: PhantomService) extends Logging {
  implicit val ec = ExecutionContext.Implicits.global

  def storeTag(tag: Tag, requestType: String): Future[Boolean] = {
    val res = phantomService
      .storeTag(tag, requestType)
      .map(_.wasApplied())
    res
  }

  def getTag(orgid: String, siteid: String, uid: String, requestType: String): Future[Option[Tag]] =
    phantomService
      .getTag(orgid, siteid, uid, requestType)
      .map(e => e.filter(!_.isdeleted))

  def getAllTag(orgid: String, siteid: String, requestType: String): Future[List[Tag]] =
    phantomService.getAllTag(orgid, siteid, requestType).map(tag => tag.filter(!_.isdeleted))

  def deleteByTagId(uid: String, requestType: String, orgid: String, siteid: String): Future[Boolean] =
    phantomService
      .deleteByTagId(uid, requestType, orgid: String, siteid: String)
      .map(_.wasApplied)

  def updateByTagId(uid: String, tag: Tag, requestType: String): Future[Boolean] =
    phantomService
      .updateByTagId(uid, tag, requestType)
      .map(_.wasApplied)

  def tagExistWithSameName(orgId: String,
                           siteId: String,
                           nameFromUser: String,
                           requestType: String,
                           nameFromDB: Option[String] = None): Future[Boolean] =
    nameFromDB match {
      case Some(dbName) =>
        if (nameFromUser.equalsIgnoreCase(dbName)) {
          Future {
            false
          }
        } else tagNameDuplicateCheck(orgId, siteId, requestType, nameFromUser)
      case None => tagNameDuplicateCheck(orgId, siteId, requestType, nameFromUser)
    }

  private[this] def tagNameDuplicateCheck(orgId: String,
                                          siteId: String,
                                          requestType: String,
                                          nameFromUser: String): Future[Boolean] =
    getAllTag(orgId, siteId, requestType).map(tag => {
      val tags = tag.filter(_.name.equalsIgnoreCase(nameFromUser))
      if (tags.isEmpty) false
      else true
    })

  def storePolicy(policy: Policy, requestType: String): Future[SuccessMessage] =
    phantomService
      .storePolicy(policy, requestType)
      .map(
        x =>
          if (x.wasApplied()) {
            SuccessMessage(true)
          } else {
            SuccessMessage(false)
        }
      )

  private[this] def polciyNameDuplicateCheck(orgId: String,
                                             siteId: String,
                                             requestType: String,
                                             nameFromUser: String): Future[Boolean] =
    getAllPolicies(orgId, siteId, requestType).map(policy => {
      val policies = policy.filter(_.name.equalsIgnoreCase(nameFromUser))
      if (policies.isEmpty) false
      else true
    })

  def policyExistWithSameNameInSite(orgId: String,
                                    siteId: String,
                                    nameFromUser: String,
                                    requestType: String,
                                    nameFromDB: Option[String] = None): Future[Boolean] =
    nameFromDB match {
      case Some(dbName) =>
        if (nameFromUser.equalsIgnoreCase(dbName)) {
          Future {
            false
          }
        } else polciyNameDuplicateCheck(orgId, siteId, requestType, nameFromUser)
      case None => polciyNameDuplicateCheck(orgId, siteId, requestType, nameFromUser)
    }

  def getAllPoliciesById(uid: String, orgId: String, siteId: String, requestType: String): Future[List[Policy]] =
    phantomService.getAllPoliciesById(uid, orgId, siteId, requestType)

  def getAllLivePoliciesById(uid: String,
                             orgid: String,
                             siteid: String,
                             requestType: String = productionApi.value): Future[List[Policy]] =
    phantomService.getAllLivePoliciesById(uid, orgid, siteid, requestType).map(_.filter(_.isDeleted == false))

  def getPolicyByIdAndVersion(uid: String,
                              version: Int,
                              orgid: String,
                              siteid: String,
                              requestType: String = productionApi.value): Future[List[Policy]] = {
    val result = phantomService
      .getPolicyByIdAndVersion(uid, version, orgid, siteid, requestType)
      .map(_.filter(_.isDeleted == false))
    result.map {
      case Some(policy) => List(policy)
      case _            => Nil
    }
  }

  def getMaxVersionPolicyByIdOrgSite(policyId: String,
                                     orgId: String,
                                     siteId: String,
                                     requestType: String): Future[Int] =
    phantomService.getMaxVersionBypolciyIdOrgSite(policyId, orgId, siteId, requestType)

  def getAllPoliciesByTagid(orgId: String, siteId: String, tagid: String, requestType: String): Future[List[Policy]] =
    phantomService.getPoliciesByTagId(orgId, siteId, tagid, requestType)

  def getAllPoliciesByNameWithinOrg(name: String, orgid: String, siteid: String): Future[List[Policy]] =
    phantomService.getPoliciesByNameWithinOrg(name, orgid, siteid).map(getListOfPoliciesWithMaxVerison)

  def getAllPoliciesBytagidWithinOrg(tagid: String, orgid: String, siteid: String): Future[List[Policy]] =
    phantomService.getPoliciesByTagWithinOrg(tagid, orgid, siteid).map(getListOfPoliciesWithMaxVerison)

  def getAllPolicies(orgid: String, siteid: String, requestType: String): Future[List[Policy]] =
    phantomService
      .getAllPolicies(orgid, siteid, requestType)
      .map(getListOfPoliciesWithMaxVerison)

  def getListOfPoliciesWithMaxVerison(list: List[Policy]): List[Policy] =
    list
      .filter(_.isDeleted == false)
      .groupBy(_.uid)
      .mapValues(xs => xs.maxBy(_.version))
      .foldLeft(List[Policy]())((a, b) => b._2 :: a)
      .sortWith(_.createdOn > _.createdOn)

  def deleteByPolicyIdAndVersion(uid: String,
                                 version: Int,
                                 orgId: String,
                                 siteId: String,
                                 requestType: String): Future[SuccessMessage] =
    phantomService
      .deleteByPolicyId(uid, version, orgId, siteId, requestType)
      .map(
        x =>
          if (x.wasApplied()) {
            SuccessMessage(true)
          } else {
            SuccessMessage(false)
        }
      )

  def updateByPolicyId(uid: String, version: Int, policy: Policy, requestType: String): Future[SuccessMessage] =
    phantomService
      .updatePolicyById(uid, version, policy, requestType)
      .map(
        x =>
          if (x.wasApplied()) {
            SuccessMessage(true)
          } else {
            SuccessMessage(false)
        }
      )

  def applyGroupPolicyLink(grouppolicy: ParkingGroupPolicyLink, version: Int): Future[SuccessMessage] =
    phantomService
      .associateGroupPolicy(grouppolicy, version)
      .map(
        x =>
          if (x.wasApplied()) {
            SuccessMessage(true)
          } else {
            SuccessMessage(false)
        }
      )

  def checkGroupLinkedWithPolicy(
      groupid: String
  ): Future[List[ParkingGroupPolicyLink]] =
    phantomService.checkGroupLinkedWithPolicy(groupid)

  def checkGroupAssociatedWithPolicy(groupid: String, policyid: String): Future[List[ParkingGroupPolicyLink]] =
    phantomService.checkGroupAssociatedWithPolicy(groupid, policyid)

  def getPolicyByIdInTimeline(policyid: String, requestType: String): Future[List[ParkingGroupPolicyLink]] =
    phantomService.getPolicyByIdInTimeline(policyid)

  def getAssociatedParkingGroupsForPolicy(policyid: String): Future[List[String]] =
    phantomService
      .getPolicyByIdInTimeline(policyid)
      .map(timelineList => getAssociatedGroupsAtPresent(timelineList))

  private def getAssociatedGroupsAtPresent(timelineList: List[ParkingGroupPolicyLink]): List[String] = {
    val liveAssociations = timelineList.filter(policyGroupLink => policyGroupLink.endTime == -1)
    liveAssociations.map(_.parkinggroupid)
  }

  def getWhatIfJobsByPolicyId(policyId: String, orgId: String, siteId: String): Future[List[WhatIfJob]] =
    phantomService
      .getWhatIfJobsByPolicyId(policyId, orgId, siteId)
      .map(jobs => {
        jobs.map(whatIfJob => whatIfJobsWithOutVersion(whatIfJob))
      })
      .map(_.filter(job => job.policieswithversion.getOrElse(List()).contains(policyId)))

  def getAllJobs(orgId: String, siteId: String): Future[List[WhatIfJob]] =
    phantomService
      .getAllJobs(orgId, siteId)
      .map(jobs => {
        jobs.map(whatIfJob => whatIfJobsWithOutVersion(whatIfJob))
      })

  def whatIfJobsWithOutVersion(whatIfJob: WhatIfJob) =
    WhatIfJob(
      createdbyuserid = whatIfJob.createdbyuserid,
      lastupdatedbyuserid = whatIfJob.lastupdatedbyuserid,
      updatedat = whatIfJob.updatedat,
      createdat = whatIfJob.createdat,
      orgid = whatIfJob.orgid,
      siteid = whatIfJob.siteid,
      jobid = whatIfJob.jobid,
      name = whatIfJob.name,
      fromtime = whatIfJob.fromtime,
      totime = whatIfJob.totime,
      parkinggroups = whatIfJob.parkinggroups,
      policieswithversion = Option(getPolicyListWithOutVersion(whatIfJob.policieswithversion.getOrElse(List()))),
      starttime = whatIfJob.starttime,
      endtime = whatIfJob.endtime,
      submittime = whatIfJob.submittime,
      jobstatus = whatIfJob.jobstatus,
      batchid = whatIfJob.batchid,
      appid = whatIfJob.appid,
      mode = whatIfJob.mode,
      aborttime = whatIfJob.aborttime,
      intervalperiod = whatIfJob.intervalperiod,
      email = whatIfJob.email
    )

  def getPolicyListWithOutVersion(policyListWithVersion: List[String]): List[String] =
    policyListWithVersion
      .map(eachPolicyWithVersion => eachPolicyWithVersion.split(":").toList)
      .map(policy => policy.head)

  def checkTheStatusOfJobsById(policyId: String, orgId: String, siteId: String): Future[List[WhatIfJob]] =
    phantomService
      .getJobsStatusByPolicyId(policyId, orgId, siteId)
      .map(jobs => {
        jobs.map(whatIfJob => whatIfJobsWithOutVersion(whatIfJob))
      })
      .map(_.filter(job => job.policieswithversion.getOrElse(List()).contains(policyId)))

  def getMaxVersionById(policyid: String, requestType: String = productionApi.value): Future[Int] =
    phantomService.getMaxVersionPolicyById(policyid, productionApi.value)

  def getGroupPolicyByGroupId(groupid: String): Future[Option[ParkingGroupPolicyLink]] =
    phantomService.getGroupPolicyByGroupId(groupid)

  def updateGroupPolicyLink(uid: String): Future[SuccessMessage] =
    phantomService
      .updateGroupPolicyLink(uid)
      .map(
        x =>
          if (x.wasApplied()) {
            SuccessMessage(true)
          } else {
            SuccessMessage(false)
        }
      )

  def getVersionHistoryById(uid: String, orgid: String, siteid: String): Future[List[Policy]] =
    phantomService.getVersionHistoryById(uid, orgid, siteid)

  def getAllPoliciesWithGroupId(
      parkinggroupid: String
  ): Future[List[ParkingGroupPolicyLink]] =
    phantomService.getAllPoliciesWithGroupId(parkinggroupid)

  def getAssociatedParkingGroupsOfPolicyId(policyId: String): Future[List[ParkingGroupInputList]] =
    phantomService.getAllAssociatedParkingGroupsOfPolicyId(policyId)

  def updateStateOfPolicy(uid: String, version: Int, state: Option[String]): Future[SuccessMessage] =
    phantomService
      .updateStateOfPolicy(uid, version, state)
      .map(
        x =>
          if (x.wasApplied()) {
            SuccessMessage(true)
          } else {
            SuccessMessage(false)
        }
      )

  def getAllActicveGroupsByPolicyId(policyid: String, version: Int): Future[SuccessMessage] =
    phantomService
      .getAllActicveGroupsByPolicyId(policyid)
      .map(
        policyGroupLink =>
          if (policyGroupLink.nonEmpty) {
            SuccessMessage(true)
          } else {
            SuccessMessage(false)
        }
      )

  def getExistingTagIds(requestedTags: List[String],
                        orgId: String,
                        siteId: String,
                        requestType: String): Future[List[String]] =
    phantomService
      .getExistingTagIds(requestedTags, requestType)
      .map(getListOfTagids(_, orgId, siteId))

  /**
   * This method is used to fetch only active tags
   */
  def getListOfTagids(list: List[Tag], orgId: String, siteId: String): List[String] =
    list
      .filter(_.isdeleted == false)
      .filter(_.orgid == orgId)
      .filter(_.siteid == siteId)
      .foldLeft(List[String]()) { (z, f) =>
        z :+ f.uid
      }

  def updateByUserDataId(appid: String,
                         userid: String,
                         userdataid: String,
                         userdata: UserData): Future[SuccessMessage] =
    phantomService
      .updateByUserDataId(appid, userid, userdataid, userdata)
      .map(
        x =>
          if (x.wasApplied()) {
            SuccessMessage(true)
          } else {
            SuccessMessage(false)
        }
      )

  def storeUserData(userData: UserData): Future[SuccessMessage] =
    phantomService
      .storeUserData(userData)
      .map(
        x =>
          if (x.wasApplied()) {
            SuccessMessage(true)
          } else {
            SuccessMessage(false)
        }
      )

  def getUserData(appid: String, userid: String, userdataid: String): Future[Option[UserData]] =
    phantomService
      .getUserData(appid, userid, userdataid)

  def getDeleteUserData(appid: String, userid: String, userdataid: String): Future[List[UserData]] =
    phantomService
      .getUserData(appid, userid, userdataid)
      .map(_ match {
        case Some(userData) => List(userData)
        case _              => Nil
      })

  //def getAllUserData(orgid: String, siteid: String): Future[List[UserData]] = PhantomService.getAllUserData(orgid, siteid)
  def getAllUserData(appid: String, userid: String): Future[List[UserData]] =
    phantomService.getAllUserData( /*orgid, siteid,*/ appid, userid)

  def deleteByUserDataId(appid: String, userid: String, userdataid: String): Future[SuccessMessage] =
    phantomService
      .deleteByUserDataId(appid, userid, userdataid)
      .map(
        x =>
          x.wasApplied() match {
            case true  => SuccessMessage(true)
            case false => SuccessMessage(false)
        }
      )

  def getAllParkingSpaces(parkingspaceids: List[String]): Future[List[ParkingSpace]] =
    phantomService.getParkingSpacesByIds(parkingspaceids).map(getListOfParkingSpaces)

  def deleteByParkingSpaceId(parkingspaceids: List[String]): Future[Boolean] =
    phantomService
      .deleteByParkingSpaceId(parkingspaceids)
      .map(x => x.wasApplied())

  def getExistingParkingSpaceIds(parkingspaceids: List[String]): Future[List[String]] =
    phantomService
      .getParkingSpacesByIds(parkingspaceids)
      .map(getListOfParkingSpaceids)

  /**
   * This method is used to fetch only active ParkingSpaces
   */
  def getListOfParkingSpaceids(parkingSpotList: List[ParkingSpace]): List[String] =
    parkingSpotList.filter(_.isDeleted == false).map(e => e.parkingspaceid)

  def getListOfParkingSpaces(parkingSpotList: List[ParkingSpace]): List[ParkingSpace] =
    parkingSpotList.filter(_.isDeleted == false)

  def updateSpaceAttributes(parkingSpot: ParkingSpace,
                            parkingspaces: Future[List[ParkingSpace]]): Future[SuccessMessageWithSpaceAttributes] = {
    val matchingSpaceRecord: Future[List[ParkingSpace]] =
      parkingspaces.map(list => list.filter(_.parkingspaceid == parkingSpot.parkingspaceid))
    val createdOn: Future[Long] = matchingSpaceRecord.map(_ match {
      case Nil      => parkingSpot.createdOn
      case listOfPC => listOfPC.head.createdOn
    })

    createdOn
      .flatMap(
        e =>
          phantomService
            .storeParkingSpace(parkingSpot, e)
            .map(
              x =>
                if (x.wasApplied()) SuccessMessageWithSpaceAttributes(true, Some(parkingSpot.parkingspaceid))
                else SuccessMessageWithSpaceAttributes(false, Some(parkingSpot.parkingspaceid))
          )
      )
      .recover {
        case ex: Exception =>
          log.error(
            "Unable to add space attributes for spaces : " + parkingSpot.parkingspaceid + " with error: " + ex.getMessage
          )
          SuccessMessageWithSpaceAttributes(false, Some(parkingSpot.parkingspaceid))
      }
  }

}

object DbLayer {
  def apply(): DbLayer = new DbLayer(new PhantomService with ProductionDatabase {})
}
