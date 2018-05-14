package com.vz.nsp.parking.db

import com.outworkers.phantom.dsl.ResultSet
import com.verizon.netsense.metrics.Instrumented
import com.vz.nsp.parking.model.CaselType.productionApi
import com.vz.nsp.parking.model.{UserData, _}
import nl.grons.metrics.scala.Timer
import com.vz.nsp.parking.model.ResponseMessage._

import scala.concurrent.{ExecutionContext, Future}

trait PhantomService extends ProductionDatabase with Instrumented {
  implicit val ec = ExecutionContext.Implicits.global

  implicit private val phantomConnector     = ProductionDb.connector
  private[this] val cassandraPersist: Timer = metrics.timer("cassandra-async-persist-timer")

  def storeTag(tag: Tag, requestType: String = productionApi.value): Future[ResultSet] = cassandraPersist.time {
    if (requestType == productionApi.value)
      database.TagTable.store(tag)
    else
      database.WhatIfParkingTagTable.store(tag)
  }

  def getTag(orgid: String, siteid: String, uid: String, requestType: String): Future[Option[Tag]] =
    if (requestType == productionApi.value)
      database.TagTable.getTagById(orgid, siteid, uid)
    else
      database.WhatIfParkingTagTable.getTagById(orgid, siteid, uid)

  def getAllTag(orgid: String, siteid: String, requestType: String): Future[List[Tag]] =
    if (requestType == productionApi.value)
      database.TagTable.getAllTag(orgid, siteid)
    else
      database.WhatIfParkingTagTable.getAllTags(orgid, siteid)

  def deleteByTagId(uid: String, requestType: String, orgId: String, siteId: String): Future[ResultSet] =
    cassandraPersist.time {
      if (requestType == productionApi.value)
        database.TagTable.deleteByTagId(uid)
      else
        database.WhatIfParkingTagTable.deleteByTagId(uid, orgId, siteId)
    }

  def updateByTagId(uid: String, tag: Tag, requestType: String): Future[ResultSet] = cassandraPersist.time {
    if (requestType == productionApi.value)
      database.TagTable.updateByTagId(uid, tag)
    else
      database.WhatIfParkingTagTable.updateByTagId(uid, tag)
  }

  def getExistingTagIds(requestedTags: List[String], requestType: String): Future[List[Tag]] =
    if (requestType == productionApi.value)
      database.TagTable.getLiveTagIds(requestedTags)
    else
      database.WhatIfParkingTagTable.getLiveTagIds(requestedTags)

  def storePolicy(policy: Policy, requestType: String): Future[ResultSet] = cassandraPersist.time {
    if (requestType == productionApi.value)
      database.policyMappingTable.storePolicy(policy)
    else
      database.WhatIfPolicyTable.storeWhatIfPolicy(policy)
  }

  def getAllPolicies(orgid: String, siteid: String, requestType: String): Future[List[Policy]] =
    if (requestType == productionApi.value)
      database.policyMappingTable.getAllPolicies(orgid, siteid)
    else
      database.WhatIfPolicyTable.getAllPolicies(orgid, siteid)

  def getPolicyById(policyId: String): Future[Option[Policy]] =
    database.policyMappingTable.getPolicyById(policyId)

  def getPoliciesByTagId(orgId: String, siteId: String, tag: String, requestType: String): Future[List[Policy]] =
    if (requestType == productionApi.value)
      database.policyMappingTable.getPolicyByTag(tag)
    else
      getAllPolicies(orgId, siteId, requestType).map(
        _.filter(newPolicy => !newPolicy.isDeleted)
          .filter(newPolicy => newPolicy.tags.contains(tag))
      )

  def getPoliciesByNameWithinOrg(name: String, orgid: String, siteid: String): Future[List[Policy]] =
    database.policyMappingTable.getPolicyByNameWithinOrg(name, orgid, siteid)

  def getPoliciesByTagWithinOrg(tagid: String, orgid: String, siteid: String): Future[List[Policy]] =
    database.policyMappingTable.getPolicyByTagidWithinOrg(tagid, orgid, siteid)

  def getPolicyByIdAndVersion(policyId: String,
                              version: Int,
                              orgid: String,
                              siteid: String,
                              requestType: String): Future[Option[Policy]] =
    if (requestType == productionApi.value)
      database.policyMappingTable.getPolicyByIdAndVerison(policyId, version, orgid, siteid)
    else
      database.WhatIfPolicyTable.getWhatIfPolicyByIdAndVersion(policyId, version, orgid, siteid)

  def updatePolicyById(policyId: String, version: Int, policy: Policy, requestType: String): Future[ResultSet] =
    cassandraPersist.time {
      if (requestType == productionApi.value)
        database.policyMappingTable.updateByPolicyId(policyId, version, policy)
      else
        database.WhatIfPolicyTable.updateWhatIfPolicyById(policyId, version, policy)
    }

  def getMaxVersionPolicyById(policyId: String, requestType: String): Future[Int] =
    database.policyMappingTable.getMaxVersionById(policyId)

  def getMaxVersionBypolciyIdOrgSite(policyId: String,
                                     orgId: String,
                                     siteId: String,
                                     requestType: String): Future[Int] =
    if (requestType == productionApi.value)
      database.policyMappingTable.getMaxVersionById(policyId)
    else
      getAllPoliciesById(database.WhatIfPolicyTable.getAllLiveWhatIfPoliciesById(policyId, orgId, siteId))

  def getAllPoliciesById(list: Future[List[Policy]]): Future[Int] = {
    val policy: Future[List[Policy]] = list.map(
      _.filter(_.isDeleted == false)
        .groupBy(_.uid)
        .mapValues(xs => xs.maxBy(_.version))
        .foldLeft(List[Policy]())((a, b) => b._2 :: a)
    )
    val policyVersion = policy.map(_ match {
      case x :: tail => x.version
      case Nil       => 0
    })
    policyVersion
  }

  def getAllPoliciesById(uid: String, orgId: String, siteId: String, requestType: String): Future[List[Policy]] =
    if (requestType == productionApi.value)
      database.policyMappingTable.getAllPoliciesById(uid)
    else
      database.WhatIfPolicyTable.getAllLiveWhatIfPoliciesById(uid, orgId, siteId)

  def getAllLivePoliciesById(uid: String, orgid: String, siteid: String, requestType: String): Future[List[Policy]] =
    if (requestType == productionApi.value)
      database.policyMappingTable.getAllLivePoliciesById(uid, orgid, siteid)
    else
      database.WhatIfPolicyTable.getAllLiveWhatIfPoliciesById(uid, orgid, siteid)

  def updatePolicyWithId(uid: String, version: Int, policy: Policy): Future[ResultSet] = cassandraPersist.time {
    database.policyMappingTable.updateByPolicyId(uid, version + 1, policy)
  }

  def deleteByPolicyId(uid: String,
                       version: Int,
                       orgId: String,
                       siteId: String,
                       requestType: String): Future[ResultSet] = cassandraPersist.time {
    if (requestType == productionApi.value)
      database.policyMappingTable.deleteByPolicyId(uid, version)
    else
      database.WhatIfPolicyTable.deleteByWhatIfPolicyId(uid, version, orgId, siteId)
  }

  def associateGroupPolicy(groupPolicy: ParkingGroupPolicyLink, version: Int): Future[ResultSet] =
    cassandraPersist.time {
      database.parkingGroupPolicyLinkTable.associateGroupPolicy(groupPolicy, version)
    }

  def checkGroupLinkedWithPolicy(groupid: String): Future[List[ParkingGroupPolicyLink]] =
    database.parkingGroupPolicyLinkTable.checkGroupLinkedWithPolicy(groupid)

  def checkGroupAssociatedWithPolicy(groupid: String, policyid: String): Future[List[ParkingGroupPolicyLink]] =
    database.parkingGroupPolicyLinkTable.checkGroupAssociatedWithPolicy(groupid, policyid)

  def getPolicyByIdInTimeline(policyid: String): Future[List[ParkingGroupPolicyLink]] =
    database.parkingGroupPolicyLinkTable.getPolicyByIdInTimeline(policyid)

  def getWhatIfJobsByPolicyId(policyId: String, orgId: String, siteId: String): Future[List[WhatIfJob]] =
    getAllJobs(orgId, siteId)

  def getJobsStatusByPolicyId(policyId: String, orgId: String, siteId: String): Future[List[WhatIfJob]] =
    getAllJobs(orgId, siteId).map(
      _.filter(
        job => job.jobstatus.getOrElse("").equalsIgnoreCase(Pending.value)  || job.jobstatus.getOrElse("").equalsIgnoreCase(Running.value)  || job.jobstatus.getOrElse("").equalsIgnoreCase(Completed.value)
      )
    )

  def getAllJobs(orgId: String, siteId: String): Future[List[WhatIfJob]] =
    database.WhatIfJobTable.getAllJobs(orgId, siteId)

  def getGroupPolicyByGroupId(groupid: String): Future[Option[ParkingGroupPolicyLink]] =
    database.parkingGroupPolicyLinkTable.getGroupPolicyByGroupId(groupid)

  def updateGroupPolicyLink(uid: String): Future[ResultSet] = cassandraPersist.time {
    database.parkingGroupPolicyLinkTable.updateGroupPolicyLink(uid)
  }

  def getVersionHistoryById(uid: String, orgid: String, siteid: String): Future[List[Policy]] =
    database.policyMappingTable.getVersionHistoryById(uid, orgid, siteid)

  def getAllPoliciesWithGroupId(parkinggroupid: String): Future[List[ParkingGroupPolicyLink]] =
    database.parkingGroupPolicyLinkTable.getAllPoliciesWithGroupId(parkinggroupid)

  def getAllAssociatedParkingGroupsOfPolicyId(policyId: String): Future[List[ParkingGroupInputList]] =
    database.parkingGroupPolicyLinkTable.getAllAssignedParkingGroupsForPolicyId(policyId)

  def storeParkingSpace(parkingSpot: ParkingSpace, createdOn: Long): Future[ResultSet] = cassandraPersist.time {
    database.ParkingSpaceTable.storeParkingSpace(parkingSpot, createdOn)
  }

  def deleteByParkingSpaceId(parkingspotids: List[String]): Future[ResultSet] = cassandraPersist.time {
    database.ParkingSpaceTable.deleteByParkingSpaceId(parkingspotids)
  }

  def updateStateOfPolicy(uid: String, version: Int, state: Option[String]): Future[ResultSet] = cassandraPersist.time {
    database.policyMappingTable.updateStateOfPolicy(uid, version, state)
  }

  def getAllActicveGroupsByPolicyId(policyid: String): Future[List[ParkingGroupPolicyLink]] =
    database.parkingGroupPolicyLinkTable.getAllActicveGroupsByPolicyId(policyid)

  def storeUserData(userData: UserData): Future[ResultSet] = cassandraPersist.time {
    database.UserDataTable.store(userData)
  }
  def getAllUserData(appid: String, userid: String): Future[List[UserData]] =
    database.UserDataTable.getAllUserData(appid, userid)

  def getUserData(appid: String, userid: String, userdataid: String): Future[Option[UserData]] =
    database.UserDataTable.getUserDataById(appid, userid, userdataid)

  def deleteByUserDataId(appid: String, userid: String, userdataid: String): Future[ResultSet] = cassandraPersist.time {
    database.UserDataTable.deleteByUserDataId(appid, userid, userdataid)
  }

  def updateByUserDataId(appid: String, userid: String, userdataid: String, userdata: UserData): Future[ResultSet] =
    cassandraPersist.time {
      database.UserDataTable.updateByUserDataId(appid, userid, userdataid, userdata)
    }

  def getParkingSpacesByIds(parkingspaceids: List[String]): Future[List[ParkingSpace]] =
    database.ParkingSpaceTable.getParkingSpacesByIds(parkingspaceids)

  def getParkingSpaceById(parkingspaceid: String): Future[Option[Long]] =
    database.ParkingSpaceTable.getParkingSpaceById(parkingspaceid)

}
