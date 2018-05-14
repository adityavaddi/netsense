package com.vz.nsp.parking.common.constants

import java.util.{Calendar, UUID}

import com.vz.nsp.parking.model._

/**
  * Created by thangth on 10/10/17.
  */
object TestDataFeed {

  val orgid = UUID.randomUUID().toString
  val siteid = UUID.randomUUID().toString
  val policyLevelViolation = PolicyLevelViolation("id", "name", None, "ppv", 20.56, Some("once"))
  val uid = UUID.randomUUID().toString
  val authorizerId = UUID.randomUUID().toString
  val hashValue = UUID.randomUUID().toString
  val tagId = UUID.randomUUID().toString
  val createdOn = Calendar.getInstance().getTimeInMillis
  val updatedOn = Calendar.getInstance().getTimeInMillis
  val policyName = "new Policy"
  val policy = Policy(uid, authorizerId, siteid, orgid, Set(tagId), Set(policyLevelViolation),
    false, hashValue, createdOn, 0, false, 1, policyName, None, "EST", Some("unassigned"), Set())
  val updatedPolicy = Policy(uid, authorizerId, siteid, orgid, Set(tagId), Set(policyLevelViolation),
    false, hashValue, createdOn, updatedOn, false, 2, policyName, None, "EST", Some("unassigned"), Set())

  val tagId2 = UUID.randomUUID().toString
  val tagName = "tag unit test"
  val tagDescription = "This tag is for unit testing"
  val tag = Tag(tagId, tagName, tagDescription, orgid, siteid, createdOn, 0, false)
  val updatedTag = Tag(tagId, tagName, tagDescription, orgid, siteid, createdOn, updatedOn, false)
  val listTagIds = List(tagId, tagId2)

  val groupUid = UUID.randomUUID().toString
  val parkingGroupId = UUID.randomUUID().toString
  val expiredOn = Calendar.getInstance().getTimeInMillis
  val groupPolicyLink = ParkingGroupPolicyLink(groupUid, parkingGroupId, uid, 1, createdOn, -1)
  val updatedGroupPolicyLink = ParkingGroupPolicyLink(groupUid, parkingGroupId, uid, 2, createdOn, -1)
  val expiredGroupPolicyLink = ParkingGroupPolicyLink(groupUid, parkingGroupId, uid, 3, createdOn, expiredOn)

  val spotId = UUID.randomUUID().toString
  val spotname = "new Spot"
  val sensorId = UUID.randomUUID().toString
  val payStationId = UUID.randomUUID().toString
  val parkingSpot = ParkingSpace(spotId, spotname, List("CAR", "MOTORCYCLE"),false, false,sensorId, "business","not-metered",true,false,List("school"), "meterid","paystationid","5","curb-side",None, 125671L,1344L, false)

}
