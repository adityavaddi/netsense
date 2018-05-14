package com.vz.nsp.parking.grouppolicyservice.db

import java.time.Instant
import java.util.UUID

import com.outworkers.phantom.dsl._
import com.vz.nsp.parking.db.EmbeddedDatabase
import com.vz.nsp.parking.grouppolicyservice.BaseSpec
import com.vz.nsp.parking.model.ParkingGroupPolicyLink

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext}

/**
 * Created by thangth on 8/17/17.
 *
  **/
class ParkingGroupPolicyLinkSpec extends BaseSpec with EmbeddedDatabase {
  private implicit val ec = ExecutionContext.Implicits.global

  object Database {
    def init() =
      Await.ready(parkingServicesDbTest.parkingGroupPolicyLinkTable.createGroupTable(), 5.seconds)

    def cleanup() =
      Await.ready(parkingServicesDbTest.parkingGroupPolicyLinkTable.truncateTable(), 5.seconds)
  }

  override def beforeAll() = {
    super.beforeAll()
    Database.init()
    session.init()
  }

  override def afterAll() = {
    Database.cleanup()
    super.afterAll()
  }

  val uid1            = UUID.randomUUID().toString
  val uid2            = UUID.randomUUID().toString
  val uid3            = UUID.randomUUID().toString
  val uid4            = UUID.randomUUID().toString
  val parkingGroupId1 = "1234567890"
  val parkingGroupId2 = "0987654321"
  val parkingGroupId3 = "1357924680"
  val parkingGroupId4 = "0864297531"
  val policyId1       = "a1b2c3d4e5f6g7h8i9j0"
  val policyId2       = "z1y2x3v4u5t6s7r8r9p0"
  val policyId3       = "a1s2d3f4g5h6j7k8l9m0"
  val policyId4       = "q1w2e3r4t5y6u7i8o9p0"

  val parkingGroupPolicyLink1 =
    ParkingGroupPolicyLink(uid1, parkingGroupId1, policyId1, 1, Instant.now().getEpochSecond, -1)
  val parkingGroupPolicyLink2 =
    ParkingGroupPolicyLink(uid2, parkingGroupId2, policyId2, 1, Instant.now().getEpochSecond, -1)
  val parkingGroupPolicyLink3 =
    ParkingGroupPolicyLink(uid3, parkingGroupId3, policyId3, 1, Instant.now().getEpochSecond, -1)
  val parkingGroupPolicyLink4 =
    ParkingGroupPolicyLink(uid4, parkingGroupId4, policyId4, 1, Instant.now().getEpochSecond, -1)

  it should "associate Policy to group " in {
    val future = database.parkingGroupPolicyLinkTable.associateGroupPolicy(parkingGroupPolicyLink1, 3)
    whenReady(future) { result =>
      result isExhausted () mustBe true
      result wasApplied () mustBe true
      result one ()
    }
  }

  behavior of "saving Parking Group Policy Link"
  it should "save Parking Group Policy Link" in {

    val chain = for {
      store <- database.parkingGroupPolicyLinkTable.associateGroupPolicy(parkingGroupPolicyLink1, 1)
    } yield store
    whenReady(chain) { res =>
      assert(res.wasApplied())
    }

  }

  behavior of "updating Parking Group Policy Link"

  it should "Parking Group Policy Link" in {
    val chain = for {
      store <- database.parkingGroupPolicyLinkTable.associateGroupPolicy(parkingGroupPolicyLink2, 1)
      store <- database.parkingGroupPolicyLinkTable.updateGroupPolicyLink(uid2)
    } yield store

    whenReady(chain) { res =>
      assert(res.wasApplied())
    }

  }

  behavior of "checkGroupLinkedWithPolicy"
  it should "checkGroupLinkedWithPolicy" in {
    val chain = for {
      store <- database.parkingGroupPolicyLinkTable.associateGroupPolicy(parkingGroupPolicyLink2, 1)
      store <- database.parkingGroupPolicyLinkTable.checkGroupLinkedWithPolicy(parkingGroupId2)
    } yield store

    whenReady(chain) { res =>
      //assert(res.filter(x => x.endTime < 0 )).size == 1)
      assert(res.filter(x => x.parkinggroupid.equals(parkingGroupId2)).size == 1)
    }
  }

  behavior of "checkGroupAssociatedWithPolicy"
  it should "checkGroupAssociatedWithPolicy" in {
    val chain = for {
      store <- database.parkingGroupPolicyLinkTable.associateGroupPolicy(parkingGroupPolicyLink3, 1)
      store <- database.parkingGroupPolicyLinkTable.checkGroupAssociatedWithPolicy(parkingGroupId3, policyId3)
    } yield store

    whenReady(chain) { res =>
      //assert(res.filter(x => x.endTime < 0 )).size == 1)
      assert(res.filter(x => x.parkinggroupid.equals(parkingGroupId3)).size == 1)
      assert(res.filter(x => x.policyid.equals(policyId3)).size == 1)

    }
  }

  behavior of "getGroupPolicyByGroupId"
  it should "getGroupPolicyByGroupId" in {

    val chain = for {
      store <- database.parkingGroupPolicyLinkTable.associateGroupPolicy(parkingGroupPolicyLink4, 1)
      store <- database.parkingGroupPolicyLinkTable.getGroupPolicyByGroupId(parkingGroupId4)
    } yield store

    whenReady(chain) { res =>
      assert(res.filter(x => x.parkinggroupid.equals(parkingGroupId4)).size == 1)
    }
  }

  behavior of "getPolicyById"
  it should "getPolicyById" in {

    val chain = for {
      store <- database.parkingGroupPolicyLinkTable.associateGroupPolicy(parkingGroupPolicyLink4, 1)
      store <- database.parkingGroupPolicyLinkTable.getPolicyByIdInTimeline(policyId4)
    } yield store

    whenReady(chain) { res =>
      assert(res.filter(x => x.policyid.equals(policyId4)).size == 1)
    }
  }

  behavior of "get Parking Group Policy Link"
  it should "get Parking Group Policy Link" in {
    val chain = for {
      store <- database.parkingGroupPolicyLinkTable.associateGroupPolicy(parkingGroupPolicyLink3, 1)
      store <- database.parkingGroupPolicyLinkTable.getAllAssignedParkingGroupsForPolicyId(policyId3)
    } yield store

    whenReady(chain) { res =>
      assert(res.map(x => x.equals(parkingGroupId3)).size == 1)
    }
  }

}
