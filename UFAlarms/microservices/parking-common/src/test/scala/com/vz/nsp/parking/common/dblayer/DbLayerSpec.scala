package com.vz.nsp.parking.common.dblayer

import com.outworkers.phantom.ResultSet
import com.vz.nsp.parking.common.BaseSpec
import com.vz.nsp.parking.db.{PhantomService}
import com.vz.nsp.parking.dblayer.DbLayer
import com.vz.nsp.parking.model.CaselType.{productionApi, syntheticApi}

import scala.concurrent.{ExecutionContext, Future}

/**
 * Created by thangth on 10/5/17.
 */
class DbLayerSpec extends BaseSpec {

  import com.vz.nsp.parking.common.constants.TestDataFeed._
  val phantomService = mock[PhantomService]
  val dblayer        = spy(new DbLayer(phantomService))
  val mockResultSet  = mock[ResultSet]
  implicit val ec    = ExecutionContext.Implicits.global

  "getAllPolicies" should "return list of Policies" in {
    doReturn(Future(List(policy, updatedPolicy)))
      .when(phantomService)
      .getAllPolicies(orgid, siteid, productionApi.value)
    val chain = for {
      response <- dblayer.getAllPolicies(orgid, siteid, productionApi.value)
    } yield response

    whenReady(chain)(res => res.size mustBe 1)

  }

  "getAllLivePolicies" should "returns list of Policies" in {
    doReturn(Future(List(policy, updatedPolicy)))
      .when(phantomService)
      .getAllLivePoliciesById(uid, orgid, siteid, productionApi.value)

    val chain = for {
      response <- dblayer.getAllLivePoliciesById(uid, orgid, siteid, productionApi.value)
    } yield response

    whenReady(chain)(res => res.size mustBe 2)

  }

  "getPolicyByIdAndVersion" should "returns list of Policies" in {
    doReturn(Future(Option(policy)))
      .when(phantomService)
      .getPolicyByIdAndVersion(uid, 1, orgid, siteid, productionApi.value)

    val chain = for {
      response <- dblayer.getPolicyByIdAndVersion(uid, 1, orgid, siteid)
    } yield response

    whenReady(chain)(res => res.size mustBe 1)

  }

  "getMaxVersionPolicyById" should "returns max version of a Policy" in {
    doReturn(Future(2)).when(phantomService).getMaxVersionPolicyById(uid, productionApi.value)

    val chain = for {
      response <- dblayer.getMaxVersionById(uid)
    } yield response

    whenReady(chain)(res => res mustBe 2)

  }

  "store Policy" should "returns success" in {
    doReturn(Future(mockResultSet)).when(phantomService).storePolicy(policy, productionApi.value)
    doReturn(true).when(mockResultSet).wasApplied()

    val chain = for {
      response <- dblayer.storePolicy(policy, productionApi.value)
    } yield response

    whenReady(chain)(res => res.success mustBe true)

  }

  "store Policy" should "returns failure" in {
    doReturn(Future(mockResultSet)).when(phantomService).storePolicy(policy, productionApi.value)
    doReturn(false).when(mockResultSet).wasApplied()

    val chain = for {
      response <- dblayer.storePolicy(policy, productionApi.value)
    } yield response

    whenReady(chain)(res => res.success mustBe false)

  }

  "getAllPoliciesByTagid" should "return policies added to Tag" in {
    doReturn(Future(List(policy, updatedPolicy)))
      .when(phantomService)
      .getPoliciesByTagId(orgid, siteid, tagId, productionApi.value)

    val chain = for {
      response <- dblayer.getAllPoliciesByTagid(orgid, siteid, tagId, productionApi.value)
    } yield response

    whenReady(chain)(res => res.size mustBe 2)

  }

  "getAllPoliciesByNameWithinOrg" should "return list of Policies in the Org" in {
    doReturn(Future(List(updatedPolicy))).when(phantomService).getPoliciesByNameWithinOrg(policyName, orgid, siteid)

    val chain = for {
      response <- dblayer.getAllPoliciesByNameWithinOrg(policyName, orgid, siteid)
    } yield response

    whenReady(chain)(res => res.size mustBe 1)

  }

  "getAllPoliciesBytagidWithinOrg" should "return list of Policies in the Org" in {
    doReturn(Future(List(policy, updatedPolicy))).when(phantomService).getPoliciesByTagWithinOrg(tagId, orgid, siteid)

    val chain = for {
      response <- dblayer.getAllPoliciesBytagidWithinOrg(tagId, orgid, siteid)
    } yield response

    whenReady(chain)(res => res.size mustBe 1)

  }

  "getListOfPoliciesWithMaxVerison" should "return list of Policies with Max version" in {
    val maxVersionList = dblayer.getListOfPoliciesWithMaxVerison(List(policy, updatedPolicy))
    assert(maxVersionList.size == 1)
  }

  "deleteByPolicyIdAndVersion" should "returns success" in {
    doReturn(Future(mockResultSet)).when(phantomService).deleteByPolicyId(uid, 2, orgid, siteid, productionApi.value)
    doReturn(true).when(mockResultSet).wasApplied()

    val chain = for {
      response <- dblayer.deleteByPolicyIdAndVersion(uid, 2, orgid, siteid, productionApi.value)
    } yield response

    whenReady(chain)(res => res.success mustBe true)

  }

  "deleteByPolicyIdAndVersion" should "returns failure" in {
    doReturn(Future(mockResultSet)).when(phantomService).deleteByPolicyId(uid, 2, orgid, siteid, productionApi.value)
    doReturn(false).when(mockResultSet).wasApplied()

    val chain = for {
      response <- dblayer.deleteByPolicyIdAndVersion(uid, 2, orgid, siteid, productionApi.value)
    } yield response

    whenReady(chain)(res => res.success mustBe false)

  }

  "updateWhatIfPolicyById" should "returns success" in {
    doReturn(Future(mockResultSet)).when(phantomService).updatePolicyById(uid, 2, updatedPolicy, productionApi.value)
    doReturn(true).when(mockResultSet).wasApplied()

    val chain = for {
      response <- dblayer.updateByPolicyId(uid, 2, updatedPolicy, productionApi.value)
    } yield response

    whenReady(chain)(res => res.success mustBe true)

  }

  "updateWhatIfPolicyById" should "returns failure" in {
    doReturn(Future(mockResultSet)).when(phantomService).updatePolicyById(uid, 2, updatedPolicy, productionApi.value)
    doReturn(false).when(mockResultSet).wasApplied()

    val chain = for {
      response <- dblayer.updateByPolicyId(uid, 2, updatedPolicy, productionApi.value)
    } yield response

    whenReady(chain)(res => res.success mustBe false)

  }

  "store Tag" should "returns success" in {
    doReturn(Future(mockResultSet)).when(phantomService).storeTag(tag, productionApi.value)
    doReturn(true).when(mockResultSet).wasApplied()

    val chain = for {
      response <- dblayer.storeTag(tag, productionApi.value)
    } yield response

    whenReady(chain)(res => res mustBe true)

  }

  "store Tag" should "returns failure" in {
    doReturn(Future(mockResultSet)).when(phantomService).storeTag(tag, productionApi.value)
    doReturn(false).when(mockResultSet).wasApplied()

    val chain = for {
      response <- dblayer.storeTag(tag, productionApi.value)
    } yield response

    whenReady(chain)(res => res mustBe false)

  }

  "get Tag" should "returns tag" in {
    doReturn(Future(Option(updatedTag))).when(phantomService).getTag(orgid, siteid, tagId, productionApi.value)

    val chain = for {
      response <- dblayer.getTag(orgid, siteid, tagId, productionApi.value)
    } yield response

    whenReady(chain)(res => res.size mustBe 1)

  }

  "get Tag" should "returns no tag" in {
    doReturn(Future(None)).when(phantomService).getTag(orgid, siteid, tagId, productionApi.value)

    val chain = for {
      response <- dblayer.getTag(orgid, siteid, tagId, productionApi.value)
    } yield response

    whenReady(chain)(res => res.size mustBe 0)

  }

  "get All Tag" should "returns list of tags" in {
    doReturn(Future(List(tag, updatedTag))).when(phantomService).getAllTag(orgid, siteid, productionApi.value)

    val chain = for {
      response <- dblayer.getAllTag(orgid, siteid, productionApi.value)
    } yield response

    whenReady(chain)(res => res.size mustBe 2)

  }

  "get All Tag" should "returns Nil of tags" in {
    doReturn(Future(Nil)).when(phantomService).getAllTag(orgid, siteid, productionApi.value)

    val chain = for {
      response <- dblayer.getAllTag(orgid, siteid, productionApi.value)
    } yield response

    whenReady(chain)(res => res.size mustBe 0)

  }

  "Update By Tag Id" should "returns updated tag" in {
    doReturn(Future(mockResultSet)).when(phantomService).updateByTagId(tagId, tag, productionApi.value)
    doReturn(true).when(mockResultSet).wasApplied()

    val chain = for {
      response <- dblayer.updateByTagId(tagId, tag, productionApi.value)
    } yield response

    whenReady(chain)(res => res mustBe true)

  }

  "Update By Tag Id" should "returns failure update" in {
    doReturn(Future(mockResultSet)).when(phantomService).updateByTagId(tagId, tag, productionApi.value)
    doReturn(false).when(mockResultSet).wasApplied()

    val chain = for {
      response <- dblayer.updateByTagId(tagId, tag, productionApi.value)
    } yield response

    whenReady(chain)(res => res mustBe false)

  }
  //////////////////////////What-if tags

  "Store What-if Tag" should "returns success" in {
    doReturn(Future(mockResultSet)).when(phantomService).storeTag(tag, syntheticApi.value)
    doReturn(true).when(mockResultSet).wasApplied()

    val chain = for {
      response <- dblayer.storeTag(tag, syntheticApi.value)
    } yield response

    whenReady(chain)(res => res mustBe true)

  }

  "Store What-if Tag" should "returns failure" in {
    doReturn(Future(mockResultSet)).when(phantomService).storeTag(tag, syntheticApi.value)
    doReturn(false).when(mockResultSet).wasApplied()

    val chain = for {
      response <- dblayer.storeTag(tag, syntheticApi.value)
    } yield response

    whenReady(chain)(res => res mustBe false)

  }

  "get What-if Tag" should "returns tag" in {
    doReturn(Future(Option(updatedTag))).when(phantomService).getTag(orgid, siteid, tagId, syntheticApi.value)

    val chain = for {
      response <- dblayer.getTag(orgid, siteid, tagId, syntheticApi.value)
    } yield response

    whenReady(chain)(res => res.size mustBe 1)

  }

  "get What-if Tag" should "returns no tag" in {
    doReturn(Future(None)).when(phantomService).getTag(orgid, siteid, tagId, syntheticApi.value)

    val chain = for {
      response <- dblayer.getTag(orgid, siteid, tagId, syntheticApi.value)
    } yield response

    whenReady(chain)(res => res.size mustBe 0)

  }

  "get All What-if Tag" should "returns list of tags" in {
    doReturn(Future(List(tag, updatedTag))).when(phantomService).getAllTag(orgid, siteid, syntheticApi.value)

    val chain = for {
      response <- dblayer.getAllTag(orgid, siteid, syntheticApi.value)
    } yield response

    whenReady(chain)(res => res.size mustBe 2)

  }

  "get All What-if Tag" should "returns Nil of tags" in {
    doReturn(Future(Nil)).when(phantomService).getAllTag(orgid, siteid, syntheticApi.value)

    val chain = for {
      response <- dblayer.getAllTag(orgid, siteid, syntheticApi.value)
    } yield response

    whenReady(chain)(res => res.size mustBe 0)

  }

  "Update By What-if TagId" should "returns updated tag" in {
    doReturn(Future(mockResultSet)).when(phantomService).updateByTagId(tagId, tag, syntheticApi.value)
    doReturn(true).when(mockResultSet).wasApplied()

    val chain = for {
      response <- dblayer.updateByTagId(tagId, tag, syntheticApi.value)
    } yield response

    whenReady(chain)(res => res mustBe true)

  }

  "Update By What-if TagId" should "returns failure update" in {
    doReturn(Future(mockResultSet)).when(phantomService).updateByTagId(tagId, tag, syntheticApi.value)
    doReturn(false).when(mockResultSet).wasApplied()

    val chain = for {
      response <- dblayer.updateByTagId(tagId, tag, syntheticApi.value)
    } yield response

    whenReady(chain)(res => res mustBe false)

  }

  //////////////////////////

  "Create Parking Group Policy Link" should "returns success" in {
    doReturn(Future(mockResultSet)).when(phantomService).associateGroupPolicy(groupPolicyLink, 1)
    doReturn(true).when(mockResultSet).wasApplied()

    val chain = for {
      response <- dblayer.applyGroupPolicyLink(groupPolicyLink, 1)
    } yield response

    whenReady(chain)(res => res.success mustBe true)

  }

  "Create Parking Group Policy Link" should "returns failure" in {
    doReturn(Future(mockResultSet)).when(phantomService).associateGroupPolicy(groupPolicyLink, 1)
    doReturn(false).when(mockResultSet).wasApplied()

    val chain = for {
      response <- dblayer.applyGroupPolicyLink(groupPolicyLink, 1)
    } yield response

    whenReady(chain)(res => res.success mustBe false)

  }

  "Check Parking Group Policy Linked" should "returns success" in {
    doReturn(Future(List(groupPolicyLink, updatedGroupPolicyLink)))
      .when(phantomService)
      .checkGroupLinkedWithPolicy(parkingGroupId)

    val chain = for {
      response <- dblayer.checkGroupLinkedWithPolicy(parkingGroupId)
    } yield response

    whenReady(chain)(res => res.size mustBe 2)

  }

  "Check Parking Group Policy Linked" should "returns Nil List" in {
    doReturn(Future(Nil)).when(phantomService).checkGroupLinkedWithPolicy(parkingGroupId)

    val chain = for {
      response <- dblayer.checkGroupLinkedWithPolicy(parkingGroupId)
    } yield response

    whenReady(chain)(res => res.size mustBe 0)

  }

  "Get Parking Group List using Policy Id" should "returns list of policies" in {
    doReturn(Future(List(groupPolicyLink, updatedGroupPolicyLink, expiredGroupPolicyLink)))
      .when(phantomService)
      .getPolicyByIdInTimeline(uid)

    val chain = for {
      response <- dblayer.getPolicyByIdInTimeline(uid, productionApi.value)
    } yield response

    whenReady(chain)(res => res.size mustBe 3)

  }

  "Get Parking Group List using Group Id" should "returns list of policies" in {
    doReturn(Future(Option(groupPolicyLink))).when(phantomService).getGroupPolicyByGroupId(parkingGroupId)

    val chain = for {
      response <- dblayer.getGroupPolicyByGroupId(parkingGroupId)
    } yield response

    whenReady(chain)(res => res.size mustBe 1)

  }

  "Dis assiciate Parking Group for a policy" should "returns success" in {
    doReturn(Future(mockResultSet)).when(phantomService).updateGroupPolicyLink(groupUid)
    doReturn(true).when(mockResultSet).wasApplied()

    val chain = for {
      response <- dblayer.updateGroupPolicyLink(groupUid)
    } yield response

    whenReady(chain)(res => res.success mustBe true)

  }

  "Dis assiciate Parking Group for a policy" should "returns failure" in {
    doReturn(Future(mockResultSet)).when(phantomService).updateGroupPolicyLink(groupUid)
    doReturn(false).when(mockResultSet).wasApplied()

    val chain = for {
      response <- dblayer.updateGroupPolicyLink(groupUid)
    } yield response

    whenReady(chain)(res => res.success mustBe false)

  }

  "Create Parking Spot" should "returns success" in {
    doReturn(Future(mockResultSet)).when(phantomService).storeParkingSpace(parkingSpot, 125671L)
    doReturn(true).when(mockResultSet).wasApplied()

    val chain = for {
      response <- dblayer.updateSpaceAttributes(parkingSpot, Future(List(parkingSpot)))
    } yield response

    whenReady(chain)(res => res.success mustBe true)

  }

  "Create Parking Spot" should "returns failure" in {
    doReturn(Future(mockResultSet)).when(phantomService).storeParkingSpace(parkingSpot, 125671L)
    doReturn(false).when(mockResultSet).wasApplied()

    val chain = for {
      response <- dblayer.updateSpaceAttributes(parkingSpot, Future(List(parkingSpot)))
    } yield response

    whenReady(chain)(res => res.success mustBe false)

  }

  "Get All Parking Spot" should "returns all parking Spots" in {
    doReturn(Future(List(parkingSpot))).when(phantomService).getParkingSpacesByIds(List(parkingSpot.parkingspaceid))

    val chain = for {
      response <- dblayer.getAllParkingSpaces(List(parkingSpot.parkingspaceid))
    } yield response

    whenReady(chain)(res => res.size mustBe 1)

  }

  "Delete Parking Spot" should "returns success" in {
    doReturn(Future(mockResultSet)).when(phantomService).deleteByParkingSpaceId(List(parkingSpot.parkingspaceid))
    doReturn(true).when(mockResultSet).wasApplied()

    val chain = for {
      response <- dblayer.deleteByParkingSpaceId(List(parkingSpot.parkingspaceid))
    } yield response

    whenReady(chain)(res => res mustBe true)

  }

  "Delete Parking Spot" should "returns failure" in {
    doReturn(Future(mockResultSet)).when(phantomService).deleteByParkingSpaceId(List(parkingSpot.parkingspaceid))
    doReturn(false).when(mockResultSet).wasApplied()

    val chain = for {
      response <- dblayer.deleteByParkingSpaceId(List(parkingSpot.parkingspaceid))
    } yield response

    whenReady(chain)(res => res mustBe false)

  }

  /*"Update a Parking Spot" should "returns success" in {
    doReturn(Future(mockResultSet)).when(phantomService).updateByParkingSpotId(spotId,parkingSpot)
    doReturn(true).when(mockResultSet).wasApplied()

    val chain = for {
      response <- dblayer.updateByParkingSpotId(spotId,parkingSpot)
    } yield response

    whenReady(chain)(res =>
      res.success mustBe true
    )

  }


  "Update a Parking Spot" should "returns failure" in {
    doReturn(Future(mockResultSet)).when(phantomService).updateByParkingSpotId(spotId,parkingSpot)
    doReturn(false).when(mockResultSet).wasApplied()

    val chain = for {
      response <- dblayer.updateByParkingSpotId(spotId,parkingSpot)
    } yield response

    whenReady(chain)(res =>
      res.success mustBe false
    )

  }
   */

  "Update State of Policy" should "returns failure" in {
    doReturn(Future(mockResultSet)).when(phantomService).updateStateOfPolicy(uid, 2, Some("active"))
    doReturn(false).when(mockResultSet).wasApplied()

    val chain = for {
      response <- dblayer.updateStateOfPolicy(uid, 2, Some("active"))
    } yield response

    whenReady(chain)(res => res.success mustBe false)

  }

  "Update State of Policy" should "returns success" in {
    doReturn(Future(mockResultSet)).when(phantomService).updateStateOfPolicy(uid, 2, Some("active"))
    doReturn(true).when(mockResultSet).wasApplied()

    val chain = for {
      response <- dblayer.updateStateOfPolicy(uid, 2, Some("active"))
    } yield response

    whenReady(chain)(res => res.success mustBe true)

  }

  "Get all Active Groups By Policy Id" should "returns success" in {
    doReturn(Future(List(groupPolicyLink, updatedGroupPolicyLink)))
      .when(phantomService)
      .getAllActicveGroupsByPolicyId(uid)

    val chain = for {
      response <- dblayer.getAllActicveGroupsByPolicyId(uid, 2)
    } yield response

    whenReady(chain)(res => res.success mustBe true)

  }

  "Get allActiveGroups By Policy Id" should "returns failure" in {
    doReturn(Future(Nil)).when(phantomService).getAllActicveGroupsByPolicyId(uid)

    val chain = for {
      response <- dblayer.getAllActicveGroupsByPolicyId(uid, 2)
    } yield response

    whenReady(chain)(res => res.success mustBe false)

  }

  "Get Existing Tag Ids" should "returns success" in {
    doReturn(Future(List(tag))).when(phantomService).getExistingTagIds(listTagIds, productionApi.value)
    val chain = for {
      response <- dblayer.getExistingTagIds(listTagIds, orgid, siteid, productionApi.value)
    } yield response

    whenReady(chain)(res => res.size mustBe 1)

  }

  "getListOfTagids" should "returns success" in {
    val list = dblayer.getListOfTagids(List(tag), orgid, siteid)
    list.size mustBe 1
  }

}
