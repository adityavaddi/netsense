package com.vz.nsp.parking.grouppolicyservice.helper

import java.util.{Calendar, UUID}

import com.vz.nsp.parking.db.{PhantomService, ProductionDatabase}
import com.vz.nsp.parking.dblayer.DbLayer
import com.vz.nsp.parking.grouppolicyservice.BaseSpec
import com.vz.nsp.parking.grouppolicyservice.constants.TestDataFeed._
import com.vz.nsp.parking.grouppolicyservice.db.parkingServicesDbTest.{ParkingSpaceTable, parkingGroupPolicyLinkTable, policyMappingTable, session}
import com.vz.nsp.parking.grouppolicyservice.service.{GroupPolicyService, ParkingSpaceService}
import com.vz.nsp.parking.model.CaselType.productionApi
import com.vz.nsp.parking.model._
import org.scalatest.time.{Seconds, Span}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
  * Created by nagarna on 9/26/17.
  */

class StreamHelperSpec extends BaseSpec {
  val uid = UUID.randomUUID().toString
  val messageId = UUID.randomUUID().toString
  val messageId1 = UUID.randomUUID().toString
  val messageId2 = UUID.randomUUID().toString
  val orgid = UUID.randomUUID().toString
  val siteid = UUID.randomUUID().toString
  val instanceId = "1"
  val resTopic = "responseTopic"
  val requestId = UUID.randomUUID().toString
  val parkingGroupId = UUID.randomUUID().toString
  val parkingGroupId1 = UUID.randomUUID().toString
  val parkingGroupId2 = UUID.randomUUID().toString
  val policyId = UUID.randomUUID().toString
  val policyId1 = UUID.randomUUID().toString
  val policyId2 = UUID.randomUUID().toString
  val dbLayer = spy(new DbLayer(new PhantomService with ProductionDatabase {}))
  val groupPolicyHelper = spy(new GroupPolicyService(dbLayer))
  val parkingSpotHelper = spy(new ParkingSpaceService(dbLayer))
  val streamHelper = spy(new StreamHelper(groupPolicyHelper, parkingSpotHelper))
  val appRequest: AppRequest = AppRequest(messageId, resTopic,
    new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "associatedParkingGroups", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgid), SiteProps(siteid),
      ConfigProps(Some(policyId), None, None, Some(parkingGroupId), None, None, None, None, None, None, None, None, None, None), AppUserDataProps(None, "", "", None)))
  val appRequestIrrelevent = AppRequest(messageId, resTopic,
    new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString, "Irrelevant",
      "NoneModel", "CAN_READ", Some("user"), OrgProps(orgid), SiteProps(siteid),
      ConfigProps(Some(policyId), None, None, Some(parkingGroupId), None, None, None, None, None, None, None, None, None, None), AppUserDataProps(None, "", "", None)))
  val appRequestMsgNull = AppRequest(null, resTopic,
    new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString, "associatedParkingGroups",
      "NoneModel", "CAN_READ", Some("user"), OrgProps(orgid), SiteProps(siteid),
      ConfigProps(Some(policyId), None, None, Some(parkingGroupId), None, None, None, None, None, None, None, None, None, None), AppUserDataProps(None, "", "", None)))

  private implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
  val appRequestRspTopicNull = AppRequest(messageId1, null,
    new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString, "associatedParkingGroups",
      "NoneModel", "CAN_READ", Some("user"), OrgProps(orgid), SiteProps(siteid),
      ConfigProps(Some(policyId), None, None, Some(parkingGroupId), None, None, None, None, None, None, None, None, None, None), AppUserDataProps(None, "", "", None)))
  val appRequestReqObjNull = AppRequest(messageId1, resTopic, null)
  val timestamp = "9867654246885"
  val failRespBody = FailureResponseBody(requestId, timestamp, true, "Has it Failed!!!", 400)
  val appFailResp = AppFailureResponse(messageId, failRespBody)
  val ParkingGroupInputList1 = ParkingGroupInputList(parkingGroupId1, 123456, -1, 10)
  val ParkingGroupInputList2 = ParkingGroupInputList(parkingGroupId2, 879875, -1, 10)
  val parkingIpList = List(ParkingGroupInputList1, ParkingGroupInputList2)


  /* Method: processRequest */
  "processRequest" should "Return Failure due to \"Message ID NULL\"" in {

    val chain = for {
      appResponse <- streamHelper.processRequest(appRequestMsgNull)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.isEmpty)
    }
  }

  "processRequest" should "Return Failure due to \"Response Topic NULL\"" in {

    val chain = for {
      appResponse <- streamHelper.processRequest(appRequestRspTopicNull)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId1).==(false))
    }
  }

  "processRequest" should "Return Failure due to \"Request Object NULL\"" in {

    val chain = for {
      appResponse <- streamHelper.processRequest(appRequestReqObjNull)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId1).==(false))
    }
  }

  "processRequest" should "Return Generic Failure Case - \"Irrelevant type found in the request\"" in {

    val chain = for {
      appResponse <- streamHelper.processRequest(appRequestIrrelevent)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }
  }

  "processRequest" should "Return Failure Case" in {

    doReturn(Future(appFailResp)).when(streamHelper).matchCaselTypeAndProcessRequest(appRequest, orgid, siteid, "Irrelevant")

    val chain = for {
      appResponse <- streamHelper.processRequest(appRequest)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }
  }
  val appRequest1 = AppRequest(messageId1, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
    "associatedParkingGroups", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgid), SiteProps(siteid),
    ConfigProps(Some(policyId1), None, None, Some(parkingGroupId1), None, None, None, None, None, None, None,
      None, None, None), AppUserDataProps(None, "", "", None)))
  val appRequestParkingSpot = AppRequest(messageId2, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
    "getMetadataForParkingSpot", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgid), SiteProps(siteid),
    ConfigProps(Some(policyId2), None, None, None, None, None, Some(parkingSpotId), None, None, None,
      None, None, None, None), AppUserDataProps(None, "", "", None)))

  override def beforeAll(): Unit = {
    DatabaseService.init()
    session.init()
  }

  override def afterAll(): Unit = {
    DatabaseService.cleanup()

  }

  "processRequest" should "Return Success Case" in {

    doReturn(Future(List(policy))).when(dbLayer).getAllLivePoliciesById(policyId1, orgid, siteid,productionApi.value)
    doReturn(Future(parkingIpList)).when(dbLayer).getAssociatedParkingGroupsOfPolicyId(policyId1)

    val chain = for {
      appResponse <- streamHelper.processRequest(appRequest1)
    } yield appResponse


    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId1))
    }
  }

  object DatabaseService {

    def init() = {
      Await.ready(policyMappingTable.customTypesPolicy, 5.seconds)
      Await.ready(policyMappingTable.createPolicyTable, 5.seconds)
      Await.ready(parkingGroupPolicyLinkTable.createGroupTable(), 5.seconds)
      Await.ready(ParkingSpaceTable.customTypesSpace, 5.seconds)
      Await.ready(ParkingSpaceTable.createSpaceTable(), 5.seconds)
    }

    def cleanup() = {
      Await.ready(policyMappingTable.truncatePolicy, 5.seconds)
      Await.ready(parkingGroupPolicyLinkTable.truncateTable(), 5.seconds)
    }

  }

}
