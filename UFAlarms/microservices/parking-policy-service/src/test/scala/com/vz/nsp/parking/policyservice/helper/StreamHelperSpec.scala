package com.vz.nsp.parking.policyservice.helper

import java.util.{Calendar, UUID}

import com.vz.nsp.parking.db.{EmbeddedDatabase, PhantomService, ProductionDatabase}
import com.vz.nsp.parking.dblayer.DbLayer
import com.vz.nsp.parking.model._
import com.vz.nsp.parking.policyservice.config.TestSuiteConfig
import com.vz.nsp.parking.policyservice.db.PolicyConnector.policyMappingTable
import com.vz.nsp.parking.policyservice.util.BaseSpec
import org.scalatest.time.{Seconds, Span}
import com.vz.nsp.parking.model.CaselType.productionApi

import scala.concurrent.Future

/*
* Created by nagarna on 9/28/17.
*/

class StreamHelperSpec extends BaseSpec with EmbeddedDatabase with TestSuiteConfig {

  object DatabaseService {

    def init() = {
      policyMappingTable.policyCustomTypes
      policyMappingTable.createPolicyTable
    }

    def cleanup() = {
      policyMappingTable.truncatePolicy
    }
  }

  override def beforeAll(): Unit = {
    DatabaseService.init()
    session.init()
  }

  override def afterAll(): Unit = {
    DatabaseService.cleanup()
  }

  val uid = UUID.randomUUID().toString

  val messageId = UUID.randomUUID().toString
  val messageId1 = UUID.randomUUID().toString
  val messageId2 = UUID.randomUUID().toString

  val orgId = UUID.randomUUID().toString
  val siteId = UUID.randomUUID().toString

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
  val policyHelper = spy(new PolicyHelper(dbLayer))
  val streamHelper = spy(new StreamHelper(policyHelper))

  private implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  val appRequest: AppRequest = AppRequest(messageId, resTopic,
    new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "associatedParkingGroups", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(Some(policyId), None, None, Some(parkingGroupId), None, None, None, None, None, None, None, None, None, None), AppUserDataProps(None, "", "", None)))

  val appRequestIrrelevent = AppRequest(messageId, resTopic,
    new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString, "Irrelevant",
      "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(Some(policyId), None, None, Some(parkingGroupId), None, None, None, None, None, None, None, None, None, None), AppUserDataProps(None, "", "", None)))

  val appRequestMsgNull = AppRequest(null, resTopic,
    new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString, "associatedParkingGroups",
      "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(Some(policyId), None, None, Some(parkingGroupId), None, None, None, None, None, None, None, None, None, None), AppUserDataProps(None, "", "", None)))

  val appRequestRspTopicNull = AppRequest(messageId1, null,
    new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString, "associatedParkingGroups",
      "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(Some(policyId), None, None, Some(parkingGroupId), None, None, None, None, None, None, None, None, None, None), AppUserDataProps(None, "", "", None)))

  val appRequestReqObjNull = AppRequest(messageId1, resTopic, null)

  val timestamp = "9867654246885"
  val failRespBody = FailureResponseBody(requestId, timestamp, true, "Has it Failed!!!", 400)
  val appFailResp = AppFailureResponse(messageId, failRespBody)

  val timeRange: TimeRange             = TimeRange("06:00:00", "07:00:00")
  val timeRange2: TimeRange            = TimeRange("07:00:00","08:00:00")
  val timeRange3: TimeRange            = TimeRange("08:00:00","10:00:00")
  val valperiod: Period                = Period("2017-06-28;19:31:03", "2018-07-28;19:31:03")
  val datePeriod: DatePeriod           = DatePeriod(valperiod, "yearly")
  val parkingSchedule: ParkingSchedule = ParkingSchedule(Some("Schedule"), None, None, Some(datePeriod), Some(Set(timeRange,timeRange2,timeRange3)))
  val violation: ViolationFine         = ViolationFine("ppv", 100.0, Some("daily"))
  val parkingPenalty: ParkingPenalty   = ParkingPenalty(Some("Penalty description"), Set(violation))
  val maxDuration: MaxDuration         = MaxDuration(24.0, "hours")
  val parkingCharge: ParkingCharge     = ParkingCharge(Some("Charge Description"), Some("Charge Description"), None, Some(maxDuration), 60.4)
  val parkingRule: ParkingRule         = ParkingRule(Some("Rule"), true, Some(parkingCharge), Some(parkingPenalty))


  val policyRule: PolicyRule = PolicyRule(Some("Policy rule"), Some("Policy"), Some("Policy rule Description"), 12, parkingSchedule, parkingRule)

  val policyLvlViolation = PolicyLevelViolation("1234", "PolicyViolation-1", Some("Polcy Violation Description"),
    "ppv", 222.32, Some("daily"))

  val policy: Policy = Policy(uid, "newupdateon 13 july", siteId, orgId, Set("Sunday policy"), Set(policyLvlViolation),
    false, "7653612563", 14986863999977L, 14986963999977L, false, 10, "new policy",
    Some("Policy description"), "Africa/Costarica", None, Set(policyRule))

  val policyRequest = PolicyRequest(Some(uid), Some("policyAuthorizerid"), Option(Set("Sunday policy")), Some(Set(policyLvlViolation)),
    "name", Some("timeZone"), Some("description"), Some(Set(policyRule)))

  val appRequest1 = AppRequest(messageId1, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
    "createParkingPolicy", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
    ConfigProps(Some(policyId1), None, None, Some(parkingGroupId1), None, None, None, None, None, None,
      Some(policyRequest), None, None, None), AppUserDataProps(None, "", "", None)))

  val parkingSpotId = UUID.randomUUID().toString

  val appRequestParkingSpot = AppRequest(messageId2, resTopic, RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
    "getParkingPolicyVersion", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
    ConfigProps(Some(policyId2), None, Some(10), None, None, None, Some(parkingSpotId), None, None, None, None,
      None, None, None), AppUserDataProps(None, "", "", None)))


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

    doReturn(Future(appFailResp)).when(streamHelper).matchCaselTypeAndProcessRequest(appRequest, orgId, siteId, "")

    val chain = for {
      appResponse <- streamHelper.processRequest(appRequest)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }
  }

  "processRequest" should "Return Success Case" in {

    doReturn(Future(SuccessMessage(true))).when(dbLayer).storePolicy(policy,productionApi.value)

    val chain = for {
      appResponse <- streamHelper.processRequest(appRequest1)
    } yield appResponse


    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId1))
    }
  }

  /* Method: matchCaselTypeAndProcessRequest */
  "matchCaselTypeAndProcessRequest" should "Return Success-1" in {

    doReturn(Future(List(policy))).when(dbLayer).getPolicyByIdAndVersion(policyId2, policy.version, orgId, siteId,productionApi.value)

    val chain = for {
      appResponse <- streamHelper.matchCaselTypeAndProcessRequest(appRequestParkingSpot, orgId, siteId, "getParkingPolicyVersion")
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId2))
    }
  }

}
