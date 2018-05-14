package com.vz.nsp.parking.policyservice.helper

import java.util.{Calendar, UUID}

import com.vz.nsp.parking.db.{EmbeddedDatabase, PhantomService, ProductionDatabase}
import com.vz.nsp.parking.dblayer.DbLayer
import com.vz.nsp.parking.model._
import com.vz.nsp.parking.policyservice.db.PolicyConnector._
import com.vz.nsp.parking.policyservice.util.BaseSpec
import org.scalatest.time.{Seconds, Span}
import com.vz.nsp.parking.model.CaselType.productionApi
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/*
 * Created by nagarna on 9/28/17.
 */

class PolicyHelperSpec extends BaseSpec with EmbeddedDatabase {

  val uid  = UUID.randomUUID().toString
  val uid1 = UUID.randomUUID().toString
  val uid2 = UUID.randomUUID().toString
  val uid3 = UUID.randomUUID().toString

  val messageId   = UUID.randomUUID().toString
  val messageId1  = UUID.randomUUID().toString
  val messageId2  = UUID.randomUUID().toString
  val messageId3  = UUID.randomUUID().toString
  val messageId4  = UUID.randomUUID().toString
  val messageId5  = UUID.randomUUID().toString
  val messageId6  = UUID.randomUUID().toString
  val messageId7  = UUID.randomUUID().toString
  val messageId8  = UUID.randomUUID().toString
  val messageId9  = UUID.randomUUID().toString
  val messageId10 = UUID.randomUUID().toString
  val messageId11 = UUID.randomUUID().toString
  val messageId12 = UUID.randomUUID().toString
  val messageId13 = UUID.randomUUID().toString
  val messageId14 = UUID.randomUUID().toString
  val messageId15 = UUID.randomUUID().toString
  val messageId16 = UUID.randomUUID().toString
  val messageId17 = UUID.randomUUID().toString
  val messageId18 = UUID.randomUUID().toString
  val messageId19 = UUID.randomUUID().toString
  val messageId20 = UUID.randomUUID().toString
  val messageId21 = UUID.randomUUID().toString

  val orgId  = UUID.randomUUID().toString
  val siteId = UUID.randomUUID().toString

  val instanceId = "1"
  val resTopic   = "responseTopic"

  val requestId = UUID.randomUUID().toString

  val parkingGroupId  = UUID.randomUUID().toString
  val parkingGroupId1 = UUID.randomUUID().toString
  val parkingGroupId2 = UUID.randomUUID().toString
  val parkingGroupId3 = UUID.randomUUID().toString

  val policyId   = UUID.randomUUID().toString
  val policyId1  = UUID.randomUUID().toString //
  val policyId2  = UUID.randomUUID().toString
  val policyId3  = UUID.randomUUID().toString
  val policyId4  = UUID.randomUUID().toString
  val policyId5  = UUID.randomUUID().toString
  val policyId6  = UUID.randomUUID().toString
  val policyId7  = UUID.randomUUID().toString
  val policyId8  = UUID.randomUUID().toString
  val policyId9  = UUID.randomUUID().toString
  val policyId10 = UUID.randomUUID().toString
  val policyId11 = UUID.randomUUID().toString
  val policyId12 = UUID.randomUUID().toString
  val policyId13 = UUID.randomUUID().toString
  val policyId14 = UUID.randomUUID().toString
  val policyId15 = UUID.randomUUID().toString
  val policyId16 = UUID.randomUUID().toString
  val policyId17 = UUID.randomUUID().toString
  val policyId18 = UUID.randomUUID().toString
  val policyId19 = UUID.randomUUID().toString
  val policyId20 = UUID.randomUUID().toString
  val policyId21 = UUID.randomUUID().toString

  val dbLayer      = spy(new DbLayer(new PhantomService with ProductionDatabase {}))
  val policyHelper = spy(new PolicyHelper(dbLayer))

  private implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  val policyAppRequest = AppRequest(
    messageId,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "associatedParkingGroups",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(Some(policyId),
                  None,
                  None,
                  Some(parkingGroupId),
                  None,
                  None,
                  None,
                  None,
                  None,
                  None,
                  None,
                  None,
                  None,
                  None),
      AppUserDataProps(None, "", "", None)
    )
  )

  val timeRange: TimeRange   = TimeRange("06:00:00", "07:00:00")
  val timeRange1: TimeRange  = TimeRange("06:00:00", "10:00:00")
  val timeRange2: TimeRange  = TimeRange("07:00:00", "08:00:00")
  val timeRange3: TimeRange  = TimeRange("08:00:00", "10:00:00")
  val valperiod: Period      = Period("2017-06-28;19:31:03", "2018-07-28;19:31:03")
  val datePeriod: DatePeriod = DatePeriod(valperiod, "yearly")
  val parkingSchedule: ParkingSchedule =
    ParkingSchedule(Some("Schedule"), None, None, Some(datePeriod), Some(Set(timeRange, timeRange2)))
  val parkingScheduleWithConflictingTimeRange: ParkingSchedule =
    ParkingSchedule(Some("Schedule"), None, None, Some(datePeriod), Some(Set(timeRange1, timeRange2, timeRange3)))
  val parkingScheduleWithNonConflictingTimeRange: ParkingSchedule =
    ParkingSchedule(Some("Schedule"), None, None, Some(datePeriod), Some(Set(timeRange, timeRange2, timeRange3)))
  val violation: ViolationFine       = ViolationFine("ppv", 100.0, Some("daily"))
  val parkingPenalty: ParkingPenalty = ParkingPenalty(Some("Penalty description"), Set(violation))
  val maxDuration: MaxDuration       = MaxDuration(24.0, "hours")
  val parkingCharge: ParkingCharge =
    ParkingCharge(Some("Charge Description"), Some("Charge Description"), None, Some(maxDuration), 60.4)
  val parkingRule: ParkingRule = ParkingRule(Some("Rule"), true, Some(parkingCharge), Some(parkingPenalty))

  val policyRule: PolicyRule =
    PolicyRule(Some("Policy rule"), Some("Policy"), Some("Policy rule Description"), 12, parkingSchedule, parkingRule)
  val policyRulewithConflictingTimeRange: PolicyRule = PolicyRule(Some("Policy rule"),
                                                                  Some("Policy"),
                                                                  Some("Policy rule Description"),
                                                                  12,
                                                                  parkingScheduleWithConflictingTimeRange,
                                                                  parkingRule)
  val policyRulewithNonConflictingTimeRange: PolicyRule = PolicyRule(Some("Policy rule"),
                                                                     Some("Policy"),
                                                                     Some("Policy rule Description"),
                                                                     12,
                                                                     parkingScheduleWithNonConflictingTimeRange,
                                                                     parkingRule)

  val policyLvlViolation =
    PolicyLevelViolation("1234", "PolicyViolation-1", Some("PolicyViolationDescription"), "ppv", 222.32, Some("daily"))

  val policy: Policy = Policy(
    uid1,
    "newupdateon 13 july",
    siteId,
    orgId,
    Set("Sunday policy"),
    Set(policyLvlViolation),
    false,
    "7653612563",
    14986863999977L,
    14986963999977L,
    false,
    10,
    "new policy",
    Some("Policy description"),
    "Africa/Costarica",
    Some("State"),
    Set(policyRule)
  )

  val policy2: Policy = Policy(
    uid2,
    "newupdateon 13 july",
    siteId,
    orgId,
    Set("Sunday policy"),
    Set(policyLvlViolation),
    false,
    "7653612563",
    14986863999977L,
    14986963999977L,
    false,
    10,
    "new policy",
    Some("Policy description"),
    "Africa/Costarica",
    None,
    Set(policyRule)
  )

  val policy5: Policy = Policy(
    policyId5,
    "newupdateon 13 july",
    siteId,
    orgId,
    Set("Sunday policy"),
    Set(policyLvlViolation),
    false,
    "7653612563",
    14986863999977L,
    14986963999977L,
    false,
    10,
    "new policy",
    Some("Policy description"),
    "Africa/Costarica",
    None,
    Set(policyRule)
  )

  val policyRequest = PolicyRequest(
    Some(uid2),
    Some("policyAuthorizerid"),
    Option(Set("Sunday policy")),
    Some(Set(policyLvlViolation)),
    "name",
    Some("timeZone"),
    Some("description"),
    Some(Set(policyRule))
  )

  val policyRequest3 = PolicyRequest(
    Some(uid3),
    Some("policyAuthorizerid"),
    Option(Set("Sunday policy")),
    Some(Set(policyLvlViolation)),
    "name",
    Some("timeZone"),
    Some("description"),
    Some(Set(policyRule))
  )

  val policyRequestWithConflictingTimRangeWithinPolicyRule = PolicyRequest(
    Some(uid3),
    Some("policyAuthorizerid"),
    Option(Set("Sunday policy")),
    Some(Set(policyLvlViolation)),
    "name",
    Some("timeZone"),
    Some("description"),
    Some(Set(policyRulewithConflictingTimeRange))
  )

  val policyRequestWithNonConflictingTimRangeWithinPolicyRule = PolicyRequest(
    Some(policyId2),
    Some("policyAuthorizerid"),
    Option(Set("Sunday policy")),
    Some(Set(policyLvlViolation)),
    "name",
    Some("timeZone"),
    Some("description"),
    Some(Set(policyRulewithNonConflictingTimeRange))
  )

  val policywithoutPolicyRules = PolicyRequest(
    Some(uid3),
    Some("policyAuthorizerid"),
    Option(Set("Sunday policy")),
    Some(Set(policyLvlViolation)),
    "name",
    Some("timeZone"),
    Some("description"),
    Some(Set())
  )

  val policywithOnePolicyRule = PolicyRequest(
    Some(uid3),
    Some("policyAuthorizerid"),
    Option(Set("Sunday policy")),
    Some(Set(policyLvlViolation)),
    "name",
    Some("timeZone"),
    Some("description"),
    Some(Set(policyRule))
  )

  val mutiplePolicRulesWithConflictingTimeRange = PolicyRequest(
    Some(uid3),
    Some("policyAuthorizerid"),
    Option(Set("Sunday policy")),
    Some(Set(policyLvlViolation)),
    "name",
    Some("timeZone"),
    Some("description"),
    Some(Set(policyRulewithNonConflictingTimeRange, policyRulewithConflictingTimeRange))
  )

  val parkingGroupPolicyLink = ParkingGroupPolicyLink(uid1, parkingGroupId1, policyId1, 10, 1506632586L, 1506630000L)

  val policyAppRequest1 = AppRequest(
    messageId1,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "policyTagsAssociation",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(Some(policyId1),
                  None,
                  None,
                  Some(parkingGroupId1),
                  None,
                  None,
                  None,
                  None,
                  None,
                  None,
                  Some(policyRequest),
                  None,
                  None,
                  None),
      AppUserDataProps(None, "", "", None)
    )
  )

  val policyAppRequest2 = AppRequest(
    messageId2,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "policyTagsAssociation",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(Some(policyId2),
                  None,
                  None,
                  Some(parkingGroupId2),
                  None,
                  None,
                  None,
                  None,
                  None,
                  None,
                  Some(policyRequest),
                  None,
                  None,
                  None),
      AppUserDataProps(None, "", "", None)
    )
  )

  val policyAppRequest3 = AppRequest(
    messageId3,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "policyTagsAssociation",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(Some(policyId3),
                  None,
                  None,
                  Some(parkingGroupId3),
                  None,
                  None,
                  None,
                  None,
                  None,
                  None,
                  Some(policyRequest),
                  None,
                  None,
                  None),
      AppUserDataProps(None, "", "", None)
    )
  )

  val policyAppRequest4 = AppRequest(
    messageId4,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "policyTagsAssociation",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(Some(policyId4),
                  None,
                  None,
                  Some(parkingGroupId3),
                  None,
                  None,
                  None,
                  None,
                  None,
                  None,
                  Some(policyRequest),
                  None,
                  None,
                  None),
      AppUserDataProps(None, "", "", None)
    )
  )

  val tagsPolicyLinkRequest = Some(TagsPolicyLinkRequest(Set("tags")))

  val policyAppRequest5 = AppRequest(
    messageId5,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "policyTagsAssociation",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(Some(policyId5),
                  None,
                  None,
                  Some(parkingGroupId3),
                  None,
                  None,
                  None,
                  None,
                  None,
                  tagsPolicyLinkRequest,
                  Some(policyRequest),
                  None,
                  None,
                  None),
      AppUserDataProps(None, "", "", None)
    )
  )

  val policyAppRequest6 = AppRequest(
    messageId6,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "policyTagsAssociation",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(Some(policyId6),
                  None,
                  None,
                  Some(parkingGroupId3),
                  None,
                  None,
                  None,
                  None,
                  None,
                  tagsPolicyLinkRequest,
                  Some(policyRequest),
                  None,
                  None,
                  None),
      AppUserDataProps(None, "", "", None)
    )
  )

  val policyAppRequest7 = AppRequest(
    messageId7,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "policyTagsDisassociation",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(Some(policyId7),
                  None,
                  None,
                  Some(parkingGroupId3),
                  None,
                  None,
                  None,
                  None,
                  None,
                  tagsPolicyLinkRequest,
                  Some(policyRequest),
                  None,
                  None,
                  None),
      AppUserDataProps(None, "", "", None)
    )
  )

  val policyAppRequest8 = AppRequest(
    messageId8,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "policyTagsDisassociation",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(Some(policyId8),
                  None,
                  None,
                  Some(parkingGroupId3),
                  None,
                  None,
                  None,
                  None,
                  None,
                  tagsPolicyLinkRequest,
                  Some(policyRequest),
                  None,
                  None,
                  None),
      AppUserDataProps(None, "", "", None)
    )
  )

  val policyAppRequest9 = AppRequest(
    messageId9,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "policyTagsDisassociation",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(Some(policyId9),
                  None,
                  None,
                  Some(parkingGroupId3),
                  None,
                  None,
                  None,
                  None,
                  None,
                  tagsPolicyLinkRequest,
                  Some(policyRequest),
                  None,
                  None,
                  None),
      AppUserDataProps(None, "", "", None)
    )
  )

  val policyAppRequest10 = AppRequest(
    messageId10,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "policyTagsDisassociation",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(Some(policyId10),
                  None,
                  None,
                  Some(parkingGroupId3),
                  None,
                  None,
                  None,
                  None,
                  None,
                  tagsPolicyLinkRequest,
                  Some(policyRequest),
                  None,
                  None,
                  None),
      AppUserDataProps(None, "", "", None)
    )
  )

  val parkingGroupPolicyLink11 = ParkingGroupPolicyLink(uid1, parkingGroupId1, policyId11, 10, 1506632586L, 1506630000L)

  val policyAppRequest11 = AppRequest(
    messageId11,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "policyTagsDisassociation",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(Some(policyId11),
                  None,
                  None,
                  Some(parkingGroupId3),
                  None,
                  None,
                  None,
                  None,
                  None,
                  tagsPolicyLinkRequest,
                  Some(policyRequest),
                  None,
                  None,
                  None),
      AppUserDataProps(None, "", "", None)
    )
  )

  val policyAppRequest12 = AppRequest(
    messageId12,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "policyTagsDisassociation",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(Some(policyId12),
                  None,
                  None,
                  Some(parkingGroupId3),
                  None,
                  None,
                  None,
                  None,
                  None,
                  tagsPolicyLinkRequest,
                  Some(policyRequest),
                  None,
                  None,
                  None),
      AppUserDataProps(None, "", "", None)
    )
  )

  val policyAppRequest13 = AppRequest(
    messageId13,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "policyTagsDisassociation",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(Some(policyId13),
                  None,
                  None,
                  Some(parkingGroupId3),
                  None,
                  None,
                  None,
                  None,
                  None,
                  tagsPolicyLinkRequest,
                  Some(policyRequest),
                  None,
                  None,
                  None),
      AppUserDataProps(None, "", "", None)
    )
  )

  val policyAppRequest14 = AppRequest(
    messageId14,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "policyTagsDisassociation",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(Some(policyId14),
                  None,
                  Some(policy.version),
                  Some(parkingGroupId3),
                  None,
                  None,
                  None,
                  None,
                  None,
                  tagsPolicyLinkRequest,
                  Some(policyRequest),
                  None,
                  None,
                  None),
      AppUserDataProps(None, "", "", None)
    )
  )

  val searchPolicyByNameOrTagid = SearchPolicyByNameOrTagid(Some("name"), None)

  val policyAppRequest15 = AppRequest(
    messageId15,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "policyTagsDisassociation",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(
        Some(policyId15),
        None,
        Some(policy.version),
        Some(parkingGroupId3),
        None,
        None,
        None,
        None,
        None,
        tagsPolicyLinkRequest,
        Some(policyRequest),
        None,
        Some(searchPolicyByNameOrTagid),
        None
      ),
      AppUserDataProps(None, "", "", None)
    )
  )

  val policyAppRequest16 = AppRequest(
    messageId16,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "policyTagsDisassociation",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(Some(policyId16),
                  None,
                  Some(policy.version),
                  Some(parkingGroupId3),
                  None,
                  None,
                  None,
                  None,
                  None,
                  tagsPolicyLinkRequest,
                  Some(policyRequest),
                  None,
                  None,
                  None),
      AppUserDataProps(None, "", "", None)
    )
  )

  val policyAppRequest17 = AppRequest(
    messageId17,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "policyTagsDisassociation",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(Some(policyId17),
                  None,
                  Some(policy.version),
                  Some(parkingGroupId3),
                  None,
                  None,
                  None,
                  None,
                  None,
                  tagsPolicyLinkRequest,
                  Some(policyRequest),
                  None,
                  None,
                  None),
      AppUserDataProps(None, "", "", None)
    )
  )

  val policyAppRequest18 = AppRequest(
    messageId18,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "policyTagsDisassociation",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(
        Some(policyId18),
        None,
        Some(policy.version),
        Some(parkingGroupId3),
        Some("98676457978"),
        Some("98676459778"),
        None,
        None,
        None,
        tagsPolicyLinkRequest,
        Some(policyRequest),
        None,
        None,
        None
      ),
      AppUserDataProps(None, "", "", None)
    )
  )

  val policyAppRequest19 = AppRequest(
    messageId19,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "policyTagsDisassociation",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(
        Some(policyId19),
        None,
        Some(policy.version),
        Some(parkingGroupId3),
        Some("98676457978"),
        Some("98676459778"),
        None,
        None,
        None,
        tagsPolicyLinkRequest,
        Some(policyRequest),
        None,
        None,
        None
      ),
      AppUserDataProps(None, "", "", None)
    )
  )

  val policyAppRequest20 = AppRequest(
    messageId20,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "policyTagsDisassociation",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(
        Some(policyId20),
        None,
        Some(policy.version),
        Some(parkingGroupId3),
        Some("98676457978"),
        Some("98676459778"),
        None,
        None,
        None,
        tagsPolicyLinkRequest,
        Some(policyRequest),
        None,
        None,
        None
      ),
      AppUserDataProps(None, "", "", None)
    )
  )

  object DatabaseService {

    def init() = {
      Await.ready(policyMappingTable.policyCustomTypes, 5.seconds)
      Await.ready(policyMappingTable.createPolicyTable, 5.seconds)
      Await.ready(tagTable.createTable(), 5.seconds)
      Await.ready(parkingGroupPolicyLinkTable.createTable(), 5.seconds)
    }

    def cleanup() = {
      Await.ready(policyMappingTable.truncatePolicy, 5.seconds)
      Await.ready(tagTable.truncateTable(), 5.seconds)
      Await.ready(parkingGroupPolicyLinkTable.truncateTable(), 5.seconds)
    }

  }

  override def beforeAll(): Unit = {
    DatabaseService.init()
    session.init()
  }

  override def afterAll(): Unit =
    DatabaseService.cleanup()

  behavior of "Store Policy"
  "Store Policy" should "Store Policy " in {

    val future = database.policyMappingTable.storePolicy(policy)
    whenReady(future) { result =>
      result isExhausted () mustBe true
      result wasApplied () mustBe true
      result one ()
    }
  }

  val policyAppRequest21 = AppRequest(
    messageId20,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "createParkingPolicy",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(
        Some(policyId20),
        None,
        Some(policy.version),
        Some(parkingGroupId3),
        Some("98676457978"),
        Some("98676459778"),
        None,
        None,
        None,
        tagsPolicyLinkRequest,
        Some(policyRequestWithConflictingTimRangeWithinPolicyRule),
        None,
        None,
        None
      ),
      AppUserDataProps(None, "", "", None)
    )
  )

  val policyAppRequest22 = AppRequest(
    messageId20,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "createParkingPolicy",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(
        Some(policyId20),
        None,
        Some(policy.version),
        Some(parkingGroupId3),
        Some("98676457978"),
        Some("98676459778"),
        None,
        None,
        None,
        None,
        Some(policyRequestWithNonConflictingTimRangeWithinPolicyRule),
        None,
        None,
        None
      ),
      AppUserDataProps(None, "", "", None)
    )
  )

  val policyAppRequest23 = AppRequest(
    messageId20,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "createParkingPolicy",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(
        Some(policyId20),
        None,
        Some(policy.version),
        Some(parkingGroupId3),
        Some("98676457978"),
        Some("98676459778"),
        None,
        None,
        None,
        None,
        Some(mutiplePolicRulesWithConflictingTimeRange),
        None,
        None,
        None
      ),
      AppUserDataProps(None, "", "", None)
    )
  )

  val policyAppRequest24 = AppRequest(
    messageId20,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "updateParkingPolicy",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(
        Some(policyId20),
        None,
        Some(policy.version),
        Some(parkingGroupId3),
        Some("98676457978"),
        Some("98676459778"),
        None,
        None,
        None,
        None,
        Some(policyRequestWithNonConflictingTimRangeWithinPolicyRule),
        None,
        None,
        None
      ),
      AppUserDataProps(None, "", "", None)
    )
  )

  val policyAppRequest25 = AppRequest(
    messageId20,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "updateParkingPolicy",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(
        Some(policyId20),
        None,
        Some(policy.version),
        Some(parkingGroupId3),
        Some("98676457978"),
        Some("98676459778"),
        None,
        None,
        None,
        None,
        Some(policyRequestWithConflictingTimRangeWithinPolicyRule),
        None,
        None,
        None
      ),
      AppUserDataProps(None, "", "", None)
    )
  )

  val policyAppRequest26 = AppRequest(
    messageId20,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "updateParkingPolicy",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(
        Some(policyId20),
        None,
        Some(policy.version),
        Some(parkingGroupId3),
        Some("98676457978"),
        Some("98676459778"),
        None,
        None,
        None,
        None,
        Some(mutiplePolicRulesWithConflictingTimeRange),
        None,
        None,
        None
      ),
      AppUserDataProps(None, "", "", None)
    )
  )

  /* Method: postParkingPolicyProcess */
  "postParkingPolicyProcess" should "return Success" in {

    doReturn(Future(SuccessMessage(true))).when(dbLayer).storePolicy(policy, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.postParkingPolicyProcess(orgId, siteId, policyAppRequest1, productionApi.value)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId1))
    }
  }

  "postParkingPolicyProcess" should "return Error \"Policy not found in the request\"" in {

    doReturn(Future(SuccessMessage(false))).when(dbLayer).storePolicy(policy, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.postParkingPolicyProcess(orgId, siteId, policyAppRequest, productionApi.value)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }
  }

  /* Method: postParkingPolicyProcess */
  "getParkingProcess" should "return Success" in {

    doReturn(Future(10)).when(dbLayer).getMaxVersionPolicyByIdOrgSite(policyId1, orgId, siteId, productionApi.value)
    doReturn(Future(List(policy.copy(uid = policyId1))))
      .when(dbLayer)
      .getPolicyByIdAndVersion(policyId1, 10, orgId, siteId, productionApi.value)
    doReturn(Future(List("pg1", "pg2"))).when(dbLayer).getAssociatedParkingGroupsForPolicy(policyId1)

    val chain = for {
      appResponse <- policyHelper.getParkingProcess(orgId, siteId, policyAppRequest1, productionApi.value)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[PolicyResponse]].messageid.equals(messageId1))
    }
  }

  "getParkingProcess" should "return Error - \"Policy not found with id\"" in {

    val messageId_ForErr = UUID.randomUUID().toString
    val policyId_ForErr  = UUID.randomUUID().toString
    val policyAppRequest_ForErr = AppRequest(
      messageId_ForErr,
      resTopic,
      new RequestBody(
        instanceId,
        requestId,
        Calendar.getInstance().toInstant.toString,
        "policyTagsAssociation",
        "NoneModel",
        "CAN_READ",
        Some("user"),
        OrgProps(orgId),
        SiteProps(siteId),
        ConfigProps(Some(policyId_ForErr),
                    None,
                    None,
                    Some(parkingGroupId1),
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    Some(policyRequest),
                    None,
                    None,
                    None),
        AppUserDataProps(None, "", "", None)
      )
    )

    doReturn(Future(List(policy)))
      .when(dbLayer)
      .getPolicyByIdAndVersion(policyId_ForErr, 10, orgId, siteId, productionApi.value)
    doReturn(Future(9))
      .when(dbLayer)
      .getMaxVersionPolicyByIdOrgSite(policyId_ForErr, orgId, siteId, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.getParkingProcess(orgId, siteId, policyAppRequest_ForErr, productionApi.value)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId_ForErr))
    }
  }

  /* Method: postParkingPolicyProcess */
  "getPolicyIdAndVersion" should "return Success" in {

    doReturn(Future(List(policy)))
      .when(dbLayer)
      .getPolicyByIdAndVersion(policyId1, 10, orgId, siteId, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.getPolicyIdAndVersion(policyId1, Future(10), orgId, siteId, productionApi.value)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.toString().contains(uid1))
    }
  }

  "getPolicyIdAndVersion" should "return Empty List of Policy" in {

    val messageId_ForErr = UUID.randomUUID().toString
    val policyId_ForErr  = UUID.randomUUID().toString
    val policyAppRequest_ForErr = AppRequest(
      messageId_ForErr,
      resTopic,
      new RequestBody(
        instanceId,
        requestId,
        Calendar.getInstance().toInstant.toString,
        "policyTagsAssociation",
        "NoneModel",
        "CAN_READ",
        Some("user"),
        OrgProps(orgId),
        SiteProps(siteId),
        ConfigProps(Some(policyId_ForErr),
                    None,
                    None,
                    Some(parkingGroupId1),
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    Some(policyRequest),
                    None,
                    None,
                    None),
        AppUserDataProps(None, "", "", None)
      )
    )

    doReturn(Future(List(policy)))
      .when(dbLayer)
      .getPolicyByIdAndVersion(policyId_ForErr, 10, orgId, siteId, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.getPolicyIdAndVersion(policyId_ForErr, Future(11), orgId, siteId, productionApi.value)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.isEmpty)
    }
  }

  /* Method: postParkingPolicyProcess */
  "getAllParkingProcess" should "return Success" in {

    doReturn(Future(List(policy))).when(dbLayer).getAllPolicies(orgId, siteId, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.getAllParkingProcess(orgId, siteId, policyAppRequest1, productionApi.value)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId1))
    }
  }

  "getAllParkingProcess" should "with no PolicyID" in {

    // This method always returns Success Response
    val messageId_ForErr = UUID.randomUUID().toString
    val policyAppRequest_ForErr = AppRequest(
      messageId_ForErr,
      resTopic,
      new RequestBody(
        instanceId,
        requestId,
        Calendar.getInstance().toInstant.toString,
        "policyTagsAssociation",
        "NoneModel",
        "CAN_READ",
        Some("user"),
        OrgProps(orgId),
        SiteProps(siteId),
        ConfigProps(None,
                    None,
                    None,
                    Some(parkingGroupId1),
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None),
        AppUserDataProps(None, "", "", None)
      )
    )

    val chain = for {
      appResponse <- policyHelper.getAllParkingProcess(orgId, siteId, policyAppRequest_ForErr, productionApi.value)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[Policy]].response.success.equals(true))
    }
  }

  /* Method: updateParkingProcess */
  "updateParkingProcess" should "return Error - \"Operation failed because policy is linked to some group\" " in {

    doReturn(Future(List(parkingGroupPolicyLink))).when(dbLayer).getPolicyByIdInTimeline(policyId1, productionApi.value)
    doReturn(Future(List(policy))).when(dbLayer).getAllPolicies(orgId, siteId, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.updateParkingProcess(orgId, siteId, policyAppRequest1, productionApi.value)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId1))
    }
  }

  "updateParkingProcess" should "return Success " in {

    doReturn(Future(List(None))).when(dbLayer).getPolicyByIdInTimeline(policyId, productionApi.value)
    doReturn(Future(List(policy))).when(dbLayer).getAllPolicies(orgId, siteId, productionApi.value)
    doReturn(Future(List(policy))).when(dbLayer).getAllLivePoliciesById(policyId2, orgId, siteId, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.updateParkingProcess(orgId, siteId, policyAppRequest2, productionApi.value)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId2))
    }
  }

  /* Method: checkTimelineForPolicy */
  "checkTimelineForPolicy" should "return Failure - \"Policy not found with id\"" in {

    doReturn(Future(List(None))).when(dbLayer).getPolicyByIdInTimeline(policyId, productionApi.value)
    doReturn(Future(List(policy))).when(dbLayer).getAllLivePoliciesById(policyId, orgId, siteId, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.checkTimelineForPolicy(policyId1,
                                                         policyAppRequest1,
                                                         policyRequest,
                                                         orgId,
                                                         siteId,
                                                         productionApi.value)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId1))
    }
  }

  "checkTimelineForPolicy" should "return Success " in {

    doReturn(Future(List(None))).when(dbLayer).getPolicyByIdInTimeline(policyId, productionApi.value)
    doReturn(Future(List(policy))).when(dbLayer).getAllLivePoliciesById(policyId3, orgId, siteId, productionApi.value)
    doReturn(Future(SuccessMessage(true))).when(dbLayer).updateByPolicyId(policyId3, 11, policy, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.checkTimelineForPolicy(policyId3,
                                                         policyAppRequest3,
                                                         policyRequest,
                                                         orgId,
                                                         siteId,
                                                         productionApi.value)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId3))
    }
  }

  /* Method: getMaxVerionOfPolicyById */
  "getMaxVerionOfPolicyById" should "return Success " in {

    doReturn(Future(List(policy))).when(dbLayer).getAllLivePoliciesById(policyId4, orgId, siteId, productionApi.value)
    doReturn(Future(SuccessMessage(true))).when(dbLayer).updateByPolicyId(policyId4, 11, policy, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.getMaxVerionOfPolicyById(policyId4,
                                                           policyAppRequest4,
                                                           policyRequest,
                                                           orgId,
                                                           siteId,
                                                           productionApi.value)
    } yield appResponse

    whenReady(chain, timeout(Span(5, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId4))
    }
  }

  /* Method: updateDBPolicyAndGenerateResponse1 */
  "updateDBPolicyAndGenerateResponse1" should "return Success " in {

    doReturn(Future(SuccessMessage(true))).when(dbLayer).updateByPolicyId(policyId4, 11, policy, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.updateDBPolicyAndGenerateResponse1(policyAppRequest1,
                                                                     policy,
                                                                     policyRequest,
                                                                     policyId1,
                                                                     productionApi.value)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId1))
    }
  }

  "updateDBPolicyAndGenerateResponse1" should "return Success when empty UID is passed for Policy" in {

    val messageId_ForErr = UUID.randomUUID().toString
    val policyId_ForErr  = UUID.randomUUID().toString

    val policyAppRequest_ForErr = AppRequest(
      messageId_ForErr,
      resTopic,
      new RequestBody(
        instanceId,
        requestId,
        Calendar.getInstance().toInstant.toString,
        "policyTagsAssociation",
        "NoneModel",
        "CAN_READ",
        Some("user"),
        OrgProps(""),
        SiteProps(siteId),
        ConfigProps(None, None, None, None, None, None, None, None, None, None, None, None, None, None),
        AppUserDataProps(None, "", "", None)
      )
    )

    val policy_Err: Policy = Policy("",
                                    "",
                                    "",
                                    "",
                                    Set(""),
                                    Set(policyLvlViolation),
                                    false,
                                    "",
                                    14986863999977L,
                                    14986963999977L,
                                    false,
                                    10,
                                    null,
                                    Some(""),
                                    "",
                                    Some(""),
                                    Set(policyRule))

    doReturn(Future(SuccessMessage(false)))
      .when(dbLayer)
      .updateByPolicyId(policyId_ForErr, 11, policy_Err, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.updateDBPolicyAndGenerateResponse1(policyAppRequest_ForErr,
                                                                     policy_Err,
                                                                     policyRequest,
                                                                     policyId_ForErr,
                                                                     productionApi.value)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(
        res
          .asInstanceOf[AppSuccessResponse[AppRequest]]
          .response
          .result
          .asInstanceOf[PolicyResponse]
          .policyId
          .equals("")
      )
    }
  }

  /* Method: updatePolicyTags */
  "updatePolicyTags" should "return Success " in {

    doReturn(Future(List(policy5))).when(dbLayer).getAllLivePoliciesById(policyId5, orgId, siteId, productionApi.value)
    doReturn(Future(List("tags")))
      .when(dbLayer)
      .getExistingTagIds(policyAppRequest5.request.configprops.tagspolicylink.get.tagids.toList,
                         orgId,
                         siteId,
                         productionApi.value)
    doReturn(Future(SuccessMessage(true))).when(dbLayer).updateByPolicyId(policyId5, 11, policy, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.updatePolicyTags(orgId, siteId, policyAppRequest5, productionApi.value)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId5))
    }
  }

  "updatePolicyTags" should "return Failure Error - \"Requested TagIds not available in Database\" " in {

    val messageId_ForErr             = UUID.randomUUID().toString
    val policyId_ForErr              = UUID.randomUUID().toString
    val tagsPolicyLinkRequest_ForErr = Some(TagsPolicyLinkRequest(Set("tags_err")))

    val policyAppRequest_ForErr = AppRequest(
      messageId_ForErr,
      resTopic,
      new RequestBody(
        instanceId,
        requestId,
        Calendar.getInstance().toInstant.toString,
        "policyTagsAssociation",
        "NoneModel",
        "CAN_READ",
        Some("user"),
        OrgProps(orgId),
        SiteProps(siteId),
        ConfigProps(Some(policyId_ForErr),
                    None,
                    None,
                    Some(parkingGroupId1),
                    None,
                    None,
                    None,
                    None,
                    None,
                    tagsPolicyLinkRequest_ForErr,
                    Some(policyRequest),
                    None,
                    None,
                    None),
        AppUserDataProps(None, "", "", None)
      )
    )

    val policy_Err: Policy = Policy(policyId_ForErr,
                                    "",
                                    "",
                                    "",
                                    Set(""),
                                    Set(policyLvlViolation),
                                    false,
                                    "",
                                    14986863999977L,
                                    14986963999977L,
                                    false,
                                    10,
                                    "",
                                    Some(""),
                                    "",
                                    Some(""),
                                    Set(policyRule))

    doReturn(Future(List(policy_Err)))
      .when(dbLayer)
      .getAllLivePoliciesById(policyId_ForErr, orgId, siteId, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.updatePolicyTags(orgId, siteId, policyAppRequest_ForErr, productionApi.value)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId_ForErr))
    }
  }

  /* Method: getLatestPolicyAndUpdateTags */
  "getLatestPolicyAndUpdateTags" should "return Success " in {

    doReturn(Future(List(policy))).when(dbLayer).getAllLivePoliciesById(policyId6, orgId, siteId, productionApi.value)
    doReturn(Future(List("tags")))
      .when(dbLayer)
      .getExistingTagIds(policyAppRequest6.request.configprops.tagspolicylink.get.tagids.toList,
                         orgId,
                         siteId,
                         productionApi.value)
    doReturn(Future(SuccessMessage(true))).when(dbLayer).updateByPolicyId(policyId6, 11, policy, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.getLatestPolicyAndUpdateTags(policyAppRequest6,
                                                               policyId6,
                                                               orgId,
                                                               siteId,
                                                               productionApi.value)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId6))
    }
  }

  "getLatestPolicyAndUpdateTags" should "return Failure - \"Requested TagIds not available in Database\"" in {

    val messageId_ForErr             = UUID.randomUUID().toString
    val policyId_ForErr              = UUID.randomUUID().toString
    val tagsPolicyLinkRequest_ForErr = Some(TagsPolicyLinkRequest(Set("tags_err")))

    val policyAppRequest_ForErr = AppRequest(
      messageId_ForErr,
      resTopic,
      new RequestBody(
        instanceId,
        requestId,
        Calendar.getInstance().toInstant.toString,
        "policyTagsAssociation",
        "NoneModel",
        "CAN_READ",
        Some("user"),
        OrgProps(orgId),
        SiteProps(siteId),
        ConfigProps(Some(policyId_ForErr),
                    None,
                    None,
                    Some(parkingGroupId1),
                    None,
                    None,
                    None,
                    None,
                    None,
                    tagsPolicyLinkRequest_ForErr,
                    Some(policyRequest),
                    None,
                    None,
                    None),
        AppUserDataProps(None, "", "", None)
      )
    )

    val policy_Err: Policy = Policy(policyId_ForErr,
                                    "",
                                    "",
                                    "",
                                    Set(""),
                                    Set(policyLvlViolation),
                                    false,
                                    "",
                                    14986863999977L,
                                    14986963999977L,
                                    false,
                                    10,
                                    "",
                                    Some(""),
                                    "",
                                    Some(""),
                                    Set(policyRule))

    doReturn(Future(List(policy_Err)))
      .when(dbLayer)
      .getAllLivePoliciesById(policyId_ForErr, orgId, siteId, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.getLatestPolicyAndUpdateTags(policyAppRequest_ForErr,
                                                               policyId_ForErr,
                                                               orgId,
                                                               siteId,
                                                               productionApi.value)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId_ForErr))
    }
  }

  /* Method: updatePolicyWithTags */
  "updatePolicyWithTags" should "return Success " in {

    doReturn(Future(List(policy))).when(dbLayer).getAllLivePoliciesById(policyId7, orgId, siteId, productionApi.value)
    doReturn(Future(SuccessMessage(true))).when(dbLayer).updateByPolicyId(policyId7, 11, policy, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.updatePolicyWithTags(policyAppRequest7,
                                                       policy,
                                                       policyId7,
                                                       orgId,
                                                       siteId,
                                                       productionApi.value)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId7))
    }
  }

  /* Method: addTagsToPolicy */
  "addTagsToPolicy" should "return Success " in {

    doReturn(Future(List(policy))).when(dbLayer).getAllLivePoliciesById(policyId8, orgId, siteId, productionApi.value)
    doReturn(Future(List("tags")))
      .when(dbLayer)
      .getExistingTagIds(policyAppRequest8.request.configprops.tagspolicylink.get.tagids.toList,
                         orgId,
                         siteId,
                         productionApi.value)
    doReturn(Future(SuccessMessage(true))).when(dbLayer).updateByPolicyId(policyId8, 11, policy, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.addTagsToPolicy(policyAppRequest8,
                                                  policy,
                                                  policyId8,
                                                  orgId,
                                                  siteId,
                                                  productionApi.value)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId8))
    }
  }

  /* Method: removeTagsFromPolicy */
  "removeTagsFromPolicy" should "return Success " in {

    doReturn(Future(List(policy))).when(dbLayer).getAllLivePoliciesById(policyId9, orgId, siteId, productionApi.value)
    doReturn(Future(List("tags")))
      .when(dbLayer)
      .getExistingTagIds(policyAppRequest9.request.configprops.tagspolicylink.get.tagids.toList,
                         orgId,
                         siteId,
                         productionApi.value)
    doReturn(Future(SuccessMessage(true))).when(dbLayer).updateByPolicyId(policyId9, 11, policy, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.removeTagsFromPolicy(policyAppRequest9,
                                                       policy,
                                                       policyId9,
                                                       orgId,
                                                       siteId,
                                                       productionApi.value)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId9))
    }
  }

  "removeTagsFromPolicy" should "return Success with most of the policy values set to empty String " in {

    val messageId_ForErr = UUID.randomUUID().toString
    val policyId_ForErr  = UUID.randomUUID().toString

    val policyAppRequest_ForErr = AppRequest(
      messageId_ForErr,
      resTopic,
      new RequestBody(
        instanceId,
        requestId,
        Calendar.getInstance().toInstant.toString,
        "policyTagsAssociation",
        "NoneModel",
        "CAN_READ",
        Some("user"),
        OrgProps(orgId),
        SiteProps(siteId),
        ConfigProps(Some(policyId_ForErr),
                    None,
                    None,
                    Some(parkingGroupId1),
                    None,
                    None,
                    None,
                    None,
                    None,
                    tagsPolicyLinkRequest,
                    Some(policyRequest),
                    None,
                    None,
                    None),
        AppUserDataProps(None, "", "", None)
      )
    )

    val policy_Err: Policy = Policy(policyId_ForErr,
                                    "",
                                    "",
                                    "",
                                    Set(""),
                                    Set(policyLvlViolation),
                                    false,
                                    "",
                                    14986863999977L,
                                    14986963999977L,
                                    false,
                                    10,
                                    "",
                                    Some(""),
                                    "",
                                    Some(""),
                                    Set(policyRule))

    doReturn(Future(SuccessMessage(true))).when(dbLayer).updateByPolicyId(policyId10, 11, policy, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.removeTagsFromPolicy(policyAppRequest_ForErr,
                                                       policy_Err,
                                                       policyId_ForErr,
                                                       orgId,
                                                       siteId,
                                                       productionApi.value)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId_ForErr))
    }
  }

  /* Method: upsertPolicyTags */
  "upsertPolicyTags" should "return Success " in {

    doReturn(Future(List(policy))).when(dbLayer).getAllLivePoliciesById(policyId10, orgId, siteId, productionApi.value)
    doReturn(Future(List("tags")))
      .when(dbLayer)
      .getExistingTagIds(policyAppRequest10.request.configprops.tagspolicylink.get.tagids.toList,
                         orgId,
                         siteId,
                         productionApi.value)
    doReturn(Future(SuccessMessage(true))).when(dbLayer).updateByPolicyId(policyId10, 11, policy, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.upsertPolicyTags(policyAppRequest10, policy, policyId10, None, productionApi.value)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId10))
    }
  }

  /* Method: deletePolicyProcess */
  "deletePolicyProcess" should "return Success " in {

    val lst_ParkingGroupPolicyLink: List[ParkingGroupPolicyLink] = Nil

    doReturn(Future(lst_ParkingGroupPolicyLink)).when(dbLayer).getPolicyByIdInTimeline(policyId11, productionApi.value)
    doReturn(Future(10)).when(dbLayer).getMaxVersionPolicyByIdOrgSite(policyId11, orgId, siteId, productionApi.value)
    doReturn(Future(List(policy)))
      .when(dbLayer)
      .getPolicyByIdAndVersion(policyId11, 10, orgId, siteId, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.deletePolicyProcess(orgId, siteId, policyAppRequest11, productionApi.value)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId11))
    }
  }

  /* Method: deletePolicyFromDBandGenerateResponse */
  "deletePolicyFromDBandGenerateResponse" should "return Success " in {

    doReturn(Future(List(policy))).when(dbLayer).getAllPoliciesById(policyId12, orgId, siteId, productionApi.value)
    doReturn(Future(SuccessMessage(true)))
      .when(dbLayer)
      .deleteByPolicyIdAndVersion(policyId12, policy.version, orgId, siteId, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.deletePolicyFromDBandGenerateResponse(policyAppRequest12,
                                                                        policyId12,
                                                                        orgId,
                                                                        siteId,
                                                                        productionApi.value)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId12))
    }
  }

  /* Method: getVersionHistoryProcess */
  "getVersionHistoryProcess" should "return Success " in {

    doReturn(Future(List(policy))).when(dbLayer).getVersionHistoryById(policyId13, orgId, siteId)

    val chain = for {
      appResponse <- policyHelper.getVersionHistoryProcess(orgId, siteId, policyAppRequest13)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId13))
    }
  }

  /* Method: getVersionHistoryProcess */
  "getPolicyByVersionNumberAndId" should "return Success " in {

    doReturn(Future(List(policy)))
      .when(dbLayer)
      .getPolicyByIdAndVersion(policyId14, policy.version, orgId, siteId, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.getPolicyByVersionNumberAndId(orgId, siteId, policyAppRequest14)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId14))
    }
  }

  "getPolicyByVersionNumberAndId" should "return Failure Response error - \"Required version number missing in request\" " in {

    val messageId_ForErr = UUID.randomUUID().toString
    val policyId_ForErr  = UUID.randomUUID().toString

    val policyAppRequest_ForErr = AppRequest(
      messageId_ForErr,
      resTopic,
      new RequestBody(
        instanceId,
        requestId,
        Calendar.getInstance().toInstant.toString,
        "policyTagsAssociation",
        "NoneModel",
        "CAN_READ",
        Some("user"),
        OrgProps(orgId),
        SiteProps(siteId),
        ConfigProps(Some(policyId_ForErr),
                    None,
                    None,
                    Some(parkingGroupId1),
                    None,
                    None,
                    None,
                    None,
                    None,
                    tagsPolicyLinkRequest,
                    Some(policyRequest),
                    None,
                    None,
                    None),
        AppUserDataProps(None, "", "", None)
      )
    )

    val chain = for {
      appResponse <- policyHelper.getPolicyByVersionNumberAndId(orgId, siteId, policyAppRequest_ForErr)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId_ForErr))
    }
  }

  /* Method: searchPolicyByNameOrTagProcess */
  "searchPolicyByNameOrTagProcess" should "return Success " in {

    val name = "name"
    doReturn(Future(List(policy))).when(dbLayer).getAllPoliciesByNameWithinOrg(name, orgId, siteId)

    val chain = for {
      appResponse <- policyHelper.searchPolicyByNameOrTagProcess(orgId, siteId, policyAppRequest15)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId15))
    }
  }

  "searchPolicyByNameOrTagProcess" should "return Failure Response error - \"Required policyid missing in request\"" in {

    val messageId_ForErr = UUID.randomUUID().toString
    val policyId_ForErr  = UUID.randomUUID().toString

    val policyAppRequest_ForErr = AppRequest(
      messageId_ForErr,
      resTopic,
      new RequestBody(
        instanceId,
        requestId,
        Calendar.getInstance().toInstant.toString,
        "policyTagsAssociation",
        "NoneModel",
        "CAN_READ",
        Some("user"),
        OrgProps(orgId),
        SiteProps(siteId),
        ConfigProps(Some(policyId_ForErr),
                    None,
                    None,
                    Some(parkingGroupId1),
                    None,
                    None,
                    None,
                    None,
                    None,
                    tagsPolicyLinkRequest,
                    Some(policyRequest),
                    None,
                    None,
                    None),
        AppUserDataProps(None, "", "", None)
      )
    )

    val chain = for {
      appResponse <- policyHelper.searchPolicyByNameOrTagProcess(orgId, siteId, policyAppRequest_ForErr)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId_ForErr))
    }
  }

  /* Method: searchDBWithNameOrTagid */
  "searchDBWithNameOrTagid" should "return Success " in {

    val srchPolicyByNameOrTagid = SearchPolicyByNameOrTagid(Some("nameN"), None)

    val chain = for {
      appResponse <- policyHelper.searchDBWithNameOrTagid(srchPolicyByNameOrTagid, orgId, siteId)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.isEmpty)
    }
  }

  /* Method: getActivePolicyProcess */
  "getActivePolicyProcess" should "return Success " in {

    val parkingGroupPolicyLink = ParkingGroupPolicyLink(uid1, parkingGroupId3, policyId16, 10, 1506632586L, 1506630000L)

    doReturn(Future(Some(parkingGroupPolicyLink))).when(dbLayer).getGroupPolicyByGroupId(parkingGroupId3)
    doReturn(Future(List(policy)))
      .when(dbLayer)
      .getPolicyByIdAndVersion(parkingGroupPolicyLink.policyid,
                               parkingGroupPolicyLink.version,
                               orgId,
                               siteId,
                               productionApi.value)

    val chain = for {
      appResponse <- policyHelper.getActivePolicyProcess(orgId, siteId, policyAppRequest16)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId16))
    }
  }

  "getActivePolicyProcess" should "return Failure error - \"Specified parking group is not associated with any policy\" " in {

    val messageId_ForErr = UUID.randomUUID().toString
    val policyId_ForErr  = UUID.randomUUID().toString

    val policyAppRequest_ForErr = AppRequest(
      messageId_ForErr,
      resTopic,
      new RequestBody(
        instanceId,
        requestId,
        Calendar.getInstance().toInstant.toString,
        "policyTagsAssociation",
        "NoneModel",
        "CAN_READ",
        Some("user"),
        OrgProps(orgId),
        SiteProps(siteId),
        ConfigProps(Some(policyId_ForErr),
                    None,
                    None,
                    Some(parkingGroupId1),
                    None,
                    None,
                    None,
                    None,
                    None,
                    tagsPolicyLinkRequest,
                    Some(policyRequest),
                    None,
                    None,
                    None),
        AppUserDataProps(None, "", "", None)
      )
    )

    val parkingGroupPolicyLink =
      ParkingGroupPolicyLink(uid1, parkingGroupId3, policyId_ForErr, 10, 1506632586L, 1506630000L)

    doReturn(Future(Some(parkingGroupPolicyLink))).when(dbLayer).getGroupPolicyByGroupId(parkingGroupId3)

    val chain = for {
      appResponse <- policyHelper.getActivePolicyProcess(orgId, siteId, policyAppRequest_ForErr)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId_ForErr))
    }
  }

  /* Method: getPolicyByParkingGroupId */
  "getPolicyByParkingGroupId" should "return Success " in {

    val parkingGroupPolicyLink = ParkingGroupPolicyLink(uid1, parkingGroupId3, policyId17, 10, 1506632586L, 1506630000L)

    doReturn(Future(List(policy)))
      .when(dbLayer)
      .getPolicyByIdAndVersion(parkingGroupPolicyLink.policyid,
                               parkingGroupPolicyLink.version,
                               orgId,
                               siteId,
                               productionApi.value)

    val chain = for {
      appResponse <- policyHelper.getPolicyByParkingGroupId(orgId, siteId, parkingGroupPolicyLink, policyAppRequest17)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId17))
    }
  }

  "getPolicyByParkingGroupId" should "return Failure Response error \"Policy not found with id\"" in {

    val messageId_ForErr             = UUID.randomUUID().toString
    val uid_ForErr                   = UUID.randomUUID().toString
    val policyId_ForErr              = UUID.randomUUID().toString
    val tagsPolicyLinkRequest_ForErr = Some(TagsPolicyLinkRequest(Set("tags_errord")))

    val policyAppRequest_ForErr = AppRequest(
      messageId_ForErr,
      resTopic,
      new RequestBody(
        instanceId,
        requestId,
        Calendar.getInstance().toInstant.toString,
        "policyTagsAssociation",
        "NoneModel",
        "CAN_READ",
        Some("user"),
        OrgProps(orgId),
        SiteProps(siteId),
        ConfigProps(Some(policyId_ForErr),
                    None,
                    None,
                    Some(parkingGroupId1),
                    None,
                    None,
                    None,
                    None,
                    None,
                    tagsPolicyLinkRequest_ForErr,
                    Some(policyRequest),
                    None,
                    None,
                    None),
        AppUserDataProps(None, "", "", None)
      )
    )

    val parkingGroupPolicyLink =
      ParkingGroupPolicyLink(uid_ForErr, parkingGroupId1, policyId_ForErr, 10, 1506632586L, 1506630000L)

    val chain = for {
      appResponse <- policyHelper.getPolicyByParkingGroupId(orgId,
                                                            siteId,
                                                            parkingGroupPolicyLink,
                                                            policyAppRequest_ForErr)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId_ForErr))
    }
  }

  /* Method: getActivePoliciesInTimeline */
  "getActivePoliciesInTimeline" should "return Success " in {

    val parkingGroupPolicyLink = ParkingGroupPolicyLink(uid1, parkingGroupId3, policyId18, 10, 1506632586L, 1506630000L)

    doReturn(Future(List(parkingGroupPolicyLink))).when(dbLayer).getAllPoliciesWithGroupId(parkingGroupId3)

    val chain = for {
      appResponse <- policyHelper.getActivePoliciesInTimeline(policyAppRequest18)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId18))
    }
  }

  /* Method: queryDbForActivePoliciesInTimeline */
  "queryDbForActivePoliciesInTimeline" should "return Success " in {

    val parkingGroupPolicyLink = ParkingGroupPolicyLink(uid1, parkingGroupId3, policyId19, 10, 1506632586L, 1506630000L)

    doReturn(Future(List(parkingGroupPolicyLink))).when(dbLayer).getAllPoliciesWithGroupId(parkingGroupId3)

    val chain = for {
      appResponse <- policyHelper.queryDbForActivePoliciesInTimeline(parkingGroupPolicyLink.parkinggroupid,
                                                                     parkingGroupPolicyLink.startTime,
                                                                     parkingGroupPolicyLink.endTime,
                                                                     policyAppRequest19)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId19))
    }
  }

  /* Method: filterTimelinePolicies */
  "filterTimelinePolicies" should "return Success " in {

    val parkingGroupPolicyLink = ParkingGroupPolicyLink(uid1, parkingGroupId3, policyId19, 10, 1506632586L, 1506630000L)

    val chain = for {
      appResponse <- policyHelper.filterTimelinePolicies(List(parkingGroupPolicyLink),
                                                         parkingGroupPolicyLink.startTime,
                                                         parkingGroupPolicyLink.endTime)
    } yield appResponse

    chain.foreach(resp => assert(resp.asInstanceOf[ActivePolicyResponse].policyid.equals(policyId19)))

  }

  "filterTimelinePolicies" should "return Empty List" in {

    val policyId_ForErr = UUID.randomUUID().toString

    val parkingGroupPolicyLink =
      ParkingGroupPolicyLink(uid1, parkingGroupId3, policyId_ForErr, 10, 1506632586L, 1506630000L)

    val chain = for {
      appResponse <- policyHelper.filterTimelinePolicies(List(),
                                                         parkingGroupPolicyLink.startTime,
                                                         parkingGroupPolicyLink.endTime)
    } yield appResponse

    assert(chain.isEmpty.equals(true))

  }

  /* Method: logicToFindPolicyWithinTimeline */
  "logicToFindPolicyWithinTimeline" should "return Success " in {

    val Queryfromtime: Long = 1506632586L
    val Querytotime: Long   = 1506630000L
    val linkstarttime: Long = 1506622586L
    val linkendtime: Long   = -1L

    val result: Boolean =
      policyHelper.logicToFindPolicyWithinTimeline(Queryfromtime, Querytotime, linkstarttime, linkendtime)

    assert(result.equals(true))

  }

  /* Method: verify Time Range Conflicts */
  "timerangeConflictwithinPolicyRule" should "return success" in {
    val listOfConflicts: List[String] =
      policyHelper.validateTimeRangesWithinPolicyRule(policyRequestWithConflictingTimRangeWithinPolicyRule)
    listOfConflicts.size mustBe 2
  }

  "timeRangeWithNonConflictedPOlicyRule" should "return success" in {
    val listOfConflicts: List[String] =
      policyHelper.validateTimeRangesWithinPolicyRule(policyRequestWithNonConflictingTimRangeWithinPolicyRule)
    listOfConflicts.size mustBe 0
  }

  "policyWithoutPolicyRule" should "return success" in {
    val lisOfconflicts: List[String] = policyHelper.validateTimeRangesWithinPolicyRule(policywithoutPolicyRules)
    lisOfconflicts.size mustBe 0
  }

  "policyWithOnePolicyRule" should "return success" in {
    val lisOfconflicts: List[String] = policyHelper.validateTimeRangesWithinPolicyRule(policywithOnePolicyRule)
    lisOfconflicts.size mustBe 0
  }

  "mutiplePolicRulesWithConflictingTimeRange" should "return success" in {
    val lisOfconflicts: List[String] =
      policyHelper.validateTimeRangesWithinPolicyRule(mutiplePolicRulesWithConflictingTimeRange)
    lisOfconflicts.size mustBe 2
  }

  "postParkingPolicyWithConflictingTimeRange" should "return Success" in {

    doReturn(Future(SuccessMessage(true))).when(dbLayer).storePolicy(policy, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.postParkingPolicyProcess(orgId, siteId, policyAppRequest22, productionApi.value)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId20))
    }
  }

  "postParkingPolicywithNonConflictingTimeRangePolicyRule" should "return Time Range Conflicting Error" in {

    doReturn(Future(SuccessMessage(false))).when(dbLayer).storePolicy(policy, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.postParkingPolicyProcess(orgId, siteId, policyAppRequest21, productionApi.value)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId20))
      assert(res.asInstanceOf[AppFailureResponse].response.status.equals(400))
      assert(
        res
          .asInstanceOf[AppFailureResponse]
          .response
          .error
          .equalsIgnoreCase(
            "Rules of the same priority cannot overlap in time: There are overlaps in timeranges between (06:00:00 - 10:00:00) and (07:00:00 - 08:00:00) for rulename: Policy , (06:00:00 - 10:00:00) and (08:00:00 - 10:00:00) for rulename: Policy"
          )
      )
    }
  }

  "postParkingPolicywithConflictingTimeRangeWithMultiplePolicyRule" should "return Time Range Conflicting Error" in {

    doReturn(Future(SuccessMessage(false))).when(dbLayer).storePolicy(policy, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.postParkingPolicyProcess(orgId, siteId, policyAppRequest23, productionApi.value)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId20))
      assert(res.asInstanceOf[AppFailureResponse].response.status.equals(400))
      assert(
        res
          .asInstanceOf[AppFailureResponse]
          .response
          .error
          .equalsIgnoreCase(
            "Rules of the same priority cannot overlap in time: There are overlaps in timeranges between (06:00:00 - 10:00:00) and (07:00:00 - 08:00:00) for rulename: Policy , (06:00:00 - 10:00:00) and (08:00:00 - 10:00:00) for rulename: Policy"
          )
      )
    }
  }

  "updateParkingProcesswithConflictingTimeRange" should "return Time Range Conflicting Error " in {

    doReturn(Future(List(parkingGroupPolicyLink))).when(dbLayer).getPolicyByIdInTimeline(policyId1, productionApi.value)
    doReturn(Future(List(policy))).when(dbLayer).getAllPolicies(orgId, siteId, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.updateParkingProcess(orgId, siteId, policyAppRequest25, productionApi.value)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId20))
      assert(
        res
          .asInstanceOf[AppFailureResponse]
          .response
          .error
          .equalsIgnoreCase(
            "Rules of the same priority cannot overlap in time: There are overlaps in timeranges between (06:00:00 - 10:00:00) and (07:00:00 - 08:00:00) for rulename: Policy , (06:00:00 - 10:00:00) and (08:00:00 - 10:00:00) for rulename: Policy"
          )
      )
    }
  }

  "updateParkingProcesswithConflictingTimeRangeWithMutiplePolicyRules" should "return Time Range Conflicting Error " in {

    doReturn(Future(List(parkingGroupPolicyLink))).when(dbLayer).getPolicyByIdInTimeline(policyId1, productionApi.value)
    doReturn(Future(List(policy))).when(dbLayer).getAllPolicies(orgId, siteId, productionApi.value)

    val chain = for {
      appResponse <- policyHelper.updateParkingProcess(orgId, siteId, policyAppRequest26, productionApi.value)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      print(res)
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId20))
      println(
        " updateParkingProcesswithConflictingTimeRangeWithMutiplePolicyRules ===>  " + res
          .asInstanceOf[AppFailureResponse]
          .response
          .error
      )
      assert(
        res
          .asInstanceOf[AppFailureResponse]
          .response
          .error
          .equalsIgnoreCase(
            "Rules of the same priority cannot overlap in time: There are overlaps in timeranges between (06:00:00 - 10:00:00) and (07:00:00 - 08:00:00) for rulename: Policy , (06:00:00 - 10:00:00) and (08:00:00 - 10:00:00) for rulename: Policy"
          )
      )
    }
  }

}
