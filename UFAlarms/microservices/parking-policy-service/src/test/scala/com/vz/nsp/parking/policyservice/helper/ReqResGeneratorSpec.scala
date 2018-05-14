package com.vz.nsp.parking.policyservice.helper

import java.util.{Calendar, UUID}

import com.vz.nsp.parking.model._
import com.vz.nsp.parking.policyservice.util.BaseSpec

/*
* Created by nagarna on 9/28/17.
*/

class ReqResGeneratorSpec extends BaseSpec {

  val uid = UUID.randomUUID().toString
  val messageId = UUID.randomUUID().toString
  val orgId = UUID.randomUUID().toString
  val siteId = UUID.randomUUID().toString
  val requestId = UUID.randomUUID().toString
  val parkingGroupId = UUID.randomUUID().toString
  val policyId = UUID.randomUUID().toString

  val instanceId: String = "1"
  val resTopic: String = "responseTopic"
  val resultBody: Int = 1

  val appRequest = AppRequest(messageId, resTopic,
    new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString, "associatedParkingGroups",
      "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(Some(policyId), None, None, Some(parkingGroupId), None, None, None, None, None, None,
        None, None, None, None), AppUserDataProps(None, "", "", None)))

  private implicit val ec = scala.concurrent.ExecutionContext.Implicits.global


  "failurecaseResponse" should "Return Success Message" in {

    val output = ReqResGenerator.failurecaseResponse(appRequest, 500, "Error: failurecaseResponse!!!")

    assert(output.isInstanceOf[SuccessMessage].!=(true))

  }

  "generateAppFailureResponse" should "Return Failure Response" in {

    val output = ReqResGenerator.generateAppFailureResponse(appRequest, 500, "Error: generateAppFailureResponse!!!")

    assert(output.isInstanceOf[AppFailureResponse].!=(false))

  }

  "failurecaseResponseForIrrelevantRequestType" should "Return Success \"AppFailureResponse\"" in {

    val output = ReqResGenerator.failurecaseResponseForIrrelevantRequestType(500, "Error: failurecaseResponseForIrrelevantRequestType!!!")

    assert(output.isInstanceOf[SuccessMessage].!=(true))

  }

  "successResponse" should "Return Success \"AppSuccessResponse\"" in {

    val output = ReqResGenerator.successResponse(appRequest, resultBody)

    assert(output.isInstanceOf[SuccessMessage].!=(true))

  }

  "failurecaseResponseWithoutFuture" should "Return Success \"AppSuccessResponse\"" in {

    val output = ReqResGenerator.failurecaseResponseWithoutFuture(appRequest, 500, "Error: failurecaseResponseWithoutFuture!!!")

    assert(output.isInstanceOf[AppFailureResponse].!=(false))

  }

  "successResponseWithoutFuture" should "Return Success Response" in {

    val output = ReqResGenerator.successResponseWithoutFuture(appRequest, resultBody)

    assert(output.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId))

  }

  val timeRange: TimeRange = TimeRange("06:00:00", "11:00:00")
  val valperiod: Period = Period("2017-06-28;19:31:03", "2018-07-28;19:31:03")
  val datePeriod: DatePeriod = DatePeriod(valperiod, "yearly")
  val parkingSchedule: ParkingSchedule = ParkingSchedule(Some("Schedule"), None, None, Some(datePeriod), Some(Set(timeRange)))
  val violation: ViolationFine = ViolationFine("ppv", 100.0, Some("once"))
  val parkingPenalty: ParkingPenalty = ParkingPenalty(Some("Penalty description"), Set(violation))
  val maxDuration: MaxDuration = MaxDuration(24.0, "hours")
  val parkingCharge: ParkingCharge = ParkingCharge(Some("Charge Description"), Some("Charge Description"), None, Some(maxDuration), 60.4)
  val parkingRule: ParkingRule = ParkingRule(Some("Rule"), true, Some(parkingCharge), Some(parkingPenalty))

  val policyRule: PolicyRule = PolicyRule(Some("Policy rule"), Some("Policy"), Some("Policy rule Description"), 12, parkingSchedule, parkingRule)

  val policyLvlViolation = PolicyLevelViolation("1234", "PolicyViolation-1", Some("Polcy Violation Description"),
    "ppv", 222.32, Some("once"))

  val policy: Policy = Policy(uid, "newupdateon 13 july", siteId, orgId, Set("Sunday policy"), Set(policyLvlViolation),
    false, "7653612563", 14986863999977L, 14986963999977L, false, 10, "new policy",
    Some("Policy description"), "Africa/Costarica", None, Set(policyRule))

  val policyRequest = PolicyRequest(Some(uid), Some("policyAuthorizerid"), Option(Set("Sunday policy")), Some(Set(policyLvlViolation)),
    "name", Some("timeZone"), Some("description"), Some(Set(policyRule)))

  "generatePolicyObject" should "Return Policy Object" in {

    val output = ReqResGenerator.generatePolicyObject(policyRequest, orgId, siteId,Some("userId"))

    assert(output.siteid.equals(siteId))

  }

  "updatePolicyObject" should "Return Policy Object" in {

    val output = ReqResGenerator.updatePolicyObject(policy, policyRequest,Some("userId"))

    assert(output.siteid.equals(siteId))

  }

  "generateTagObject" should "Return Tag Object" in {

    val tagRequest = TagRequest(Some(uid), "name", Some("description"))
    val output = ReqResGenerator.generateTagObject(tagRequest, orgId, siteId)

    assert(output.description.contains("description"))

  }

  "updateTagObject" should "Return Tag Object" in {

    val tagRequest = TagRequest(Some(uid), "name", Some("description"))
    val tag = Tag(UUID.randomUUID().toString, tagRequest.name, tagRequest.description.getOrElse(""),
      orgId, siteId, Calendar.getInstance().getTimeInMillis, 0, false)

    val output = ReqResGenerator.updateTagObject(tagRequest, tag)

    assert(output.isdeleted.equals(false))

  }

  "associateTagToPolicyObject" should "Return Tag Object" in {

    val tagsid = Set("tagids")

    val output = ReqResGenerator.associateTagToPolicyObject(policy, tagsid)

    assert(output.siteid.equals(siteId))

  }

}
