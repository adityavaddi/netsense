package com.vz.nsp.parking.policyservice.constants

import java.time.Instant
import java.util.{Calendar, UUID}

import com.vz.nsp.parking.model.{TagsPolicyLinkRequest, _}

/**
 * Created by donthgo on 7/12/17.
 */
object TestDataFeed {

  val name                   = "name"
  val messageId              = UUID.randomUUID().toString
  val instanceId             = UUID.randomUUID().toString
  val requestId              = UUID.randomUUID().toString
  val nodeId                 = UUID.randomUUID().toString
  val siteId                 = UUID.randomUUID().toString
  val orgId                  = UUID.randomUUID().toString
  val userId                 = UUID.randomUUID().toString
  val policytagId            = UUID.randomUUID().toString
  val policytagIdTwo         = UUID.randomUUID().toString
  val policyId               = UUID.randomUUID().toString
  val policyIdOne            = UUID.randomUUID().toString
  val policyIdTwo            = UUID.randomUUID().toString
  val policyIdThree          = UUID.randomUUID().toString
  val policyIdFour           = UUID.randomUUID().toString
  val policyIdFive           = UUID.randomUUID().toString
  val policyIdSix            = UUID.randomUUID().toString
  val policyIdSeven          = UUID.randomUUID().toString
  val sensorType             = "pc"
  val sampleValue            = 123123.0
  val callbackaddress        = "callbackaddress"
  val callbackuri            = "/dummy"
  val timestamp              = Calendar.getInstance().toInstant.toString
  val timeInMilliSecondsLong = System.currentTimeMillis()
  val limit                  = 120
  val `type`                 = "getTag"
  val model                  = "NodeModel"
  val action                 = "CAN_READ"
  val user                   = "d48dcd46-da6e-49b0-b76e-e3cbc79c4565"
  val sensorid               = "pc"
  val date1                  = "2017-06-03T22:54:46.670Z"
  val date2                  = "2099-06-19T22:55:16.737Z"
  val period                 = "15min"
  val timeInMillis           = Calendar.getInstance().getTimeInMillis
  val policyGroupUUID        = UUID.randomUUID().toString
  val parkingGroupId         = UUID.randomUUID().toString

  def generateAppRequestWithHeaders(responseTopic: String): AppRequest = {
    val orgProps  = OrgProps(orgId)
    val nodeProps = NodeProps(nodeId)
    val siteProps = SiteProps(siteId)
    val requestBody = RequestBody(
      instanceId,
      requestId,
      timestamp,
      `type`,
      model,
      action,
      Some("abc"),
      orgProps,
      siteProps,
      ConfigProps(Some("policyid"),
                  Some("policyid"),
                  Some(1),
                  None,
                  None,
                  None,
                  None,
                  None,
                  Some(tagFromUser),
                  None,
                  None,
                  None,
                  None,
                  None),AppUserDataProps(None,"","",None)
    )
    AppRequest(messageId,responseTopic, requestBody)
  }

  def generateAppRequestWithHeaders(responseTopic: String,
                                    reqtype: String,
                                    tagid: String,
                                    tagFromUser: TagRequest): AppRequest = {
    val orgProps  = OrgProps(orgId)
    val nodeProps = NodeProps(nodeId)
    val siteProps = SiteProps(siteId)
    val configProps = ConfigProps(None,
                                  Some(tagid),
                                  Some(1),
                                  None,
                                  None,
                                  None,
                                  None,
                                  None,
                                  Some(tagFromUser),
                                  None,
                                  None,
                                  None,
                                  None,
                                  None)
    val requestBody = RequestBody(instanceId,
                                  requestId,
                                  timestamp,
                                  reqtype,
                                  model,
                                  action,
                                  Some("abc"),
                                  orgProps,
                                  siteProps,
                                  configProps,AppUserDataProps(None,"","",None))
    AppRequest(messageId,responseTopic, requestBody)
  }


  def  generateAppRequestWithHeaders(responseTopic: String,
                                    reqtype: String,
                                    policyid: String,
                                    policyRequestFromUser: PolicyRequest): AppRequest = {
    val orgProps = OrgProps(orgId)
    val nodeProps = NodeProps(nodeId)
    val siteProps = SiteProps(siteId)
    val configProps =
      ConfigProps(Some(policyid),None, Some(10), Some(parkingGroupId),
        Some("1508963043000"), Some("1509395043000"), None, None, None,
        Some(TagsPolicyLinkRequest(Set(policytagId,"invalidTagId"))), Some(policyRequestFromUser), None, None, None)
    val requestBody = RequestBody(instanceId,
      requestId,
      timestamp,
      reqtype,
      model,
      action,
      Some("abc"),
      orgProps,
      siteProps,
      configProps,AppUserDataProps(None,"","",None))
    AppRequest(messageId, responseTopic, requestBody)
  }

  def  generateAppRequestWithHeadersWrongOrgidOrSiteid(responseTopic: String,
                                    reqtype: String,
                                    policyid: String,
                                    policyRequestFromUser: PolicyRequest,orgid:String,siteid:String): AppRequest = {
    val orgProps = OrgProps(orgid)
    val nodeProps = NodeProps(nodeId)
    val siteProps = SiteProps(siteid)
    val configProps =
      ConfigProps(Some(policyid),None, Some(10), Some(parkingGroupId),
        Some("1508963043000"), Some("1509395043000"), None, None, None,
        None, Some(policyRequestFromUser), None, None, None)
    val requestBody = RequestBody(instanceId,
      requestId,
      timestamp,
      reqtype,
      model,
      action,
      Some("abc"),
      orgProps,
      siteProps,
      configProps,AppUserDataProps(None,"","",None))
    AppRequest(messageId, responseTopic, requestBody)
  }

  def  generateAppRequestWithHeadersForCreatePolicyFailure(responseTopic: String,
                                     reqtype: String,
                                     policyid: String): AppRequest = {
    val orgProps = OrgProps(orgId)
    val nodeProps = NodeProps(nodeId)
    val siteProps = SiteProps(siteId)
    val configProps =
      ConfigProps(Some(policyid),None, Some(10), Some(parkingGroupId), Some("14986863999977"), Some("14986863999987"), None, None, None, None, None, None, None, None)
    val requestBody = RequestBody(instanceId,
      requestId,
      timestamp,
      reqtype,
      model,
      action,
      Some("abc"),
      orgProps,
      siteProps,
      configProps,AppUserDataProps(None,"","",None))
    AppRequest(messageId, responseTopic, requestBody)
  }

  def  generateAppRequestWithHeadersForGetAllParkingProcessFailure(responseTopic: String,
                                                           reqtype: String,
                                                           policyid: String,
                                                           orgID: String,
                                                           siteID: String): AppRequest = {
    val orgProps = OrgProps(orgID)
    val nodeProps = NodeProps(nodeId)
    val siteProps = SiteProps(siteID)
    val configProps =
      ConfigProps(Some(policyid),None, Some(10), Some(parkingGroupId), Some("14986863999977"), Some("14986863999987"), None, None, None, None, None, None, None, None)
    val requestBody = RequestBody(instanceId,
      requestId,
      timestamp,
      reqtype,
      model,
      action,
      Some("abc"),
      orgProps,
      siteProps,
      configProps,AppUserDataProps(None,"","",None))
    AppRequest(messageId, responseTopic, requestBody)
  }

  def  generateAppRequestWithHeadersForPolicyVersionFailure(responseTopic: String,
                                     reqtype: String,
                                     policyid: String,
                                     policyRequestFromUser: PolicyRequest): AppRequest = {
    val orgProps = OrgProps(orgId)
    val nodeProps = NodeProps(nodeId)
    val siteProps = SiteProps(siteId)
    val configProps =
      ConfigProps(Some(policyid),None, None, Some(parkingGroupId), Some("14986863999977"), Some("14986863999987"), None, None, None, None, Some(policyRequestFromUser), None, None, None)
    val requestBody = RequestBody(instanceId,
      requestId,
      timestamp,
      reqtype,
      model,
      action,
      Some("abc"),
      orgProps,
      siteProps,
      configProps,AppUserDataProps(None,"","",None))
    AppRequest(messageId, responseTopic, requestBody)
  }

  def generateAppRequestWithHeadersForActivePolicyFailure(responseTopic: String,
                                   reqtype: String,
                                   policyid: String,
                                   policyRequestFromUser: PolicyRequest): AppRequest = {
    val orgProps = OrgProps(orgId)
    val nodeProps = NodeProps(nodeId)
    val siteProps = SiteProps(siteId)
    val parkingGroupIdForFail = UUID.randomUUID().toString
    val configProps =
      ConfigProps(None, None, Some(10), Some(parkingGroupIdForFail), None, None, None, None, None, None, Some(policyRequestFromUser), None, None, None)
    val requestBody = RequestBody(instanceId,
      requestId,
      timestamp,
      reqtype,
      model,
      action,
      Some("abc"),
      orgProps,
      siteProps,
      configProps,AppUserDataProps(None,"","",None))
    AppRequest(messageId, responseTopic, requestBody)
  }


  def generateAppRequestWithHeadersObject(responseTopic: String,
                                    reqtype: String,
                                    policyid: String,
                                    policyRequestFromUser: PolicyRequest): AppRequest = {
    val orgProps = OrgProps(orgId)
    val nodeProps = NodeProps(nodeId)
    val siteProps = SiteProps(siteId)
    val tagsPolicyLinkRequest=TagsPolicyLinkRequest(Set(policytagId))
    val configProps =
      ConfigProps(Some(policyid), None, Some(10), None, None, None, None, None, None,Some(tagsPolicyLinkRequest), Some(policyRequestFromUser), None, Some(searchPolicyByNameOrTagid), None)
    val requestBody = RequestBody(instanceId,
      requestId,
      timestamp,
      reqtype,
      model,
      action,
      Some("abc"),
      orgProps,
      siteProps,
      configProps,AppUserDataProps(None,"","",None))
    AppRequest(messageId, responseTopic, requestBody)
  }

  def searchPolicyByNameOrTagid =  SearchPolicyByNameOrTagid(Some("new policy"), None)


  def generateAppRequestWithHeader(responseTopic: String,
                                    reqtype: String,
                                    policyid: String,
                                    policyRequestFromUser: PolicyRequest): AppRequest = {
    val orgProps = OrgProps(orgId)
    val nodeProps = NodeProps(nodeId)
    val siteProps = SiteProps(siteId)
    val configProps =
      ConfigProps(None, None, Some(10), Some(parkingGroupId), None, None, None, None, None, None, Some(policyRequestFromUser), None, None, None)
    val requestBody = RequestBody(instanceId,
      requestId,
      timestamp,
      reqtype,
      model,
      action,
      Some("abc"),
      orgProps,
      siteProps,
      configProps,AppUserDataProps(None,"","",None))
    AppRequest(messageId, responseTopic, requestBody)
  }


  def generateAppResponse[T](result: T) = {
    val responseBody = ResponseBody(requestId, timestamp, true, result)
    AppSuccessResponse(messageId, responseBody)
  }

  def tag =
    Tag(policytagId, "Tag", " Tag desc", orgId, siteId, timeInMillis, timeInMillis, false)

  def updatedTag =
    Tag(policytagId, "TagUpdated", "Tag desc updated", orgId, siteId, timeInMillis, timeInMillis, false)

  def tagTwo =
    Tag(policytagIdTwo, "Tag", " Tag desc", orgId, siteId, timeInMillis, timeInMillis, false)

  def tagFromUser =
    TagRequest(Some(policytagId), "name", Some("desc"))

  def updatedTagFromUser =
    TagRequest(Some(policytagId), "TagUpdated", Some("Tag desc updated"))

  val violation: ViolationFine       = ViolationFine("max-time-exceeded", 100.0, Some("daily"))
  val parkingPenalty: ParkingPenalty = ParkingPenalty(Some("Penalty description"), Set(violation))
  val chargeDuration: ChargeDuration = ChargeDuration(Some("Name"),60.0, 5.0, "hours", 10.0)
  val maxDuration: MaxDuration       = MaxDuration(24.0, "hours")
  val parkingCharge: ParkingCharge =
    ParkingCharge(Some("Charge Description"), Some("Charge Description"), None, Some(maxDuration), 60.4)
  val valperiod: Period      = Period("2017-06-28;19:31:03", "2018-07-28;19:31:03")
  val timeRange: TimeRange   = TimeRange("06:00:00", "11:00:00")
  val datePeriod: DatePeriod = DatePeriod(valperiod, "yearly")
  val parkingSchedule: ParkingSchedule =
    ParkingSchedule(Some("Schedule"), None, None, Some(datePeriod), Some(Set(timeRange)))

  val parkingRule: ParkingRule = ParkingRule(Some("Rule"), true, Some(parkingCharge), Some(parkingPenalty))
  val policyRule: PolicyRule =
    PolicyRule(Some("id"),Some("Policy rule"), Some("Policy rule Description"), 12, parkingSchedule, parkingRule)
  val policyLevelViolation = PolicyLevelViolation("id","name",None,"max-time-exceeded",20.56,Some("daily"))

  def parkingGroupPolicyLink = ParkingGroupPolicyLink(policyGroupUUID,parkingGroupId, policyId, 10, 1509135843820L, -1L)


  def policy =
    Policy(
      policyId,
      "newupdateon 13 july",
      siteId,
      orgId,
      Set("Sunday policy"),
      Set(policyLevelViolation),
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
  def policyResponse(policy: Policy)=    PolicyResponse(
    policyId = policy.uid,
    description = policy.description,
    userId = policy.policyAuthorizerid,
    siteId = policy.siteid,
    orgId = policy.orgid,
    tags = policy.tags,
    policyLevelViolations = policy.policyLevelViolations,
    isLibraryPolicy = policy.isLibraryPolicy,
    hashValue = policy.hashValue,
    createdOn = convertEpochToDate(policy.createdOn),
    lastUpdated = convertEpochToDate(policy.lastUpdated),
    version = policy.version,
    name = policy.name,
    timeZone = policy.timeZone,
    state = policy.state,
    policyRule = policy.policyRule
  )


  def convertEpochToDate(epoch: Long): String =
    if (epoch == 0) "" else Instant.ofEpochMilli(epoch).toString

  def activePolicy =
    Policy(
      policyId,
      "newupdateon 13 july",
      siteId,
      orgId,
      Set("Sunday policy"),
      Set(policyLevelViolation),
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

  def policyOne =
    Policy(
      policyIdOne,
      "newupdateon 29 july",
      siteId,
      orgId,
      Set("Sunday policy"),
      Set(policyLevelViolation),
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

  def updatedPolicy =
    Policy(
      policyId,
      "newupdateon 13 july",
      "siteid",
      "orgid",
      Set("Sunday policy"),
      Set(policyLevelViolation),
      false,
      "7653612563",
      14986863999977L,
      14986963999977L,
      false,
      10,
      "new policy",
      Some("Policy description"),
      "Africa/Costarica",
      Some(""),
      Set(policyRule)
    )

  def policyTwo =
    Tag(policyIdTwo, "Tag", " Tag desc", orgId, siteId, timeInMillis, timeInMillis, false)

  def policyFromUser =
    PolicyRequest(
      Some(policyId),
      Some("d8547b37-95ba-410d-9382-0b190d951332"),
      Some(Set("new", "Cate2")),
      Some(Set(policyLevelViolation)),
      "ters policy",
      Some("Africa/Costarica"),
      Some("Policy description"),
      Some(Set(policyRule))
    )

  def updatedPolicyFromUser =
    PolicyRequest(
      Some(policyId),
      Some("d8547b37-95ba-410d-9382-0b190d951332"),
      Some(Set("Update categoty", "Update Cate2")),
      Some(Set(policyLevelViolation)),
      "Updated policy",
      Some("Africa/Costarica"),
      Some("Policy description"),
      Some(Set(policyRule))
    )

}
