package com.vz.nsp.parking.tagservice.constants

import java.time.Instant
import java.util.{Calendar, UUID}

import com.vz.nsp.parking.model._

import scala.collection.immutable.Nil
import scala.concurrent.Future

/**
  * Created by donthgo on 7/12/17.
  */
object TestDataFeed {

  val name = "name"
  val messageId: String = UUID.randomUUID().toString
  val instanceId: String = UUID.randomUUID().toString
  val requestId: String = UUID.randomUUID().toString
  val nodeId: String = UUID.randomUUID().toString
  val siteId: String = UUID.randomUUID().toString
  val orgId: String = UUID.randomUUID().toString
  val userId: String = UUID.randomUUID().toString
  val tagId: String = UUID.randomUUID().toString
  val tagId2: String = UUID.randomUUID().toString
  val policyId: String = UUID.randomUUID().toString
  val policyIdTwo: String = UUID.randomUUID().toString
  val policyIdThree: String = UUID.randomUUID().toString
  val policyIdFour: String = UUID.randomUUID().toString
  val policyIdFive: String = UUID.randomUUID().toString
  val policyIdSix: String = UUID.randomUUID().toString
  val policyIdSeven: String = UUID.randomUUID().toString
  val sensorType = "pc"
  val sampleValue = 123123.0
  val callbackaddress = "callbackaddress"
  val callbackuri = "/dummy"
  val timestamp: String = Calendar.getInstance().toInstant.toString
  val timeInMilliSecondsLong: Long = System.currentTimeMillis()
  val limit = 120
  val `type` = "getTag"
  val model = "NodeModel"
  val action = "CAN_READ"
  val user = "d48dcd46-da6e-49b0-b76e-e3cbc79c4565"
  val sensorid = "pc"
  val date1 = "2017-06-03T22:54:46.670Z"
  val date2 = "2099-06-19T22:55:16.737Z"
  val period = "15min"
  val timeInMillis: Long = Calendar.getInstance().getTimeInMillis

  def generateAppRequestWithHeaders(responseTopic: String): AppRequest = {
    val orgProps = OrgProps(orgId)
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
        None,
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
        None),
      AppUserDataProps(None, "", "", None)
    )
    AppRequest(messageId, messageId, requestBody)
  }

  def generateAppRequestWithHeaders(responseTopic: String,
                                    reqtype: String,
                                    tagid: String,
                                    tagFromUser: TagRequest): AppRequest = {
    val orgProps = OrgProps(orgId)
    val nodeProps = NodeProps(nodeId)
    val siteProps = SiteProps(siteId)
    val userData = AppUserDataProps(None, "", "", None)
    val configProps =
      ConfigProps(None, Some(tagid), Some(1), None, None, None, None, None, Some(tagFromUser), None, None, None, None, None)
    val requestBody = RequestBody(instanceId,
      requestId,
      timestamp,
      reqtype,
      model,
      action,
      Some("abc"),
      orgProps,
      siteProps,
      configProps,
      userData)
    AppRequest(messageId, responseTopic,
      requestBody)
  }

  def generateAppRequestWithHeadersGetTagFailure(responseTopic: String,
                                                 reqtype: String,
                                                 tagFromUser: TagRequest): AppRequest = {

    val orgProps = OrgProps(orgId)
    val siteProps = SiteProps(siteId)
    val userData = AppUserDataProps(None, "", "", None)

    val configProps =
      ConfigProps(None, None, Some(1), None, None, None, None, None, None, None, None, None, None, None)
    val requestBody = RequestBody(instanceId,
      requestId,
      timestamp,
      reqtype,
      model,
      action,
      Some("abc"),
      orgProps,
      siteProps,
      configProps,
      userData)
    AppRequest(messageId, responseTopic,
      requestBody)
  }

  def generateAppRequestWithHeadersGetTagFailureWrongOrgIdSiteid(responseTopic: String,
                                                                 reqtype: String, tagid: String, orgid: String, siteid: String): AppRequest = {

    val orgProps = OrgProps(orgid)
    val nodeProps = NodeProps(nodeId)
    val siteProps = SiteProps(siteid)
    val userData = AppUserDataProps(None, "", "", None)

    val configProps =
      ConfigProps(None, Some(tagid), Some(1), None, None, None, None, None, Some(updatedTagFromUser), None, None, None, None, None)
    val requestBody = RequestBody(instanceId,
      requestId,
      timestamp,
      reqtype,
      model,
      action,
      Some("abc"),
      orgProps,
      siteProps,
      configProps,
      userData)
    AppRequest(messageId, responseTopic,
      requestBody)
  }

  def generateAppRequestWithHeadersForDeleteTagFailure(responseTopic: String,
                                                       reqtype: String,
                                                       tagid: String,
                                                       tagFromUser: TagRequest): AppRequest = {
    val orgProps = OrgProps(orgId)
    val siteProps = SiteProps(siteId)
    val userData = AppUserDataProps(None, "", "", None)

    val configProps =
      ConfigProps(None, Some(tagid), Some(1), None, None, None, None, None, Some(tagFromUser), None, None, None, None, None)
    val requestBody = RequestBody(instanceId,
      requestId,
      timestamp,
      reqtype,
      model,
      action,
      Some("abc"),
      orgProps,
      siteProps,
      configProps,
      userData)
    AppRequest(messageId, responseTopic,
      requestBody)
  }

  def generateAppResponse[T](result: T): AppSuccessResponse[T] = {
    val responseBody = ResponseBody(requestId, timestamp, true, result)
    AppSuccessResponse(messageId, responseBody)
  }

  def tag =
    Tag(tagId, "Tag", " Tag desc", orgId, siteId, timeInMillis, timeInMillis, false)

  def tagResponse =
    TagResponse(tagId, "Tag", " Tag desc", orgId, siteId, convertEpochToDate(timeInMillis), convertEpochToDate(timeInMillis))

  def convertEpochToDate(epoch: Long): String =
    if (epoch == 0) "" else Instant.ofEpochMilli(epoch).toString

  def updatedTag =
    Tag(tagId, "TagUpdated", "Tag desc updated", orgId, siteId, timeInMillis, timeInMillis, false)

  def tagTwo =
    Tag(tagId2, "Tag", " Tag desc", orgId, siteId, timeInMillis, timeInMillis, false)

  def tagFromUser =
    TagRequest(Some(tagId), "name", Some("desc"))

  def updatedTagFromUser =
    TagRequest(Some(tagId), "TagUpdated", Some("Tag desc updated"))

  val violation: ViolationFine = ViolationFine("ppv", 100.0, Some("daily"))
  val parkingPenalty: ParkingPenalty = ParkingPenalty(Some("Penalty description"), Set(violation))
  val chargeDuration: ChargeDuration = ChargeDuration(Some("name"), 60.0, 5.0, "hours", 10.0)
  val maxDuration: MaxDuration = MaxDuration(24.0, "hours")
  val parkingCharge: ParkingCharge =
    ParkingCharge(Some("Charge Description"), Some("Charge Description"), Some(List()), Some(maxDuration), 60.4)
  val valperiod: Period = Period("2017-06-28;19:31:03", "2018-07-28;19:31:03")
  val timeRange: TimeRange = TimeRange("06:00:00", "11:00:00")
  val datePeriod: DatePeriod = DatePeriod(valperiod, "yearly")
  val parkingSchedule: ParkingSchedule =
    ParkingSchedule(Some("Schedule"), None, None, Some(datePeriod), Some(Set(timeRange)))

  val parkingRule: ParkingRule = ParkingRule(Some("Rule"), true, Some(parkingCharge), Some(parkingPenalty))
  val policyRule: PolicyRule =
    PolicyRule(Some("id"), Some("Policy rule"), Some("Policy rule Description"), 12, parkingSchedule, parkingRule)
  val policyLevelViolation = PolicyLevelViolation("id", "name", None, "ppv", 20.56, Some("daily"))

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
      Some(""),
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


  /**
    * ReqResGeneratorSpec Feed
    */
  val parkingGroupId: String = UUID.randomUUID().toString
  val resTopic: String = "responseTopic"
  val resultBody: Int = 1

  val appRequest = AppRequest(messageId, resTopic,
     RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString, "associatedParkingGroups",
      "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(Some(policyId), None, None, Some(parkingGroupId), None, None, None, None, None, None,
        None, None, None, None), AppUserDataProps(None, "", "", None)))


  /**
    * StreamHelperSpec Feed
    */

  val uid: String = UUID.randomUUID().toString
  val messageId1: String = UUID.randomUUID().toString
  val messageId2: String = UUID.randomUUID().toString
  val policyId1: String = UUID.randomUUID().toString
  val policyId2: String = UUID.randomUUID().toString

  val appRequestStreamHelper: AppRequest = AppRequest(messageId, resTopic,
     RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "associatedParkingGroups", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(Some(policyId), None, None, None, None, None, None, None, None, None, None, None, None, None), AppUserDataProps(None, "", "", None)))

  val appRequestInvalid = AppRequest(messageId, resTopic,
     RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString, "Irrelevant",
      "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(Some(policyId), None, None, None, None, None, None, None, None, None, None, None, None, None), AppUserDataProps(None, "", "", None)))

  val appRequestMsgNull = AppRequest(null, resTopic,
     RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString, "associatedParkingGroups",
      "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(Some(policyId), None, None, None, None, None, None, None, None, None, None, None, None, None), AppUserDataProps(None, "", "", None)))

  val appRequestRspTopicNull = AppRequest(messageId1, null,
     RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString, "associatedParkingGroups",
      "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(Some(policyId), None, None, None, None, None, None, None, None, None, None, None, None, None), AppUserDataProps(None, "", "", None)))

  val appRequestReqObjNull = AppRequest(messageId1, resTopic, null)

  val failRespBody = FailureResponseBody(requestId, timestamp, true, "Has it Failed!!!", 400)
  val appFailResp = AppFailureResponse(messageId, failRespBody)


  val policyLvlViolation = PolicyLevelViolation("1234", "PolicyViolation-1", Some("Polcy Violation Description"),
    "ppv", 222.32, Some("once"))


  val tagid = "tagid"

  val appRequestGetPolicy = AppRequest(messageId1, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
    "getPolicyCategory", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
    ConfigProps(Some(policyId1), Some(tagid), None, None, None, None, None, None, None,
      None, None, None, None, None), AppUserDataProps(None, "", "", None)))


  /**
    * TagHelperSpec Feed
    */

  //  val messageId           = UUID.randomUUID().toString
  //  val orgId               = UUID.randomUUID().toString
  //  val siteId              = UUID.randomUUID().toString
  //  val instanceId          = "1"
  //  val resTopic            = "responseTopic"
  //  val requestId           = UUID.randomUUID().toString
  //  val tagId               = UUID.randomUUID().toString
  //  val policyLevelViolation = PolicyLevelViolation("id","name",None,"ppv",20.56,Some("once"))

  val appRequestGetTag = AppRequest(
    messageId,
    resTopic,
     RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "getTag",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(None, Some(tagId), None, None, None, None, None, None, None, None, None, None, None, None),
      AppUserDataProps(None, "", "", None)
    )
  )

  val getAllTagAppRequest = AppRequest(
    messageId,
    resTopic,
     RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "getAllTags",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(None, Some(tagId), None, None, None, None, None, None, None, None, None, None, None, None),
      AppUserDataProps(None, "", "", None)
    )
  )

  val deleteTagAppRequest = AppRequest(
    messageId,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "getAllTags",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(None, Some(tagId), None, None, None, None, None, None, None, None, None, None, None, None),
      AppUserDataProps(None, "", "", None)
    )
  )
  val listOfTags: Future[Some[Tag]] = Future.successful(
    Some(tag))

  val listOfPolicies: Future[List[Policy]] = Future.successful(List(Policy(
    UUID.randomUUID().toString, "newupdateon 13 july", siteId, orgId, Set("Sunday policy"), Set(policyLevelViolation), false, "7653612563", 14986863999977L, 14986963999977L,
    false, 10, "new policy", Some("Policy description"), "Africa/Costarica", Some(""), Set())
  ))

  val emptyPoliciesList: Future[Nil.type] = Future.successful(Nil)


  val tagPostRequest = TagRequest(Some(tagId), "NewTag", Some("New Tag creation test"))
  val updateTagRequest = TagRequest(Some(tagId), "Update Tag", Some("Updating a existing Tag"))

  val posTag = Tag(tagId,
    "NewTag",
    "New Tag creation test",
    orgId,
    siteId,
    Calendar.getInstance().getTimeInMillis,
    Calendar.getInstance().getTimeInMillis,
    false)


  val appRequestCreateTag = AppRequest(
    messageId,
    resTopic,
    new RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "createPolicyCategory",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, Some(tagPostRequest), None, None, None, None, None),
      AppUserDataProps(None, "", "", None)
    )
  )

  val appRequestCreateTagWithoutTagRequest = AppRequest(
    messageId,
    resTopic,
     RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "createPolicyCategory",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, None, None, None, None, None, None),
      AppUserDataProps(None, "", "", None)
    )
  )

  val appRequestUpdateTagTagRequest = AppRequest(
    messageId,
    resTopic,
     RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "updatePolicyCategory",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(None, Some(tagId), None, None, None, None, None, None, Some(updateTagRequest), None, None, None, None, None),
      AppUserDataProps(None, "", "", None)
    )
  )

  val appRequestUpdateTagRequestWithoutTagId = AppRequest(
    messageId,
    resTopic,
     RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "updatePolicyCategory",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, Some(updateTagRequest), None, None, None, None, None),
      AppUserDataProps(None, "", "", None)
    )
  )

  val appRequestUpdateTagRequestWithoutTagBody = AppRequest(
    messageId,
    resTopic,
     RequestBody(
      instanceId,
      requestId,
      Calendar.getInstance().toInstant.toString,
      "updatePolicyCategory",
      "NoneModel",
      "CAN_READ",
      Some("user"),
      OrgProps(orgId),
      SiteProps(siteId),
      ConfigProps(None, Some(tagId), None, None, None, None, None, None, None, None, None, None, None, None),
      AppUserDataProps(None, "", "", None)
    )
  )

  val successMessage = SuccessMessage(true)
  val failureMessage = SuccessMessage(false)
  val futureSuccessMessage: Future[Boolean] = Future.successful(true)
  val futureFailureMessage: Future[Boolean] = Future.successful(false)

  val tag1: Future[List[Tag]] = Future.successful(
    List(
      Tag(UUID.randomUUID().toString,
        "Tag",
        "Tag Desc",
        orgId,
        siteId,
        Calendar.getInstance().getTimeInMillis,
        Calendar.getInstance().getTimeInMillis,
        false),
      Tag(UUID.randomUUID().toString,
        "Tag",
        "Tag Desc",
        orgId,
        siteId,
        Calendar.getInstance().getTimeInMillis,
        Calendar.getInstance().getTimeInMillis,
        false)
    )
  )

  val getTagResponse: Future[Some[Tag]] = Future.successful(
    Some(
      Tag(UUID.randomUUID().toString,
        "Tag",
        "Tag Desc",
        orgId,
        siteId,
        Calendar.getInstance().getTimeInMillis,
        Calendar.getInstance().getTimeInMillis,
        false)
  )
  )


  val getTagByIdfailure: Future[None.type] = Future.successful(None)

}
