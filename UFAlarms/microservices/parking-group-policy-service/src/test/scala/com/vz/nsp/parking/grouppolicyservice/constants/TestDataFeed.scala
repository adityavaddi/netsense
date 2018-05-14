package com.vz.nsp.parking.grouppolicyservice.constants

import java.util.{Calendar, UUID}

import com.vz.nsp.parking.model._
import com.vz.nsp.parking.model.ParkingSpaceRequest

import scala.concurrent.Future
/**
  * Created by dasarst on 8/16/17.
  */
object TestDataFeed {

  val messageId = UUID.randomUUID().toString
  val instanceId = UUID.randomUUID().toString
  val requestId = UUID.randomUUID().toString
  val nodeId = UUID.randomUUID().toString
  val siteid = UUID.randomUUID().toString
  val orgid = UUID.randomUUID().toString
  val policyId = UUID.randomUUID().toString
  val policyIdTwo = UUID.randomUUID().toString
  val policyGroupUUID = UUID.randomUUID().toString
  val parkingSpotId = UUID.randomUUID().toString
  val parkingSpotIdTwo = UUID.randomUUID().toString
  val timestamp = Calendar.getInstance().toInstant.toString
  val model = "NodeModel"
  val action = "CAN_READ"
  val parkinggroupid = UUID.randomUUID().toString
  val responseTopic   = "responseTopic"
  val orgProps = OrgProps(orgid)
  val siteProps = SiteProps(siteid)
  val geoCoordinate = GeoCoordinate(73.89,-87.98)
  val corner   = SpaceGeometry(geoCoordinate, geoCoordinate, geoCoordinate, geoCoordinate)

  val futureSuccessMessage = Future.successful(SuccessMessage(true))
  val futureFailureMessage = Future.successful(SuccessMessage(false))
  val futureSuccessspot= Future.successful(SuccessMessageWithSpaceAttributes(true,Some(parkingSpotId)))
  val parkingSpotIdStore = UUID.randomUUID().toString

  val parkingSpot = ParkingSpace(parkingSpotId, "lowell",List("car"), false, false,"monitorSensor", "businessUse","not-metered", false,false, List("school"),"meterid","paystationid", "5","curb-side", Some(SpaceGeometry(geoCoordinate,geoCoordinate,geoCoordinate,geoCoordinate)),
    1516986568422L, 1516986568422L, false)

  val parkingSpotResponse = ParkingSpaceResponse(parkingSpotId, "lowell",List("car"), false, false,"monitorSensor", "businessUse","not-metered", false,false, List("school"),"meterid","paystationid", "5","curb-side", Some(SpaceGeometry(geoCoordinate,geoCoordinate,geoCoordinate,geoCoordinate)),
    "1969-12-31T19:00:00.050Z", "2018-01-26T12:09:28.0500Z")
  val parkingSpotReqNewId = ParkingSpaceRequest(parkingSpotIdStore, Some("lowell"),Some(List("car")), Some(false), Some(false),Some("monitorSensor"), Some("businessUse"),Some("not-metered"), Some(false),Some(false), Some(List("school")),Some("meterid"),Some("paystationid"), Some("5"),Some("curb-side"), Some(SpaceGeometry(geoCoordinate,geoCoordinate,geoCoordinate,geoCoordinate)))


  val appRequestParkingSpot = AppRequest(messageId,responseTopic,
    new RequestBody(instanceId,requestId,Calendar.getInstance().toInstant.toString, "getMetadataForParkingSpot",
      "NoneModel","CAN_READ",Some("user"), OrgProps(orgid),SiteProps(siteid),
      ConfigProps(Some(policyId),None,None,None,None,None,Some(parkingSpotId),None,None,None,None,None,None,Some(parkingSpotReqNewId)),AppUserDataProps(None,"","",None),Some(List(parkingSpotRequest)),Some(Set(parkingSpotId))))

  val violation: ViolationFine       = ViolationFine("ppv", 100.0, Some("daily"))
  val parkingPenalty: ParkingPenalty = ParkingPenalty(Some("Penalty description"), Set(violation))
  val chargeDuration: ChargeDuration = ChargeDuration(Some("charge duration name"),60.0, 5.0, "hours", 10.0)
  val maxDuration: MaxDuration = MaxDuration(24.0, "hours")
  val parkingCharge: ParkingCharge =
    ParkingCharge(Some("Charge Description"), Some("Charge Description"), Some(List()), Some(maxDuration), 60.4)
  val valperiod: Period = Period("2017-06-28;19:31:03", "2018-06-28;19:31:03")
  val timeRange: TimeRange = TimeRange("06:00:00", "11:00:00")
  val datePeriod: DatePeriod = DatePeriod(valperiod, "yearly")
  val parkingSchedule: ParkingSchedule = ParkingSchedule(Some("Schedule"), None, None, Some(datePeriod), Some(Set(timeRange)))

  val parkingRule: ParkingRule = ParkingRule(Some("Rule"), true, Some(parkingCharge), Some(parkingPenalty))
  val policyRule: PolicyRule = PolicyRule(Some("id"),Some("Policy rule"), Some("Policy rule Description"), 12, parkingSchedule, parkingRule)
  val policyLevelViolation = PolicyLevelViolation("id","name",None,"ppv",20.56,Some("once"))


  def policy =
    Policy(
      policyId,
      "newupdateon 13 july",
      siteid,
      orgid,
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

  def parkingGroupPolicyLink = ParkingGroupPolicyLink(policyGroupUUID,parkinggroupid, policyId, 10, 14986863999977L, -1L)

  def generateAppRequestWithHeaders(responseTopic: String, reqtype: String,
                                    groupPolicyId: String, parkingGroupPolicyLinkRequest: ParkingGroupPolicyLinkRequest): AppRequest = {
    val configProps = ConfigProps(Some(groupPolicyId),None,None,None,None,None,None,None,None,None,None, Some(parkingGroupPolicyLinkRequest),None,None)
    val requestBody = RequestBody(instanceId, requestId, timestamp, reqtype, model, action,
      Some("abc"), orgProps, siteProps, configProps,AppUserDataProps(None,"","",None))
    AppRequest(messageId,responseTopic, requestBody)
  }


  def generateAppRequestWithHeaders(responseTopic: String, reqtype: String,
                                    spotCatId: String, parkingSpotRequest: ParkingSpaceRequest,parkingspotids:Set[String]): AppRequest = {
    val configProps = ConfigProps(None,None,None,None,None,None,Some(spotCatId),None,None, None,None,None,None,Some(parkingSpotRequest))
    val requestBody = RequestBody(instanceId, requestId, timestamp, reqtype, model, action,
      Some("abc"), orgProps, siteProps, configProps,AppUserDataProps(None,"","",None),Some(List(parkingSpotRequest)),Some(parkingspotids))
    AppRequest(messageId,responseTopic, requestBody)
  }

  def generateAppRequestWithHeadersForGetAllParkingSpotFailure(responseTopic: String, reqtype: String,
                                                               spotCatId: String, parkingspotids: Set[String]): AppRequest = {

    val orgId  = UUID.randomUUID().toString
    val siteId = UUID.randomUUID().toString

    val orgProps  = OrgProps(orgId)
    val siteProps = SiteProps(siteId)

    val configProps = ConfigProps(None,None,None,None,None,None,Some(spotCatId),None,None, None,None,None,None,Some(parkingSpotRequest))
    val requestBody = RequestBody(instanceId, requestId, timestamp, reqtype, model, action,
      Some("abc"), orgProps, siteProps, configProps,AppUserDataProps(None,"","",None),None,Some(parkingspotids))
    AppRequest(messageId,responseTopic, requestBody)
  }

  def generateAppRequestWithHeadersNoParkingspotRequest(responseTopic: String, reqtype: String,
                                                        spotCatId: String, parkingSpotRequest: ParkingSpaceRequest): AppRequest = {
    val configProps = ConfigProps(None,None,None,None,None,None,Some(spotCatId),None,None, None,None,None,None,None)
    val requestBody = RequestBody(instanceId, requestId, timestamp, reqtype, model, action,
      Some("abc"), orgProps, siteProps, configProps,AppUserDataProps(None,"","",None))
    AppRequest(messageId,responseTopic, requestBody)
  }

  def generateAppResponse[T](result: T) = {
    val responseBody = ResponseBody(requestId, timestamp, true, result)
    AppSuccessResponse(messageId, responseBody)
  }

  def parkingGroupPolicyLinkRequest = ParkingGroupPolicyLinkRequest(parkinggroupid)

  def parkingspot = ParkingSpace(parkingSpotId, "lowell",List("car"), false, false,"monitorSensor", "businessUse","not-metered", false,false, List("school"),"meterid","paystationid", "5","curb-side", Some(SpaceGeometry(geoCoordinate,geoCoordinate,geoCoordinate,geoCoordinate)),
    1516986568422L, 1516986568422L, false)

  def parkingSpotTwo = ParkingSpace(parkingSpotIdTwo, "lowell",List("car"), false, false,"monitorSensor", "businessUse","not-metered", false,false, List("school"),"meterid","paystationid", "5","curb-side", Some(SpaceGeometry(geoCoordinate,geoCoordinate,geoCoordinate,geoCoordinate)),
    1516986568422L, 1516986568422L, false)
  def parkingSpotRequest = ParkingSpaceRequest(parkingSpotId, Some("lowell"),Some(List("car")), Some(false), Some(false),Some("monitorSensor"), Some("loading"),Some("not-metered"), Some(false),Some(false), Some(List("school")),Some("meterid"),Some("paystationid"), Some("5"),Some("curb-side"), Some(SpaceGeometry(geoCoordinate,geoCoordinate,geoCoordinate,geoCoordinate)))

  def updatedParkingSpot = ParkingSpace(parkingSpotId, "lowell",List("truck"), false, false,"monitorSensor", "businessUse","not-metered", false,false, List("school"),"meterid","paystationid", "5","curb-side", Some(SpaceGeometry(geoCoordinate,geoCoordinate,geoCoordinate,geoCoordinate)),
    1516986568422L, 1516986568422L, false)

}
