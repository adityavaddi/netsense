package com.vz.nsp.parking.userdataservice.helper

import java.util.{Calendar, UUID}

import com.vz.nsp.parking.model._
import com.vz.nsp.parking.userdataservice.constants.TestDataFeed.{timeInMillis, userDataId}
import com.vz.nsp.parking.userdataservice.util.BaseSpec


class ReqResGeneratorSpec extends BaseSpec {

  val uid            = UUID.randomUUID().toString
  val messageId      = UUID.randomUUID().toString
  val orgId          = UUID.randomUUID().toString
  val siteId         = UUID.randomUUID().toString
  val requestId      = UUID.randomUUID().toString
  val parkingGroupId = UUID.randomUUID().toString
  val policyId       = UUID.randomUUID().toString
  val appId          = UUID.randomUUID().toString
  val userId         = UUID.randomUUID().toString
  val userDataId     = UUID.randomUUID().toString

  val instanceId: String = "1"
  val resTopic: String   = "responseTopic"
  val resultBody: Int    = 1

  val appRequest = AppRequest(messageId, resTopic,
                              new RequestBody(instanceId,requestId,Calendar.getInstance().toInstant.toString,"associatedParkingGroups",
                                              "NoneModel","CAN_READ",Some("user"),OrgProps(orgId),SiteProps(siteId),
                                              ConfigProps(Some(policyId),None,None,Some(parkingGroupId),None,None,None,None,None,None,
                                              None,None,None,None),appUserDataProps))

  val appUserDataProps = AppUserDataProps(Some(userDataId), userId, appId, Some("datavalue"))

  private implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  val reqResGenerator = new ReqResGenerator()

  "failurecaseResponse" should "Return Success Message" in {

    val output = reqResGenerator.failurecaseResponse(appRequest, 500, "Error: failurecaseResponse!!!")

    assert(output.isInstanceOf[SuccessMessage].!=(true))

  }

  "generateAppFailureResponse" should "Return Failure Response" in {

    val output = reqResGenerator.generateAppFailureResponse(appRequest, 500, "Error: generateAppFailureResponse!!!")

    assert(output.isInstanceOf[AppFailureResponse].!=(false))

  }

  "failurecaseResponseForIrrelevantRequestType" should "Return Success \"AppFailureResponse\"" in {

    val output = reqResGenerator.failurecaseResponseForIrrelevantRequestType(500, "Error: failurecaseResponseForIrrelevantRequestType!!!")

    assert(output.isInstanceOf[SuccessMessage].!=(true))

  }

  "successResponse" should "Return Success \"AppSuccessResponse\"" in {

    val output = reqResGenerator.successResponse(appRequest, resultBody)

    assert(output.isInstanceOf[SuccessMessage].!=(true))

  }

  "failurecaseResponseWithoutFuture" should "Return Success \"AppSuccessResponse\"" in {

    val output = reqResGenerator.failurecaseResponseWithoutFuture(appRequest, 500, "Error: failurecaseResponseWithoutFuture!!!")

    assert(output.isInstanceOf[AppFailureResponse].!=(false))

  }

  "successResponseWithoutFuture" should "Return Success Response" in  {

    val output = reqResGenerator.successResponseWithoutFuture(appRequest, resultBody)

    assert(output.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId))

  }


  "generateAppFailureResponse" should "Return failure response" in {

    val output = reqResGenerator.generateAppFailureResponse( appRequest, 400, "Bad Request")

    assert(output.isInstanceOf[SuccessMessage].!=(true))

  }

  "generateUserDataObject" should "Return UserData Object" in {

    val output = reqResGenerator.generateUserDataObject(appUserDataProps)

    assert(output.datavalue.contains("datavalue"))

  }

  "updateUserDataObject" should "Return userDataobject Object" in {

    val userData = UserData("appId", "userId",userDataId,"DataValue",timeInMillis, timeInMillis,false)

    val userDataRequest = UserDataRequest("appId", "userId","dataValue" )

    val output = reqResGenerator.updateUserDataObject(userData, appUserDataProps)

    assert(output.isdeleted.equals(false))

  }

}
