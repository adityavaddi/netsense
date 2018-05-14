package com.vz.nsp.parking.userdataservice.constants

import java.util.{Calendar, UUID}

import com.vz.nsp.parking.model.{AppUserDataProps, _}


object TestDataFeed {

  val messageId = UUID.randomUUID().toString
  val instanceId = UUID.randomUUID().toString
  val requestId = UUID.randomUUID().toString
  val nodeId = UUID.randomUUID().toString
  val siteId = UUID.randomUUID().toString
  val orgId = UUID.randomUUID().toString
  val appId = UUID.randomUUID().toString
  val userId = UUID.randomUUID().toString
  val userDataId = UUID.randomUUID().toString
  val userDataIdOne = UUID.randomUUID().toString
  val timestamp = Calendar.getInstance().toInstant.toString
  val timeInMilliSecondsLong = System.currentTimeMillis()
  val `type` = "getTag"
  val model = "NodeModel"
  val action = "CAN_READ"
  val timeInMillis = Calendar.getInstance().getTimeInMillis

  def generateAppRequestWithHeaders(responseTopic: String): AppRequest = {
    val orgProps = OrgProps(orgId)
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
        None,
        None,
        None,
        None,
        None,
        None),
      AppUserDataProps(None,"","",None)
    )
    AppRequest(messageId, messageId, requestBody)
  }

  def generateAppRequestWithHeaders(responseTopic: String,
                                    reqtype: String,
                                    tagid: String,
                                    tagFromUser: TagRequest): AppRequest = {
    val orgProps = OrgProps(orgId)
    val siteProps = SiteProps(siteId)
    val userData = AppUserDataProps(None,"","",None)
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
def generateAppRequestWithHeader(responseTopic: String,
                                  reqtype: String,
                                 userDataId: String,
                                 userDataRequest: UserDataRequest): AppRequest = {
  val orgProps = OrgProps(orgId)
  val siteProps = SiteProps(siteId)
  val appUserDataProps = AppUserDataProps(Some(userDataId),userDataRequest.userid,userDataRequest.appid,Some(userDataRequest.datavalue))
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
    appUserDataProps)
  AppRequest(messageId, responseTopic,
    requestBody)
}

  def generateAppRequestWithHeadersForCreatePolicyFailure(responseTopic: String,
                                   reqtype: String,
                                   userDataId: String): AppRequest = {
    val orgProps = OrgProps(orgId)
    val nodeProps = NodeProps(nodeId)
    val siteProps = SiteProps(siteId)
    val appUserDataProps = AppUserDataProps(Some(userDataId),null,userDataRequest.appid,Some(userDataRequest.datavalue))
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
      appUserDataProps)
    AppRequest(messageId, responseTopic,
      requestBody)
  }


  def generateAppRequestWithHeadersForGetAllUserDataFailure(responseTopic: String,
                                                          reqtype: String,
                                                          userDataId: String,
                                                            appid: String,
                                                            userid: String): AppRequest = {
    val orgProps = OrgProps(orgId)
    val nodeProps = NodeProps(nodeId)
    val siteProps = SiteProps(siteId)
    val appUserDataProps = AppUserDataProps(Some(userDataId),userid,userDataRequest.appid,Some(userDataRequest.datavalue))
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
      appUserDataProps)
    AppRequest(messageId, responseTopic,
      requestBody)
  }

  def generateAppResponse[T](result: T) = {
    val responseBody = ResponseBody(requestId, timestamp, true, result)
    AppSuccessResponse(messageId, responseBody)
  }

  def userData = UserData("appid", "userid",userDataId,"DataValue",timeInMillis, timeInMillis,false)

  def userDataone = UserData("appid", "userid",userDataIdOne,"DataValue",timeInMillis, timeInMillis,false)

  def updatedAppUserData = UserData("appidone", "useridone",userDataIdOne,"UpdatedDataValue",timeInMillis, timeInMillis,false)

  def userDataRequest = UserDataRequest("appid", "userid", "datavalue")
  def updatedDataRequest = UserDataRequest("appid", "userid", "datavalue")




}
