package com.vz.nsp.parking.model

import com.fasterxml.jackson.annotation.JsonProperty

case class NodeProps(nodeid: String)

case class SiteProps(siteid: String)

case class OrgProps(orgid: String)

case class ConfigProps(policyid: Option[String],
                       tagid: Option[String],
                       version: Option[Int],
                       parkinggroupid: Option[String],
                       fromtime: Option[String],
                       totime: Option[String],
                       parkingspotid: Option[String],
                       validationError: Option[String],
                       @JsonProperty tag: Option[TagRequest],
                       @JsonProperty tagspolicylink: Option[TagsPolicyLinkRequest],
                       @JsonProperty policy: Option[PolicyRequest],
                       @JsonProperty ParkingGroupPolicyLink: Option[ParkingGroupPolicyLinkRequest],
                       @JsonProperty searchPayload: Option[SearchPolicyByNameOrTagid],
                       @JsonProperty parkingSpot: Option[ParkingSpaceRequest])

case class AppUserDataProps(userdataid: Option[String],
                            userid: String,
                            appid: String,
                            datavalue: Option[String]
                            )

case class AppRequest(messageid: String, responsetopic: String, @JsonProperty request: RequestBody)

case class RequestBody(instanceid: String,
                       requestid: String,
                       timestamp: String,
                       `type`: String,
                       model: String,
                       action: String,
                       user: Option[String],
                       @JsonProperty orgprops: OrgProps,
                       @JsonProperty siteprops: SiteProps,
                       @JsonProperty configprops: ConfigProps,
                       @JsonProperty appuserdataprops: AppUserDataProps,
                       @JsonProperty spaceattributes: Option[List[ParkingSpaceRequest]]=None,
                       @JsonProperty parkingspaceids: Option[Set[String]]=None
                      )

sealed trait AppResponse

case class AppSuccessResponse[T](messageid: String, @JsonProperty response: ResponseBody[T]) extends AppResponse

case class ResponseBody[T](requestid: String, timestamp: String, success: Boolean, @JsonProperty result: T)

case class FailureResponseBody(requestid: String, timestamp: String, success: Boolean, error: String, status: Int)

case class AppFailureResponse(messageid: String, response: FailureResponseBody) extends AppResponse

case class MessageIdAndResponseTopic(messageid: String, responsetopic: String)
