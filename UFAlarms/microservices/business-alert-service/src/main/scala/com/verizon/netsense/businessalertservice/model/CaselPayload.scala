package com.verizon.netsense.businessalertservice.model

import com.fasterxml.jackson.annotation.JsonProperty

case class SiteProps(siteid: String)

case class OrgProps(orgid: String)

case class BusinessAlertProps(businessAlertId: Option[String] = None, search: Option[String] = None)

case class AppRequest(messageid: String, responsetopic: String, @JsonProperty request: RequestBody)

case class RequestBody(instanceid: String,
                       requestid: String,
                       timestamp: String,
                       `type`: String,
                       model: String,
                       action: String,
                       user: Option[String]= None,
                       @JsonProperty orgprops: OrgProps,
                       @JsonProperty siteprops: SiteProps,
                       @JsonProperty businessalertprops: Option[BusinessAlertProps] = None)

sealed trait AppResponse

case class AppSuccessResponse[T](messageid: String, @JsonProperty response: ResponseBody[T]) extends AppResponse

case class ResponseBody[T](requestid: String, timestamp: String, success: Boolean, @JsonProperty result: T)

case class FailureResponseBody(requestid: String, timestamp: String, success: Boolean, error: String, status: Int)

case class AppFailureResponse(messageid: String, response: FailureResponseBody) extends AppResponse

case class MessageIdAndResponseTopic(messageid: String, responsetopic: String)
