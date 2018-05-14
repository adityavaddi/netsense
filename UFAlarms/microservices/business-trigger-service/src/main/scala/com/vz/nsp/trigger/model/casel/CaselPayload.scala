package com.vz.nsp.trigger.model.casel

import com.fasterxml.jackson.annotation.JsonProperty
import com.vz.nsp.trigger.model.app.TriggerRequest

case class SiteProps(siteid: String)

case class OrgProps(orgid: String)

case class TriggerProps(triggerId: Option[String],
                        trigger: Option[TriggerRequest] ,
                        search: Option[String])

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
                       @JsonProperty triggerprops: Option[TriggerProps]
                      )

sealed trait AppResponse

case class AppSuccessResponse[T](messageid: String, @JsonProperty response: ResponseBody[T]) extends AppResponse

case class ResponseBody[T](requestid: String, timestamp: String, success: Boolean, @JsonProperty result: T)

case class FailureResponseBody(requestid: String, timestamp: String, success: Boolean, error: String, status: Int)

case class AppFailureResponse(messageid: String, response: FailureResponseBody) extends AppResponse

case class MessageIdAndResponseTopic(messageid: String, responsetopic: String)
