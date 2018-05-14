package com.verizon.netsense.whatifservice.model.casel

import com.fasterxml.jackson.annotation.JsonProperty
import com.verizon.netsense.whatifservice.model.{WhatIfJobRequest, WhatIfJobResponse}

case class NodeProps(nodeid: String)

case class SiteProps(siteid: String)

case class OrgProps(orgid: String)

case class UserProps(userEmail: Option[String]= None)

case class WhatIfProps(jobid: Option[String], whatIfJobRequest: Option[WhatIfJobRequest], search: Option[String])

case class AppUserDataProps(userdataid: Option[String], userid: String, appid: String, datavalue: Option[String])

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
                       @JsonProperty whatifprops: Option[WhatIfProps] = None,
                       @JsonProperty userprops: UserProps)

sealed trait AppResponse

case class AppSuccessResponse[T](messageid: String, @JsonProperty response: ResponseBody[T]) extends AppResponse

case class ResponseBody[T](requestid: String, timestamp: String, success: Boolean, @JsonProperty result: T)

case class FailureResponseBody(requestid: String, timestamp: String, success: Boolean, error: String, status: Int)

case class AppFailureResponse(messageid: String, response: FailureResponseBody) extends AppResponse

case class MessageIdAndResponseTopic(messageid: String, responsetopic: String)

case class SparkRequest(messageid: String, responsetopic: String, @JsonProperty request: SparkRequestBody)

case class SparkRequestBody(instanceid: String,
                            requestid: String,
                            timestamp: String,
                            `type`: String,
                            model: String,
                            action: String,
                            user: Option[String],
                            @JsonProperty orgprops: OrgProps,
                            @JsonProperty siteprops: SiteProps,
                            @JsonProperty whatifprops: WhatIfJobResponse)
