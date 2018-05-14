package com.verizon.netsense.entity

import com.fasterxml.jackson.annotation.JsonProperty

case class CaselRequestConnectionStatusPayload(requestid: String,
                               instanceid: String,
                               timestamp: String,
                               `type`: String,
                               model: String,
                               action: String,
                               @JsonProperty nodeprops: Option[Map[String, Any]],
                               connection_status: Boolean
                              ) extends CaselRequestPayload

case class CaselRequestConfigPayload(requestid: String,
                                     instanceid: String,
                                     timestamp: String,
                                     `type`: String,
                                     model: String,
                                     action: String,
                                     @JsonProperty nodeprops: Option[Map[String, Any]],
                                     connection_status: Boolean
                                    ) extends CaselRequestPayload


