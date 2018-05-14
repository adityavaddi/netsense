package com.verizon.netsense.entity

import com.fasterxml.jackson.annotation.JsonProperty


case class ISMessageConfig(messageid: String, @JsonProperty response: ISResponseConfig) extends ISResponseMessage
case class ISMessageAllConfig(messageid: String, @JsonProperty response: ISResponseAllConfig) extends ISResponseMessage
case class ISMessageToken(messageid: String, @JsonProperty response: ISResponseToken) extends ISResponseMessage


case class ISResponseConfig(requestid: String,
                                   success: Boolean,
                                   timestamp: String,
                                   @JsonProperty config: Map[String, Any],
                                   @JsonProperty nodes: List[Map[String, String]]) extends ISResponsePayload


case class ISResponseAllConfig(requestid: String,
                                   success: Boolean,
                                   timestamp: String,
                                   @JsonProperty items: List[Map[String, Any]]) extends ISResponsePayload


case class ISResponseToken( requestid: String,
                            success: Boolean,
                            timestamp: String,
                            message: String,
                            token: String) extends ISResponsePayload


