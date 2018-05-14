package com.verizon.netsense.entity

import com.fasterxml.jackson.annotation.JsonProperty

case class IsRequestQueryEnvelope(messageid: String,
                               @JsonProperty request: IsQueryEnvelope) extends Entity

case class IsRequestQueryConfigEnvelope(messageid: String,
                                  @JsonProperty request: IsQueryConfigEnvelope) extends Entity


case class IsQueryEnvelope(requestid: String,
                           `type`: String,
                           model: String,
                           action: String,
                           instanceid: String,
                           responsetopic: String,
                           timestamp: String,
                           user: String,
                           @JsonProperty nodeprops: NodeProps,
                           @JsonProperty siteprops: SiteProps,
                           @JsonProperty orgprops: OrgProps
                           ) extends Entity

case class IsQueryConfigEnvelope(requestid: String,
                           `type`: String,
                           model: String,
                           action: String,
                           instanceid: String,
                           responsetopic: String,
                           timestamp: String,
                           user: String,
                           @JsonProperty nodeprops: NodeProps,
                           @JsonProperty siteprops: SiteProps,
                           @JsonProperty orgprops: OrgProps,
                           @JsonProperty configprops: ConfigProps
                          ) extends Entity

