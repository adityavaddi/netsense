package com.verizon.netsense.entity

import com.fasterxml.jackson.annotation.JsonProperty


trait CaselRequestPayload extends Entity

case class CaselRequestMessage(messageid: String, responsetopic: String, request: CaselRequestPayload) extends Entity

case class NodeProps(nodeid: String,
                     model: String,
                     command: String)

case class SiteProps(siteid: String)

case class OrgProps(orgid: String)

case class ConfigProps(config: Map[String, Any])



case class CaselRequestCompletePayload(requestid: String,
                               instanceid: String,
                               timestamp: String,
                               `type`: String,
                               model: String,
                               action: String,
                               user: Option[String],
                               @JsonProperty orgprops: Option[OrgProps],
                               @JsonProperty siteprops: Option[SiteProps],
                               @JsonProperty nodeprops: Option[Map[String, Any]],
                               @JsonProperty configprops: Option[Map[String, Any]]
                               ) extends CaselRequestPayload


case class CaselRequestNodePropsPayload(requestid: String,
                                       instanceid: String,
                                       timestamp: String,
                                       `type`: String,
                                       model: String,
                                       action: String,
                                       user: String,
                                       @JsonProperty nodeprops: Option[Map[String, Any]]
                                      ) extends CaselRequestPayload
