package com.verizon.netsense.entity

import com.fasterxml.jackson.annotation.JsonProperty
import org.json4s.JValue

case class Envelope(messageid: String,
                    responsetopic: String,
                    request: OTAReq) extends Entity

case class RespEnvelope(messageid: String,
                        @JsonProperty response: OTARes) extends Entity

case class OTAReq(`type`: String,
                  model: String,
                  action: String,
                  user: String,
                  @JsonProperty firmwareprops: FirmwareProps,
                  @JsonProperty otaprops: OTAProps,
                  @JsonProperty nodeprops: NodeProps,
                  @JsonProperty groupprops: GroupProps,
                  @JsonProperty orgprops: OrgProps,
                  @JsonProperty siteprops: SiteProps) extends Entity

case class OTARes(success: Boolean,
                  @JsonProperty error: Option[OTAResError] = None,
                  @JsonProperty data: Option[Any] = None) extends Entity

case class NodeProps(nodeid: String)

case class SiteProps(siteid: String)

case class OrgProps(orgid: String)

case class FirmwareProps(firmwareid: String)

case class OTAProps(jobid: String,
                    description: String,
                    action: String)

case class GroupProps(groupid: String)

case class OTAResError(status: Int,
                       message: String)

case class APIDataFlow(id: String,
                       req: OTAReq,
                       envelope: Envelope,
                       res: Option[OTARes] = None,
                       error: Option[OTAResError] = None,
                       nodeids: Option[List[String]] = None)

case class DSResRaw(messageid: String,
                    response: String) extends Entity

case class DSRes(messageid: String,
                 response: JValue) extends Entity

case class Node(nodeid: String,
                model: String)