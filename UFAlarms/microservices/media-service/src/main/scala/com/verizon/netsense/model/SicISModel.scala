package com.verizon.netsense.model

import com.fasterxml.jackson.annotation.JsonProperty
import org.joda.time.DateTime
import com.verizon.netsense.entity._


case class SicISQueryEnvelope(messageid: String,
                               responsetopic: String,
                               @JsonProperty request: SicISQueryPayload)

case class SicISQueryPayload(requestid: String,
                              `type`: String,
                              model: String,
                              action: String,
                              instanceid: String,
                              timestamp: String,
                              user: String,
                              @JsonProperty nodeprops: NodeProps,
                              @JsonProperty siteprops: SiteProps,
                              @JsonProperty orgprops: OrgProps,
                              @JsonProperty sicProps: ExtProps)


case class SicISResponseEnvelope(messageid: String, @JsonProperty response: SicISResponsePayload) extends  Entity

case class SicISResponsePayload(requestid: String,
                                    success: Boolean = false,
                                    timestamp: String,
                                    error: String,
                                    status: Int = 200,
                                    message: Array[Byte]
                                   ) extends  Entity

case class NodeProps(nodeid: String)

case class SiteProps(siteid: String)

case class OrgProps(orgid: String)

case class ExtProps( channel: String,
                     res: String,
                     imgTS: String,
                     imgFormat: String)

case class SicPayload(time: String, value: Double)

