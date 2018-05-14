package com.verizon.netsense.sse.simulator.model

import com.fasterxml.jackson.annotation.JsonProperty

/**
 * Created by subrsi9 on 11/24/17.
 */
case class OrgProps(orgid: String)
case class SiteProps(siteid: String)
case class NodeProps(nodeid: String)
case class BusinessAlertProps(application: String, triggerid: String)
case class ExtProps(eventtype: String)

case class SSERequest(instanceid: String,
                      requestid: String,
                      timestamp: String,
                      `type`: String,
                      model: String,
                      user: String,
                      @JsonProperty orgprops: OrgProps,
                      @JsonProperty siteprops: SiteProps,
                      @JsonProperty nodeprops: NodeProps,
                      @JsonProperty extprops: ExtProps)

case class SSEBusinessRequest(instanceid: String,
                              requestid: String,
                              timestamp: String,
                              `type`: String,
                              model: String,
                              user: String,
                              @JsonProperty orgprops: OrgProps,
                              @JsonProperty siteprops: SiteProps,
                              @JsonProperty businessalertprops: BusinessAlertProps)

case class SSESubscribeRequest(messageid: String, responsetopic: String, @JsonProperty request: SSERequest)
case class SSESubscribeBusinessAlertRequest(messageid: String,
                                            responsetopic: String,
                                            @JsonProperty request: SSEBusinessRequest)

case class OrgEvent(orgid: String, siteid: String, nodeid: String)
