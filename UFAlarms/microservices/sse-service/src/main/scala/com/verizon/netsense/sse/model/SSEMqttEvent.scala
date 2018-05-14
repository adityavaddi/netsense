package com.verizon.netsense.sse.model

import com.fasterxml.jackson.annotation.JsonProperty
import com.verizon.netsense.sse.config.SSEConfigLoader

sealed trait SSEResult
case class Result(topicName: String, sseEvent: SSEEvent) extends SSEResult
case object NoResult                                     extends SSEResult

case class OrgProps(orgid: String)
case class SiteProps(siteid: String)
case class BusinessAlertProps(application: String, triggerid: String)
case class NodeProps(nodeid: String)
case class ExtProps(eventtype: String)
case class StatusProps(code: String, msg: String)

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

case class SSEResponse(instanceid: String,
                       requestid: String,
                       timestamp: String,
                       `type`: String,
                       model: String,
                       user: String,
                       @JsonProperty orgprops: OrgProps,
                       @JsonProperty siteprops: SiteProps,
                       @JsonProperty nodeprops: NodeProps,
                       @JsonProperty extprops: ExtProps,
                       @JsonProperty status: StatusProps)

///streamv1/orgid/siteid/businessalert/{application}/{triggerid}

case class SSEBusinessRequest(instanceid: String,
                              requestid: String,
                              timestamp: String,
                              `type`: String,
                              model: String,
                              user: String,
                              @JsonProperty orgprops: OrgProps,
                              @JsonProperty siteprops: SiteProps,
                              @JsonProperty businessalertprops: BusinessAlertProps)

case class SSEBusinessResponse(instanceid: String,
                               requestid: String,
                               timestamp: String,
                               `type`: String,
                               model: String,
                               user: String,
                               @JsonProperty orgprops: OrgProps,
                               @JsonProperty siteprops: SiteProps,
                               @JsonProperty businessalertprops: BusinessAlertProps,
                               @JsonProperty status: StatusProps)

case class SSESubscribeRequest(messageid: String, responsetopic: String, @JsonProperty request: SSERequest)
case class SSESubscribeResponse(messageid: String, responsetopic: String, @JsonProperty response: SSEResponse)
case class SSESubscribeBusinessAlertRequest(messageid: String,
                                            responsetopic: String,
                                            @JsonProperty request: SSEBusinessRequest)
case class SSESubscribeBusinessAlertResponse(messageid: String,
                                             responsetopic: String,
                                             @JsonProperty response: SSEBusinessResponse)

case class OrgEvent(orgid: String, siteid: String, nodeid: String)

trait SSEEvent {
  var orgid: String
  var siteid: String
}

trait DeviceEvent extends SSEEvent {
  var nodeid: String
  var name: String
  def topicName: String =
    s"$orgid/$siteid/$nodeid/$name"
}

trait BusinessAlertEvent extends SSEEvent {
  var triggerId: String
  var triggerCategory: String

  def businessAlerttopicName: String = {
    val application = if (triggerCategory == "Business Alert - Parking") "Parking" else triggerCategory
    s"$orgid/$siteid/${SSEConfigLoader.businessAlertTopicSuffix}/$application/$triggerId"
  }
}

object ConnStatus extends Enumeration {
  type ConnStatus = Value
  val `true`  = Value("connected")
  val `false` = Value("disconnected")
}

object RequestType extends Enumeration {
  type RequestType = Value
  val subscribe  = Value("subscribe")
  val heartbeat  = Value("heartbeat")
  val disconnect = Value("disconnect")
}

case class NoEnrich()

case class ConnectionStatus(override var name: String = "ConnectionStatus",
                            override var nodeid: String,
                            override var orgid: String,
                            override var siteid: String,
                            status: String)
    extends SSEEvent
    with DeviceEvent

case class LoginReq(
    assocChannel: Int,
    auth: String,
    bssid: String,
    clientType: String,
    configToken: String,
    localIP: String,
    mac: String,
    override var name: String = "LoginReq",
    netName: String,
    override var nodeid: String,
    override var orgid: String,
    override var siteid: String,
    swVerId: String,
    time: Long,
    profileName: String,
    protocolVersion: Int,
    voltageType: String,
    subType: String,
    modemRevEd: String
) extends SSEEvent
    with DeviceEvent

case class SensorSample(
    override var name: String = "SensorSample",
    override var nodeid: String,
    override var orgid: String,
    sensor: String,
    override var siteid: String,
    time: Long,
    units: String,
    value: BigDecimal
) extends SSEEvent
    with DeviceEvent

case class DeviceAlarm(alertid: String,
                       msg: String,
                       override var name: String = "DeviceAlarm",
                       override var nodeid: String,
                       override var orgid: String,
                       override var siteid: String,
                       severity: String,
                       `type`: String,
                       nodehw: String,
                       ufname: String,
                       description: String,
                       orgname: String,
                       sitename: String,
                       nodename: String,
                       created: String,
                       updated: String,
                       displaytopartner: Boolean,
                       displaytocustomer: Boolean)
    extends SSEEvent
    with DeviceEvent

case class GpsSample(override var nodeid: String,
                     override var name: String = "GpsSample",
                     override var orgid: String,
                     override var siteid: String,
                     latitude: Option[Double],
                     longitude: Option[Double],
                     latUserAdded: Option[Double],
                     lonUserAdded: Option[Double],
                     created: Option[Long],
                     updated: Option[Long])
    extends SSEEvent
    with DeviceEvent

case class BusinessAlert(businessAlertId: String,
                         triggerName: String,
                         override var triggerId: String,
                         override var triggerCategory: String,
                         triggerSubcategory: String,
                         triggerUserId: String,
                         override var siteid: String,
                         override var orgid: String,
                         resourceId: String,
                         resourceName: String,
                         active: Boolean,
                         createdOn: String,
                         lastUpdated: String,
                         lastClearedAt: String,
                         lastClearedBy: String,
                         message: String,
                         severity: String)
    extends SSEEvent
    with BusinessAlertEvent
