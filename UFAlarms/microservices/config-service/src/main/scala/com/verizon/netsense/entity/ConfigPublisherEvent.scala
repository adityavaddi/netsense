package com.verizon.netsense.entity

/**
 * Created by davor on 4/10/2017.
 */
trait ConfigPublisherEvent

case class ApplyConfigToNode(nodeId: String, uuid: String, database: String, t: String, config: String)
                                                                                    extends ConfigPublisherEvent
case class GetSingleParam(n: String, p: String, db: String)                         extends ConfigPublisherEvent
case class GetConfigFromNode(nodeid: String, uuid: String, database: String)        extends ConfigPublisherEvent
case class ApplySingleParamToNode(nodeid: String, uuid: String, param: ConfigParam) extends ConfigPublisherEvent
case class ApplyDefaultConfigToNode(nodeid: String, config: String)                 extends ConfigPublisherEvent
case class ResetDevice(nodeid: String, uuid: String)                                extends ConfigPublisherEvent
case class ResetDeviceConfig(nodeid: String, uuid: String, db: String)              extends ConfigPublisherEvent
case class SendConfigLoginReqToLSS(nodeid: String, uuid: String, sid: String, a: String,
                                   f: String, p: String, l: ConfigRequest)          extends ConfigPublisherEvent
case class VpnOnDemand(nodeId: String, requestId: String, command: String)          extends ConfigPublisherEvent