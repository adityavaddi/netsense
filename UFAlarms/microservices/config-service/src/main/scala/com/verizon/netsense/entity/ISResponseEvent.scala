package com.verizon.netsense.entity

/**
 * Created by davor on 4/10/2017.
 */
case class ISDeviceConfigResponse(messageId: String, requestId: String, replyTo: String, deviceConfig: DeviceConfig)
    extends ISResponseEvent

case class ISConfigResponse(messageId: String, requestId: String, replyTo: String, model:String, body: Map[String, Any], nodes: List[Map[String, String]])
  extends ISResponseEvent
case class ISGetAllConfigsResponse(messageId: String, requestId: String, replyTo: String, model:String, body: String)
  extends ISResponseEvent
case class ISRespondWithMessage(messageId: String, requestId: String, replyTo: String, status:Int,  message: String)
  extends ISResponseEvent

case class ISApplyConfigToDeviceResponse(messageId: String,
                                         requestId: String,
                                         replyTo: String,
                                         model:String,
                                         status: Int,
                                         token: String) extends ISResponseEvent
case class ISApplySingleParamToDeviceResponse(messageId: String,
                                              requestId: String,
                                              replyTo: String,
                                              model:String, status: Int,
                                              token: String) extends ISResponseEvent

case class VPNOnDemandResponse(messageId: String, requestId: String, replyTo: String, status: Int, body: String)
  extends ISResponseEvent