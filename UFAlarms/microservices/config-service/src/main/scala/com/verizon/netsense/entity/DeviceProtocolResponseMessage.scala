package com.verizon.netsense.entity

import scala.collection.immutable.HashMap

/**
 * Created by davor on 4/4/2017.
 */
case class DeviceProtocolResponseMessage(routingKey: String, respPayload: RespPayloadMQTT)
case class DeviceProtocolResponseMessageNew(respPayload: HashMap[String, Any])
