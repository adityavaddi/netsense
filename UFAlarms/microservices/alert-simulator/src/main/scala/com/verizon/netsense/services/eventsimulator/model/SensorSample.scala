package com.verizon.netsense.services.eventsimulator.model

import com.fasterxml.jackson.annotation.JsonProperty
import com.verizon.netsense.entity.Entity

case class CoreNodeSensorSample(name: String,
                                nodeid: String,
                                sensor: String,
                                time: Long,
                                units: String,
                                value: BigDecimal)
    extends Entity

case class SensorPayload(n: String, u: String = "", s: String, v: BigDecimal, t: Long)

case class SensorSample(uuid: String, sid: String, a: String, f: String, p: String, @JsonProperty l: SensorPayload)
    extends Entity
