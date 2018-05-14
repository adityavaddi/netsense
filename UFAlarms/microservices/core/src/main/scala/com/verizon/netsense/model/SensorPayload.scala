package com.verizon.netsense.model

import com.fasterxml.jackson.annotation.{JsonIgnoreProperties, JsonProperty}
import com.verizon.netsense.entity.Entity

@JsonIgnoreProperties(ignoreUnknown = true)
case class SensorSampleEvent(uuid: String,
                             sid: String,
                             a: String,
                             f: String,
                             p: String,
                             @JsonProperty l: SensorPayload) extends Entity

case class SensorPayload(n: String,
                         u: String = "",
                         s: String,
                         v: BigDecimal,
                         t: Long) extends Entity

case class CoreNodeRawSensorSample(name: String,
                                   nodeid: String,
                                   sensor: String,
                                   time: Long,
                                   units: String,
                                   value: BigDecimal) extends Entity
