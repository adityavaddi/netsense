package com.vz.ns.ts.service.model

import com.fasterxml.jackson.annotation.JsonProperty

case class DeviceEvent(@JsonProperty nodeid: String,
                       @JsonProperty sensor: String,
                       @JsonProperty value: Double,
                       @JsonProperty time: Long)
