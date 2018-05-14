package com.vz.nsp.datasample.simulator.model

import com.fasterxml.jackson.annotation.JsonProperty

/**
 * Created by maidapr on 5/4/17.
 */
case class SensorPayload(n: String, m: String, s: String, v: Double, t: BigInt)

case class SensorSampleEvent(i: String, a: String, f: String, p: String, @JsonProperty l: SensorPayload)
