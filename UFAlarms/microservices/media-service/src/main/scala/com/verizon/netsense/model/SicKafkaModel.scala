package com.verizon.netsense.model

import com.fasterxml.jackson.annotation.{JsonIgnoreProperties, JsonProperty}
import com.verizon.netsense.entity.Entity


// imggrab - Real time image capture

@JsonIgnoreProperties(ignoreUnknown = true)
case class SicResponseEvent(uuid: String,
                        sid: String,
                        a: String,
                        f: String,
                        p: String,
                        @JsonProperty l: SicResponsePayload) extends Entity

@JsonIgnoreProperties(ignoreUnknown = true)
case class SicResponsePayload(n: String,
                              o: Int,  // Offset of current chunk
                              t: Int,  // Total Size
                              ts: String,
                              e: String,  // Optional for rimmget??
                              c: Array[Byte])  // Data for this chunk
                              extends Entity


@JsonIgnoreProperties(ignoreUnknown = true)
case class SicReqEvent(uuid: String,
                       sid: String,
                       a: String,
                       p: String,
                      // @JsonProperty l: SicRequestPayload, (send "l" as Msgpack encoded.
                       l: Array[Byte]) extends Entity

case class SicRequestPayload(f: String, // Image Format
                             c: Short // Size of Data Chunks
                             , a: Array[Byte] = Array.emptyByteArray // optional - JSON encoded bytes of annotate commands
                          ) extends Entity




