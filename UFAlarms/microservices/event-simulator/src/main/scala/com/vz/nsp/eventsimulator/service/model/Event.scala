package com.vz.nsp.eventsimulator.service.model

import com.fasterxml.jackson.annotation.JsonProperty

/**
 * Created by jittara on 4/17/17.
 */
package Event {
  sealed trait BaseEvent
  case class ScheduleEvent(@JsonProperty nodeid: Array[String],
                           @JsonProperty level: Long,
                           @JsonProperty pri: Long,
                           @JsonProperty qualifiers: Long,
                           @JsonProperty hr: Long,
                           @JsonProperty min: Long,
                           @JsonProperty sec: Long,
                           @JsonProperty id: Long,
                           @JsonProperty mask: Long)
      extends BaseEvent

  case class LightEvent(@JsonProperty nodeid: Array[String],
                        @JsonProperty name: String,
                        @JsonProperty pri: Long,
                        @JsonProperty mask: Long,
                        @JsonProperty level: Long,
                        @JsonProperty qualifiers: String,
                        @JsonProperty ftype: String)
      extends BaseEvent

  case class DeviceEvent(@JsonProperty nodeid: Array[String],
                         @JsonProperty name: String,
                         @JsonProperty sensor: String,
                         @JsonProperty value: Double,
                         @JsonProperty time: Long)
      extends BaseEvent

  case class ActionEvent(@JsonProperty nodeid: Array[String], @JsonProperty name: String, @JsonProperty action: String)
      extends BaseEvent

  case class SensorEvent(@JsonProperty name: String,
                         @JsonProperty nodeid: String,
                         @JsonProperty time: String,
                         @JsonProperty units: String,
                         @JsonProperty value: Int)
      extends BaseEvent

  case class LoginEvent(@JsonProperty assocChannel: String,
                        @JsonProperty auth: String,
                        @JsonProperty bssid: String,
                        @JsonProperty clientType: String,
                        @JsonProperty configToken: String,
                        @JsonProperty action: String,
                        @JsonProperty localIP: String,
                        @JsonProperty mac: String,
                        @JsonProperty name: String,
                        @JsonProperty netName: String,
                        @JsonProperty nodeid: String,
                        @JsonProperty profileName: String,
                        @JsonProperty protocolVersion: Int,
                        @JsonProperty swVerId: String,
                        @JsonProperty time: String)
      extends BaseEvent

  case class AlarmEvent(@JsonProperty alarmSeverity: String,
                        @JsonProperty alarmType: String,
                        @JsonProperty implmsg: String,
                        @JsonProperty name: String,
                        @JsonProperty nodeid: String)
      extends BaseEvent

}
