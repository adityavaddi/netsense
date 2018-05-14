package com.verizon.netsense.services.eventsimulator.model

import java.util.{Calendar, UUID}

import com.fasterxml.jackson.annotation.{JsonIgnoreProperties, JsonProperty}
import com.verizon.netsense.entity.Entity

/**
 * Created by vermri5 on 7/12/17.
 */
case class ApAlarm(nodeid: String, alarmType: String, severity: String, msg: String, category: String, created: String)
    extends Entity

/*
 * a : Solicitation type
 * f : Sensor type
 * p : path by which event is generated
 * sid : node id
 * d : RFC3339 format
 */
@JsonIgnoreProperties(ignoreUnknown = true)
case class AlarmEvent(uuid: String,
                      a: String,
                      f: String,
                      p: String,
                      @JsonProperty l: DeviceAlarmPayload,
                      sid: String,
                      d: String)
    extends Entity

case class CoreAlarmEvent(name: String, nodeid: String, alarmType: String, alarmSeverity: String, msg: String)
    extends Entity

/*
 * a : alarm type
 * c : category
 * s : alarm severity
 * m : message
 */
case class DeviceAlarmPayload(a: String, s: Int, m: String, c: Int)



