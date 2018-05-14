package com.verizon.netsense.services.alert.model

import java.util.{Calendar, UUID}

import com.fasterxml.jackson.annotation.{JsonIgnoreProperties, JsonProperty}
import com.verizon.netsense.entity.Entity
import com.verizon.netsense.services.alert.customexceptions.AlarmNotCreatedException

/**
 * Created by vermri5 on 7/12/17.
 */
case class ApAlarm(nodeId: String, alarmType: String, severity: String, msg: String, category: String, created: String)
    extends Entity

object ApAlarm {
  def apply(alarm: AlarmEvent): ApAlarm = {
    val alarmType = if (alarm.l.a.equalsIgnoreCase("lwt")) "Disconnect" else alarm.l.a
    ApAlarm(alarm.sid,
            alarmType,
            AlarmSeverity(alarm.l.s).toString,
            alarm.l.m,
            AlarmCategory(alarm.l.c).toString,
            alarm.d)
  }
}

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

object AlarmEvent {
  def apply(alarm: CoreAlarmEvent): AlarmEvent =
    AlarmEvent(
      UUID.randomUUID().toString,
      "UNSOL",
      "Core Node Alarm",
      "Core Node Path",
      DeviceAlarmPayload(alarm.alarmType,
                         AlarmSeverity(alarm.alarmSeverity).getOrElse(0),
                         alarm.msg,
                         AlarmCategory(getCategory(alarm.alarmType)).getOrElse(7)),
      alarm.nodeid,
      Calendar.getInstance().toInstant.toString
    )

  lazy val getCategory: Map[String, String] =
    Map(
      "Disconnect"             -> "Network",
      "CommFail"               -> "Hardware",
      "SimFail"                -> "Hardware",
      "NotTested"              -> "Software",
      "DownrevSoftware"        -> "Software",
      "BadSensorData"          -> "InternalSensor",
      "ConfigFail"             -> "Software",
      "DegradedNetwork"        -> "Network",
      "SoftwareUpdateFail"     -> "Software",
      "ScheduleFail"           -> "Software",
      "PreRuninFail"           -> "Software",
      "PostRuninFail"          -> "Software",
      "USPFail"                -> "Hardware",
      "PMACFail"               -> "Hardware",
      "DriverFail"             -> "Hardware",
      "FarmUSPFail"            -> "Hardware",
      "SensorFail"             -> "InternalSensor",
      "StrangeReboot"          -> "Software",
      "Assert"                 -> "Software",
      "X509ClientFail"         -> "Software",
      "X509ServerFail"         -> "Software",
      "UnderPower"             -> "Power",
      "OverPower"              -> "Power",
      "HardFault"              -> "Hardware",
      "HWFail_generic"         -> "Hardware",
      "HWFail_HIH6131"         -> "InternalSensor",
      "HWFail_ISL29023"        -> "InternalSensor",
      "HWFail_SE95"            -> "InternalSensor",
      "HWFail_ZMotion"         -> "InternalSensor",
      "HWFail_MMA8451"         -> "InternalSensor",
      "HWFail_TSC3414"         -> "InternalSensor",
      "HWFail_UrbanUSP"        -> "InternalSensor",
      "HWFail_RTC"             -> "InternalSensor",
      "HWFail_EEPROM"          -> "InternalSensor",
      "HWFail_NIGHTHAWK"       -> "InternalSensor",
      "SWUpdateFail_SENSORPOD" -> "InternalSensor",
      "HWFail_STUCK_RELAY"     -> "InternalSensor",
      "HWFail_PCT2075"         -> "InternalSensor",
      "HWFAIL_SIHAWK"          -> "InternalSensor",
      "HWFAIL_GPS"             -> "InternalSensor",
      "HWFail_PodBus"          -> "InternalSensor",
      "Epic_Fail"              -> "Software"
    ).withDefaultValue("Lighting")

}

case class CoreAlarmEvent(name: String, nodeid: String, alarmType: String, alarmSeverity: String, msg: String)
    extends Entity

/*
 * a : alarm type
 * c : category
 * s : alarm severity
 * m : message
 */
case class DeviceAlarmPayload(a: String, s: Int, m: String, c: Int)

object AlarmSeverity extends Enumeration {
  type AlarmSeverity = Value
  val Clear    = Value(0)
  val Warning  = Value(1)
  val Minor    = Value(2)
  val Major    = Value(3)
  val Critical = Value(4)

  def apply(s: String): Option[Int] = {

    val list = List(Clear, Warning, Minor, Major, Critical)

    try {
      val o = AlarmSeverity.withName(s)
      if (list.exists(o.equals(_))) {
        Some(o.id)
      } else {
        None
      }
    } catch {
      case _: Exception =>
        throw new AlarmNotCreatedException("Invalid Alarm Severity received in the Alarm payload: " + s)
    }
  }

}

object AlarmCategory extends Enumeration {
  type AlarmCategory = Value
  val Network        = Value(1)
  val Hardware       = Value(2)
  val Software       = Value(3)
  val Power          = Value(4)
  val InternalSensor = Value(5)
  val ExternalSensor = Value(6)
  val Lighting       = Value(7)

  def apply(s: String): Option[Int] = {

    val list = List(Network, Hardware, Software, Power, InternalSensor, ExternalSensor, Lighting)

    try {
      val o = AlarmCategory.withName(s)
      if (list.exists(o.equals(_))) {
        Some(o.id)
      } else {
        None
      }
    } catch {
      case _: Exception => throw new RuntimeException("Invalid Alarm Category received in the alarm payload")
    }
  }
}

case class UfAlarm(@JsonProperty("mappingid") mappingId: String,
                   @JsonProperty("alarmtype") alarmType: String,
                   @JsonProperty("nodemodels") nodeModels: Set[String] = Set.empty[String],
                   @JsonProperty("ufname") ufName: Option[String] = None,
                   description: Option[String] = None,
                   @JsonProperty("displaytocustomer") displayToCustomer: Boolean = false,
                   @JsonProperty("displaytopartner") displayToPartner: Boolean = false,
                   created: Option[Long] = None,
                   updated: Option[Long] = None)
    extends Entity

case class NotificationType(@JsonProperty("alarmtype") alarmType: String,
                            @JsonProperty("nodemodels") nodeModels: Set[String] = Set.empty[String],
                            @JsonProperty("ufname") ufName: Option[String] = None)
    extends Entity

object NotificationType {
  def apply(ufAlarm: UfAlarm): NotificationType =
    NotificationType(ufAlarm.alarmType, ufAlarm.nodeModels, ufAlarm.ufName)
}
