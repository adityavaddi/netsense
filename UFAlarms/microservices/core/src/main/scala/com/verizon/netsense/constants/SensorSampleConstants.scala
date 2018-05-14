package com.verizon.netsense.constants

/**
 * Created by maidapr on 6/26/17.
 */
object SensorSampleConstants {

  lazy val bR  = "bR"
  lazy val bRL = "bRL"
  lazy val bRA = "bRA"
  lazy val rf  = "rf"
  lazy val RF  = "RF"
  lazy val sn  = "sn"
  lazy val jt  = "jt"
  lazy val jtx = "jtx"
  lazy val jty = "jty"
  lazy val jtz = "jtz"
  lazy val jtm = "jtm"
  lazy val lt  = "lt"
  lazy val p  = "p"
  lazy val mP  = "mP"

  lazy val UNSOL                = "UNSOL"
  lazy val ALL                  = "all"
  lazy val ENERGY               = "energy"
  lazy val SENSOR_SAMPLE_REQ_STR  = "SensorSampleReq"
  lazy val netStatConnected       = 1
  lazy val ligStatOn              = "on"
  lazy val ligStatOff             = "off"

  //units
  lazy val mW = "mW"
  lazy val W = "W"

  lazy val PROPS: String          = "props"

  //Alarms

  lazy val underPowerMsg = "Consuming less power for Driver Level "
  lazy val overPowerMsg = "Consuming more power for Driver Level "
  lazy val criticalSeverity = "Critical"
  lazy val clearSeverity = "Clear"
}
