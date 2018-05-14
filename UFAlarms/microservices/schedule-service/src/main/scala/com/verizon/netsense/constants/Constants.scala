package com.verizon.netsense.constants

/**
  * Created by ssaurav on 1/2/18.
  */
object Constants {

  lazy val SCHEDULE_TOKEN_PREFIX_STR            = "schedule"
  lazy val SCHEDULE_TOKEN_DELIMITER_STR         = ':'
  lazy val SCHEDULE_TOKEN_VALUE_DELIMITER_STR   = '@'
  lazy val PHOTOCELL: String      = "photocell"
  lazy val NORMAL: String         = "normal"
  lazy val PHOTOCELL_LOW: String  = "photocell_low"
  lazy val PHOTOCELL_HIGH: String = "photocell_high"
  lazy val DEFAULT_DRIVER_LEVEL_LOW    = 0
  lazy val DEFAULT_DRIVER_LEVEL_HIGH   = 100
  lazy val ALL                    = "all"
  lazy val PROPS: String          = "props"
  lazy val DEFAULT: String        = "default"
  lazy val UNKNOWN: String        = "Unknown"

  lazy val SCHEDULE_POLICY: String = "scheduled"
  lazy val SCHEDULE_PRIORITY: Int   = 7
  lazy val FORCE_STATE_PRIORITY: Int = 0
  lazy val OVERRIDE_POLICY: String = "override"
  lazy val LIGHTING_SCHEDULE_EVENT_STR: String = "LightingScheduleEvent"
  lazy val LIGHTING_FORCE_STATE_STR: String = "LightingForceState"
  lazy val LIGHTING_SET_AUTO_STR: String = "LightingSetAuto"
  lazy val IS_LIGHT_CONTROL_TYPE: String = "lighting-control"

}
