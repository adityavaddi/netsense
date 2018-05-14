package com.verizon.netsense.model

import com.verizon.netsense.entity.Entity

/**
  * Created by ssaurav on 1/2/18.
  */

//dow will start from sunday sun = 0
case class ScheduleCalendar(hr: Int, min: Int, sec: Int, year: Int, month: Int, dow: Int, dom: Int) {
  require(0 <= hr && hr <= 24)
  require(0 <= min && min <= 60)
  require(0 <= sec && sec <= 60)
  require(1 <= month && month <= 12)
  require(0 <= dow && dow < 7)
  require(1 <= dom && dom <= 31)
}

/* level: driver level when ambient light is above crossover lux level
   lowLevel: driver level when ambient light is below crossover lux level(photocell mode on device)
 */
case class SlotAction(level    : Int,
                      lowLevel : Option[Int],
                      time     : Option[ScheduleCalendar],
                      mode     : String
                     ) {
  require(mode == "photocell" || mode == "normal")
}

case class STSModel(id      : String,
                    slots   : Option[Vector[SlotAction]],
                    network : Vector[SlotAction],
                    nodeids : Vector[String],
                    name    : String = "LightingScheduledEvent") extends Entity

//Light Force
case class LFSPayload (level  : Array[Int] ) extends Entity

case class LightControlSTSModel(id     : String,
                                name   : String,
                                nodeids: Vector[String],
                                payload: Array[Byte]) extends Entity {
  require(name == "LightingForceState" || name == "LightingSetAuto")
}