package com.verizon.netsense.model

import com.verizon.netsense.entity.Entity

/* Lighting Control
  Qualifier bits describing to which events this control entry applies (presence detector, ambient light, etc.):
  1<<0: Motion sensor
  1<<1: Ambient light sensor
  1<<2: No network
  1<<3: Remote motion sensor
  1<<4: Remote ambient light sensor
  1<<31: Disqualified – this entry is never true. Used with pri field to shadow/overlay other schedule/control entries.
*/
case class LightingControl(pri  : Int,
                           quals: Int,
                           mask : Int = 1,
                           level: Array[Int] // can be changed to Unsigned
                          ) extends Entity {
      require(0 <= level(0) && level(0) <= 100)
      require(mask == 1)
}

/*Calendar/Schedule Entry
  Bitfield describing days of the week for which this entry is effective. LSB == Sunday.
  Bitfield describing days of the month for which this entry is effective. LSB == First of month.
  Bitfield describing months of the year for which this entry is effective. LSB == January.
  Bitfield describing years for which this entry is effective, modulo 16. Year bit is computed as 1 << (year % 16).
*/
case class SchCalendar (
                         sec : Int,
                         min : Int,
                         hour: Int,
                         wday: Int,
                         mday : Int,
                         mon : Int,
                         year : Int
                       ) extends Entity


//Light Force State
case class LightControlPayload(`type`: Int, lctl: LightingControl, rates: Array[Int]) extends Entity

//Request for Schedule Add
case class SlotPayload(id: Int, cal: SchCalendar, lctl: LightingControl) extends Entity
case class AddSchedulePayload(Entries: Vector[SlotPayload]) extends Entity
case class AIOSchedulePayload(Entries: Vector[SlotPayload]) extends Entity

//Schedule Commit
case class CommitSchedulePayload(token: String) extends Entity


//The rest pack format
case class RestPackCommand(a: String, // Action
                         p: String, // Topic
                         sid: String, // Nodeid
                         d: String, // Date
                         uuid: String, // UUID
                         f: String,
                         l: Array[Byte], //Payload
                         t: String = ""  // token
                          ) extends Entity
