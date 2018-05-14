package com.verizon.netsense.model

import com.verizon.netsense.entity.Entity
import org.joda.time.DateTime

case class NodeLightModel(name : String, nodeid: String, level: Option[Array[Int]])

case class NodeSCHModel(slots: Vector[SlotPayload], nodeid: String, scheduleId: String)

case class Light(nodeid: String,
                 driver: Option[Int],
                 harvest_trigger: Boolean,
                 isscheduled: Boolean,
                 policy: String,
                 priority: Option[Int],
                 startdt: Option[DateTime]) extends Entity