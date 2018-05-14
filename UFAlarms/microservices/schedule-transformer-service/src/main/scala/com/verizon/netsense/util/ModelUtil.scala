package com.verizon.netsense.util

import com.verizon.netsense.model.{SlotAction, ScheduleCalendar}

object ModelUtil {

  def sortSlotsByTime(slots: Vector[SlotAction]) : Vector[SlotAction] = {
      slots.sortWith { (leftExpr, rightExpr) =>
        //If no time of day means that is for entire day and hence should be below in order
        val leftTime = leftExpr.time.getOrElse(new ScheduleCalendar(0,0,0,0,0,0,0))
        val leftExprInSec = leftTime.sec + (60 * leftTime.min) + (3600 * leftTime.hr)

        val rightTime = rightExpr.time.getOrElse(new ScheduleCalendar(0,0,0,0,0,0,0))
        val rightExprInSec = rightTime.sec + (60 * rightTime.min) + (3600 * rightTime.hr)
        leftExprInSec < rightExprInSec
      }
  }
}
