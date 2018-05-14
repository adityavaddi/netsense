package com.verizon.netsense.model

import com.verizon.netsense.entity.Entity

/**
  * Created by maidapr on 2/8/18.
  */
case class ScheduleLoopTrigger(messageid: String,
                               responsetopic: String,
                               request: ScheduleTriggerRequestProps) extends Entity

case class ScheduleTriggerRequestProps(instanceid: String,
                                       requestid: String,
                                       timestamp: String,
                                       `type`: String,
                                       model: String,
                                       user: String,
                                       extprops: TriggerExtProps)

case class TriggerExtProps(time: Option[String])

object UnAppliedScheduleLoopTrigger {

  def unapply(scheduleLoopTrigger: ScheduleLoopTrigger) = scheduleLoopTrigger.request.extprops.time

}
