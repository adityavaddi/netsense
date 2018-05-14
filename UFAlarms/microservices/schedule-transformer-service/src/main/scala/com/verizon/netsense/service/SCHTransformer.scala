package com.verizon.netsense.service

import com.verizon.netsense.constants.Constants
import com.verizon.netsense.model._
import com.verizon.netsense.util.ModelUtil
import com.verizon.netsense.utils.Logging

object ScheduleTransformer extends Logging {
  val NoNetworkPriority = 2
  val NoNetworkQualifier = 4
  val NormalPriority = 7
  val NormalQualifier = 0
  val PhotocellQualifier = 2
  val defaultCalendar = new SchCalendar(hour = 0, min = 0, sec = 0,
          mday = 2147483647, mon = 4095, year = 65535, wday = 127 )

  //TODO: Remove magic numbers
  def transform(schedule: STSModel): Vector[SlotPayload] = {
    log.debug(s"Transforming the STSModel: ${schedule.toJSON}")

    val networkSlots = calculateNoNetworkSlots(schedule.network)
    log.debug(s"Post Transform Network Slots:")
    networkSlots.foreach(sp => log.debug(s"slot: ${sp.toJSON}"))

    val normalSlots = calculateActionsSlots(schedule.slots.getOrElse(Vector()))
    log.debug(s"Post Transform Normal Slots:")
    normalSlots.foreach(sp => log.debug(s"slot: ${sp.toJSON}"))

    networkSlots ++ normalSlots
  }

  private def convertCalendar(calendar: ScheduleCalendar) : SchCalendar = {
    new SchCalendar(
      hour = calendar.hr,
      min = calendar.min,
      sec = calendar.sec,
      mday = 1 << calendar.dom - 1,
      mon = 1 << calendar.month - 1,
      year = 1 << (calendar.year % 16),
      wday = 1 << calendar.dow )
  }

  private def calculateActionsSlots(actions: Vector[SlotAction]) : Vector[SlotPayload] = {
    var slotId = 1

    val photocellAction = actions.filter(_.mode == Constants.PHOTOCELL)
    photocellAction.isEmpty match {
      case false => {
        val action = photocellAction.head
        Vector(
            new SlotPayload(id = 2,
              lctl = new LightingControl(level = Array(action.lowLevel.getOrElse(100)),
                  quals = ScheduleTransformer.PhotocellQualifier,
                  pri = ScheduleTransformer.NormalPriority - 1),
                  cal = defaultCalendar
            ),
          new SlotPayload(id = 3,
            lctl = new LightingControl(level = Array(action.level),
              quals = ScheduleTransformer.NormalQualifier,
              pri = ScheduleTransformer.NormalPriority),
              cal = defaultCalendar
          )
        )
      }
      case true => {
        val sortedActions = ModelUtil.sortSlotsByTime(actions)
        sortedActions.map( action => {
          val timeOfDay = action.time.get
          slotId += 1
          new SlotPayload(id = slotId,
            lctl = new LightingControl(level = Array(action.level),
              quals = ScheduleTransformer.NormalQualifier,
              pri = ScheduleTransformer.NormalPriority),
              cal = convertCalendar(action.time.get)
          )
        })
      }
    }
  }

  private def calculateNoNetworkSlots(networks: Vector[SlotAction]) : Vector[SlotPayload] = {
    var slotId = -1

    val photocellAction = networks.filter(_.mode == Constants.PHOTOCELL)
    photocellAction.isEmpty match {
      case false => {
        val action = photocellAction.head
        Vector(
            new SlotPayload(id = 0,
                    lctl = new LightingControl(level = Array(action.lowLevel.getOrElse(100)),
                                    quals = ScheduleTransformer.PhotocellQualifier + ScheduleTransformer.NoNetworkQualifier,
                                    pri = ScheduleTransformer.NoNetworkPriority),
                    cal = defaultCalendar
            ),
            new SlotPayload(id = 1,
              lctl = new LightingControl(level = Array(action.level),
                quals = ScheduleTransformer.NoNetworkQualifier,
                pri = ScheduleTransformer.NoNetworkPriority + 1),
                cal = defaultCalendar
            )
        )
      }
      case true => {
        val sortedNetworks = ModelUtil.sortSlotsByTime(networks)
        sortedNetworks.map( action => {
          slotId += 1
          new SlotPayload(id = slotId,
            lctl = new LightingControl(level = Array(action.level),
              quals = ScheduleTransformer.NoNetworkQualifier,
              pri = ScheduleTransformer.NoNetworkPriority),
            cal = convertCalendar(action.time.get)
          )
        })
      }
    }
  }

}