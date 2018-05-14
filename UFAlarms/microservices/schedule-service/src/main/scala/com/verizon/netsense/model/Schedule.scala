package com.verizon.netsense.model

import akka.actor.FSM.Timer
import com.fasterxml.jackson.core.`type`.TypeReference
import com.fasterxml.jackson.module.scala.JsonScalaEnumeration
import com.verizon.netsense.constants.Constants
import com.verizon.netsense.entity.Entity
import com.verizon.netsense.enums.Weekday
import com.verizon.netsense.exceptions.CustomExceptions.{NoNodesFoundToSendScheduleException, NoScheduleFoundException}
import com.verizon.netsense.utils.Logging

/**
  * Created by ssaurav on 1/2/18.
  */
//Just a place holder please ignore
case class Schedule(uuid: String,
                    priority: Int,
                    driverLevel: String )

//This is an action that we will be taken up by a slot in the device. It says set to the given driver at the specified time.
case class Action (level: Option[Int],
                   time:  Option[String])


class WeekdayType extends TypeReference[Weekday.type]

//We push daily  schdules to the device. From days/dates and tz we have to find out the applicable day.
//dates have preference over day
//photocell_highLevel: driver level when it is dark
//photocell_lowLevel: driver level when it is not dark
case class ScheduleEvent (@JsonScalaEnumeration(classOf[WeekdayType])
                           days: Option[Vector[Weekday.Value]] = None,
                           date: Option[String] = None,
                           actions: Vector[Action],
                           photocell_enabled: Option[Boolean],
                           photocell_highLevel: Option[Int],
                           photocell_lowLevel: Option[Int])

//This is the value used by the device when it is off network.
//photocell_highLevel: driver level when it is dark
//photocell_lowLevel: driver level when it is not dark
case class Network ( highTime : Option[String],
                     highLevel: Option[Int],
                     lowTime: Option[String],
                     lowLevel: Option[Int],
                     photocell_enabled: Option[Boolean],
                     photocell_highLevel: Option[Int],
                     photocell_lowLevel: Option[Int]
                    )

//nodes are all the nodeids the schedule should be applied to. lat and long are the latitude and longitude of the site.
//We may want to pass the timezone instead of the lat and long.
case class LightingSchedule(events: scala.Vector[ScheduleEvent],
                            network: Option[Network],
                            nodes: Option[scala.Vector[String]],
                            latitude: String = "0",
                            longitude: String = "0",
                            scheduleid: Option[String] = Some(Constants.DEFAULT)) extends Entity

object LightingSchedule {


  def toLightControlObj(ls: LightingSchedule): LightStatusObj = {
    val lso = LightStatusObj(eventType = Some(Constants.LIGHTING_SCHEDULE_EVENT_STR), nodeids = ls.nodes, timeout = None,
      isScheduled = true, clearEnabled = false, harvestingEnabled = false,
      lightControlRequestHeaders = Some(LightControlRequestHeaders(scheduleid = ls.scheduleid)))
    lso.copy(light = Some(LightStatusObj.defaultLightMode(lso, ls.scheduleid)))
  }

  def constructLightScheduleFromDbResp(nH: EssentialNodeHierarchyFields, lS: LightingSchedule): LightingSchedule = {
    LightingSchedule(lS.events, lS.network,
      Some(nH.nodeids), nH.latitude.getOrElse("0"), nH.longitude.getOrElse("0"),
      Some(lS.scheduleid.getOrElse(Constants.DEFAULT)))
  }
}
