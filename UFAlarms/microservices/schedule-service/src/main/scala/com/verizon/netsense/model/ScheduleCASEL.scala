package com.verizon.netsense.model

import com.verizon.netsense.constants.Constants
import com.verizon.netsense.entity.Entity
import com.verizon.netsense.exceptions.CustomExceptions.UnableToGetScheduleFromCasel
import org.joda.time.DateTime

/**
  * Created by maidapr on 2/6/18.
  */

case class ScheduleCASEL(messageid: Option[String] = None,
                         responsetopic: Option[String] = None,
                         request: RequestProps) extends Entity

case class OrgProps(orgid: Option[String])

case class SiteProps(siteid: Option[String])

case class RequestProps(instanceid: String,
                        requestid: String,
                        timestamp: String,
                        `type`: String,
                        model: String,
                        user: String,
                        orgprops: Option[OrgProps],
                        siteprops: Option[SiteProps],
                        nodeprops: Option[NodeProps],
                        scheduleprops: Option[LightingSchedule])
case class ExtProps(level: Option[Int],
                    timeout: Option[Long])

case class NodeProps(harvesting: Boolean = false,
                     level: Option[Int] = None,
                     nodeids: Option[Vector[String]] = Option.empty,
                     timeout: Option[Int] = None,
                     clear: Boolean = false,
                     `type`: Option[String] = None
                    )
object ScheduleCASEL {

  lazy val predicateForLightControlType: (String) => (Boolean) = (eventType) => {
    eventType.toLowerCase match {
      case requestType if requestType != null && requestType.nonEmpty =>
        requestType.contains(Constants.IS_LIGHT_CONTROL_TYPE.toLowerCase) ||
        requestType.contains(Constants.LIGHTING_FORCE_STATE_STR.toLowerCase())
      case _ => false
    }
  }

  lazy val predicateForLightSetAutoType: (String) => (Boolean) = (eventType) => {
    eventType.toLowerCase match {
      case requestType if requestType != null && requestType.nonEmpty =>
        predicateForLightControlType(requestType) ||
        requestType.contains(Constants.LIGHTING_SET_AUTO_STR.toLowerCase)
      case _ => false
    }
  }

  def extractScheduleProps(scheduleCASEL: ScheduleCASEL): Option[LightingSchedule] = scheduleCASEL match {
    case ScheduleCASEL(_, _, RequestProps(_, _, _, "applySchedule" | "updateSchedule", _, _, _, _,
    Some(NodeProps(_, _, Some(nodeids), _, _, _)), Some(scheduleProps))) =>
      Some(scheduleProps.copy(nodes = Some(nodeids)))
    case scheduleCasel => throw new UnableToGetScheduleFromCasel(" request type out of scope " + scheduleCasel.toJSON)
  }

  def extractLightStatusObj(scheduleCASEL: ScheduleCASEL): LightStatusObj = scheduleCASEL match {

      // For LightingForceState
    case ScheduleCASEL(Some(msgId), Some(rspTopic), RequestProps(_, reqId, timeStamp,
    (requestType), _, _, _, _,
    Some(NodeProps(_hvt, Some(_level), Some(nodeids), Some(timeout), clear, Some(eventType))), _)) if nodeids.nonEmpty
      && predicateForLightControlType(requestType) =>

      LightStatusObj(Some(Constants.LIGHTING_FORCE_STATE_STR), Some(nodeids), Some(timeout), isScheduled = false,
        light = Some(Light(nodeid = "", driver = Some(_level),
        harvest_trigger = _hvt, isscheduled = false, policy = Constants.OVERRIDE_POLICY,
        priority = Some(Constants.FORCE_STATE_PRIORITY), startdt = Some(DateTime.now.plusSeconds(timeout)))),
        lightControlRequestHeaders = Some(LightControlRequestHeaders(messageid = Some(msgId), requestid = Some(reqId),
          timestamp = Some(timeStamp), scheduleid = None,
          success = true, error = None)))

    // Failure Response if no nodeids found
    case ScheduleCASEL(Some(msgId), _, RequestProps(_, reqId, timeStamp,
    (requestType), _, _, _, _,
    Some(NodeProps(_hvt, Some(_level), Some(nodeids), Some(timeout), clear, Some(eventType))), _)) if nodeids.isEmpty
    && predicateForLightControlType(requestType) =>

      LightStatusObj(Some(eventType), Some(nodeids), Some(timeout), isScheduled = true, light = Some(Light(nodeid = "",
        driver = Some(_level),
        harvest_trigger = _hvt, isscheduled = false, policy = Constants.OVERRIDE_POLICY,
        priority = Some(Constants.FORCE_STATE_PRIORITY), startdt = None)),
        lightControlRequestHeaders = Some(LightControlRequestHeaders(messageid = Some(msgId), requestid = Some(reqId),
          timestamp = Some(timeStamp), scheduleid = None, success = false, error = Some("No Nodes defined in the request"))))

    // For LightingSetAuto
    case ScheduleCASEL(_, _, RequestProps(_, _, _,
    (requestType), _, _, _, _,
    Some(NodeProps(_hvt, _, Some(nodeids), _, clear, _)), _)) if predicateForLightSetAutoType(requestType) =>

      LightStatusObj(Some(Constants.LIGHTING_SET_AUTO_STR), Some(nodeids), None, isScheduled = true, clearEnabled = true,
        light = Some(Light(nodeid = "", driver = None,
        harvest_trigger = _hvt, isscheduled = true, policy = Constants.SCHEDULE_POLICY,
        priority = Some(Constants.SCHEDULE_PRIORITY), startdt = None)),
        lightControlRequestHeaders = Some(LightControlRequestHeaders(messageid = None, requestid = None,
          timestamp = None, scheduleid = None, success = true, error = None)))

    case _scheduleCASEL => throw new RuntimeException("Unable to process the msg " + _scheduleCASEL.toJSON)

  }

}