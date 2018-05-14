package com.verizon.netsense.model

import java.net.InetAddress
import java.util.{Calendar, UUID}

import com.verizon.netsense.constants.Constants
import com.verizon.netsense.constants.Constants._
import com.verizon.netsense.entity.Entity
import com.verizon.netsense.exceptions.CustomExceptions.NoNodesFoundToSendAutoException
import com.verizon.netsense.util.TokenUtil
import com.verizon.netsense.utils.Logging
import org.joda.time.DateTime

/**
  * Created by maidapr on 1/24/18.
  */
trait CassandraDataModels extends Entity

case class Light(nodeid: String,
                 driver: Option[Int],
                 harvest_trigger: Boolean,
                 isscheduled: Boolean,
                 policy: String,
                 priority: Option[Int],
                 schedule_id: Option[String] = None,
                 schedule_time: Option[Long] = None,
                 startdt: Option[DateTime]) extends CassandraDataModels


case class LightStatusForReconcile(light: Option[Light],
                                   isScheduled: Boolean,
                                   timeout: Long) extends DeviceEvents

case class LightControlRequestHeaders(messageid: Option[String] = None,
                                      requestid: Option[String] = None,
                                      timestamp: Option[String] = None,
                                      scheduleid: Option[String] = None,
                                      success: Boolean = true,
                                      error: Option[String] = None)

case class LightStatusObj(eventType: Option[String] = None,
                          nodeids: Option[Vector[String]] = None,
                          timeout: Option[Long],
                          isScheduled: Boolean = true,
                          clearEnabled: Boolean = false,
                          harvestingEnabled: Boolean = false,
                          light: Option[Light] = None,
                          lightControlRequestHeaders: Option[LightControlRequestHeaders]) extends DeviceEvents

object LightStatusObj extends Logging with TokenUtil {

  val inetAddress = InetAddress.getLocalHost.toString

  def defaultLightMode(_lightStatusObj: LightStatusObj, _token: Option[String] = None) = {
    val scheduleIdTime = extractScheduleIdTime(_token)
    Light(nodeid = _lightStatusObj.nodeids
    .getOrElse(throw new NoNodesFoundToSendAutoException(_lightStatusObj.toJSON)).head, driver = None,
    harvest_trigger = false, isscheduled = true,
    policy = SCHEDULE_POLICY, priority = None,  schedule_id = scheduleIdTime._2, schedule_time  = scheduleIdTime._1,
      startdt = None)
  }

  def toLightControlSetAuto(_lightStatusObj: LightStatusObj): LightStatusObj = {

    _lightStatusObj.copy(eventType = Some(Constants.LIGHTING_SET_AUTO_STR), isScheduled = true,
      clearEnabled = true, light = Some(defaultLightMode(_lightStatusObj)))
  }

  def toLightControlSetAutoCASEL(_lightStatusObj: LightStatusObj): ScheduleCASEL =  _lightStatusObj match {
    case a@LightStatusObj(Some(eventType), Some(nodeids), _, _, _, _, _,
    Some(LightControlRequestHeaders(Some(msgId), Some(reqId), Some(timestamp), _, _, _))) =>
      ScheduleCASEL(messageid = Some(msgId), responsetopic = Some(""), RequestProps(inetAddress, reqId, timestamp,
        Constants.LIGHTING_SET_AUTO_STR, "ScheduleModel", "",
        orgprops = None, siteprops = None,
        nodeprops = Some(NodeProps(nodeids = _lightStatusObj.nodeids, clear = true)),
        scheduleprops = None))

    case a@LightStatusObj(Some(eventType), Some(nodeids), _, _, _, _, _, _) =>
    def randomUUID: String = UUID.randomUUID().toString
      ScheduleCASEL(messageid = Some(randomUUID), responsetopic = Some(""), RequestProps("",
      randomUUID, Calendar.getInstance().toInstant.toString,
      Constants.LIGHTING_SET_AUTO_STR, "", "",
      orgprops = None, siteprops = None,
      nodeprops = Some(NodeProps(nodeids = _lightStatusObj.nodeids, clear = true)),
      scheduleprops = None))
    case lightStatusObj => throw new RuntimeException("Unhandled exception in converting to ScheduleCASEL " +
      lightStatusObj.toJSON)
  }


  def unapplyLightAutoFields(_lightStatusObj: LightStatusObj) = _lightStatusObj match {

    case a@LightStatusObj(Some(eventType), Some(nodeids), Some(timeout), _, _, _,
    Some(Light(nodeId, _, _, _, _, _, _, _, _)), _) if nodeId != null && nodeId.nonEmpty => Some(nodeId, timeout)

    case lso => log.error("Unhandled Auto Set Case " + lso.toJSON); None
  }
}
