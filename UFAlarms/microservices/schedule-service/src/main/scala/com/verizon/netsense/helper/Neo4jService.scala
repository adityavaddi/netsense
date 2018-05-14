package com.verizon.netsense.helper

import java.util

import com.verizon.netsense.cyphers.Neo4jCyphers
import com.verizon.netsense.model._
import com.verizon.netsense.utils.Logging

import scala.collection.JavaConverters._

/**
  * Created by maidapr on 1/16/18.
  */
class Neo4jService(neo4jHelper: Neo4jHelper) extends Neo4jCyphers with Logging {

  def updateLastSentTimeForSchedule(scheduleId: String, timeNow: String) = {
    val propsMap = new util.HashMap[String, Object]()
    propsMap.putIfAbsent("scheduleid", scheduleId)
    propsMap.putIfAbsent("now", timeNow)
    neo4jHelper.getDataFromGraphDb(update_last_sent_schedue, propsMap)
  }

  def getNodeOrgDataForNodeFromGraphDb(nodeId: String) = {
    val propsMap = new util.HashMap[String, Object]()
    propsMap.putIfAbsent("nodeid", nodeId)
    neo4jHelper.getDataFromGraphDbToDTO[NodeOrgHierarchyData](get_site_and_org_for_node, propsMap)
      .map { x => log.debug("neo4j result " + x); x.copy(nodeid = Some(nodeId))}
  }

  def getScheduleToBeSendByTimeZoneFromGraphDb(timeZones: Vector[String], models: Vector[String]) = {
    val propsMap = new util.HashMap[String, Object]()
    propsMap.putIfAbsent("timezones", new util.ArrayList[String](timeZones.toList.asJava))
    propsMap.putIfAbsent("models", new util.ArrayList[String](models.toList.asJava))
    neo4jHelper.getDataFromGraphDbToDTO[List[NodeSiteListByTimeZone]](get_all_sites_schedules_by_timezone, propsMap)
  }

  def getSiteLatLongFromGraphDb(siteId: String) = {
    val propsMap = new util.HashMap[String, Object]()
    propsMap.putIfAbsent("siteid", siteId)
    neo4jHelper.getDataFromGraphDbToDTO[SiteCoordinates](get_lat_long_for_site, propsMap)
  }

  def getScheduleToSendFromGraphDb(scheduleId: String) = {
    val propsMap = new util.HashMap[String, Object]()
    propsMap.putIfAbsent("scheduleid", scheduleId)
    neo4jHelper.getDataFromGraphDbToDTO[LightingSchedule](send_assigned_schedule_to_node, propsMap)
  }

}
