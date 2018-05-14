package com.verizon.netsense.model

import com.verizon.netsense.constants.Constants.{DEFAULT, UNKNOWN}
import com.verizon.netsense.entity.Entity
import com.verizon.netsense.exceptions.CustomExceptions.NoNodeDataFoundInGraphDb

/**
  * Created by maidapr on 1/24/18.
  */
sealed trait Neo4jGraphModel extends Entity

// TODO better naming
case class EssentialNodeHierarchyFields(nodeids: Vector[String] = Vector.empty,
                                        siteid: Option[String] = None,
                                        orgid: Option[String] = None,
                                        scheduleid: Option[String] = None,
                                        latitude: Option[String] = None,
                                        longitude: Option[String] = None) extends Entity

case class NodeOrgHierarchyData(nodeid:           Option[String] = None,
                                nodebssid:        Option[String] = None,
                                nodehw:           Option[String] = None,
                                softwareVersion:  Option[String] = None,
                                nodename:         Option[String] = None,
                                nodelabels:       Vector[Option[String]] = Vector.empty,
                                siteid:           Option[String] = Some(UNKNOWN),
                                sitename:         Option[String] = None,
                                siteaddress:      Option[String] = None,
                                latitude:         Option[String] = Some("0"),
                                longitude:        Option[String] = Some("0"),
                                sitelabels:       Vector[Option[String]] = Vector.empty,
                                orgid:            Option[String] = Some(UNKNOWN),
                                orgname:          Option[String] = None,
                                scheduleid:       Option[String] = Some(DEFAULT),
                                configid:         Option[String] = Some(DEFAULT)) extends Entity with Neo4jGraphModel {

  def createEssentialNodeOrgHierarchy: EssentialNodeHierarchyFields = {
    EssentialNodeHierarchyFields(nodeids = Vector(nodeid.getOrElse(throw new NoNodeDataFoundInGraphDb())),
      siteid = siteid, orgid = orgid, scheduleid = scheduleid, latitude = latitude, longitude = longitude)
  }
}


object NodeOrgHierarchyData {
  def applyDefaultConfig(nodeOrgHierarchyData: NodeOrgHierarchyData): NodeOrgHierarchyData = {
    NodeOrgHierarchyData(
      nodeid = nodeOrgHierarchyData.nodeid,
      siteid = Some(UNKNOWN),
      orgid = Some(UNKNOWN),
      scheduleid = Some(DEFAULT),
      configid = Some(DEFAULT),
      latitude = Some("0"),
      longitude = Some("0"))
  }
}

case class SiteCoordinates(latitude:          Option[String] = None,
                           longitude:         Option[String] = None) extends Neo4jGraphModel

case class NodeSiteListByTimeZone(sitename: Option[String] = None,
                                  siteid: Option[String] = None,
                                  nodeids: Vector[String] = Vector.empty,
                                  scheduleid: Option[String] = Some("default"),
                                  latitude: Option[String] = Some("0"),
                                  longitude: Option[String] = Some("0")) extends SchObjToNodeHierarchyConverter

trait SchObjToNodeHierarchyConverter {

  def createEssentialNodeOrgHierarchy(nodeSiteListByTimeZone: NodeSiteListByTimeZone): EssentialNodeHierarchyFields = {
    EssentialNodeHierarchyFields(nodeids = nodeSiteListByTimeZone.nodeids,
      siteid = nodeSiteListByTimeZone.siteid, orgid = None, scheduleid = nodeSiteListByTimeZone.scheduleid,
      latitude = nodeSiteListByTimeZone.latitude, longitude = nodeSiteListByTimeZone.longitude)
  }
}