package com.verizon.netsense.db.neo4j

import java.util

import com.verizon.netsense.model.{Fixture, NodeModelTimeZone}
import com.verizon.netsense.utils.Logging

/**
  * Created by maidapr on 1/16/18.
  */
class Neo4jService(neo4jHelper: Neo4jHelper) extends Neo4jCyphers with Logging {

  def getSiteModelForNodeFromGraphdb(nodeId: String, siteId: String) = {
    val propsMap = new util.HashMap[String, Object]()
    propsMap.putIfAbsent("nodeid", nodeId)
    propsMap.putIfAbsent("siteid", siteId)
    neo4jHelper.getDataFromGraphDbToDTO[NodeModelTimeZone](getNodeSiteTimeZoneCypher, propsMap)
  }

  def getFixtureForNodeFromGraphDb(nodeId: String) = {
    val propsMap = new util.HashMap[String, Object]()
    propsMap.putIfAbsent("nodeid", nodeId)
    neo4jHelper.getDataFromGraphDbToDTO[Fixture](getFixtureByNodeId, propsMap)
  }

}
