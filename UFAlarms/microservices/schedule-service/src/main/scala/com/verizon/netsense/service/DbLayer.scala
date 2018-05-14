package com.verizon.netsense.service

import com.outworkers.phantom.dsl.ResultSet
import com.verizon.netsense.cyphers.Neo4jCyphers
import com.verizon.netsense.database.{PhantomService, ProductionDatabase}
import com.verizon.netsense.helper.{Neo4jHelper, Neo4jService}
import com.verizon.netsense.model.Light

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by maidapr on 1/10/18.
  */
class DbLayer(phantomService: PhantomService, neo4jService: Neo4jService) extends  Neo4jCyphers {

  def getSiteOrgDataForNode(nodeId: String)(implicit ec: ExecutionContext) = {
    neo4jService.getNodeOrgDataForNodeFromGraphDb(nodeId)
  }

  def getLatLongForSite(siteId: String)(implicit ec: ExecutionContext)  = {
    neo4jService.getSiteLatLongFromGraphDb(siteId)
  }

  def getScheduleToSendFromDbById(scheduleId: String)(implicit ec: ExecutionContext) =
    Future {
      neo4jService.getScheduleToSendFromGraphDb(scheduleId)
    }

  def getScheduleToBeSendByTimeZone(timeZones: Vector[String], models: Vector[String])(implicit ec: ExecutionContext) =
    Future {
      neo4jService.getScheduleToBeSendByTimeZoneFromGraphDb(timeZones, models).flatten
    }

  def updateLastSentScheduleTimeInGraphDb(scheduleId: String, timeNow: String)(implicit ec: ExecutionContext) =
    Future {
      neo4jService.updateLastSentTimeForSchedule(scheduleId, timeNow)
    }

  def getLatestLightMode(nodeId: String)(implicit ec: ExecutionContext): Future[Option[Light]] = {
    phantomService.getLatestLightModeByNodeId(nodeId)
  }

  def storeLatestLightMode(light: Light)(implicit ec: ExecutionContext): Future[ResultSet]  = {
    phantomService.storeLatestLightModeByNodeId(light)
  }

}
object DbLayer {
  def apply(): DbLayer = new DbLayer(new PhantomService with ProductionDatabase{}, new Neo4jService(new Neo4jHelper) {})
}
