package com.verizon.netsense.db

import com.verizon.netsense.db.neo4j.{Neo4jCyphers, Neo4jHelper, Neo4jService}
import com.verizon.netsense.exceptions.CustomExceptions.EmptyResultSetException
import com.verizon.netsense.model.{Fixture, NodeModelTimeZone, QueryRequestBinded, SensorQueryEnvelope}

import scala.concurrent.{ExecutionContext, Future}


class DbLayer(phantomService: PhantomService, neo4jService: Neo4jService) extends Neo4jCyphers {

  implicit val ec = ExecutionContext.Implicits.global

  def getFixtureForNode(nodeId: String): Future[Fixture] = {
    Future {
      neo4jService.getFixtureForNodeFromGraphDb(nodeId).headOption match {
        case Some(fixture) => fixture
        case None => throw  EmptyResultSetException(s"No Fixture found for given node: $nodeId")
      }
    }
  }

  def getSensorHistoryData(nodeId: String, sensorId: String, fromTime: Long, toTime: Long, limit: Int)
  : Future[List[(Long, Double)]] = {
    phantomService.getSensorHistoryEvents(nodeId,sensorId,fromTime,toTime,limit)
  }


  def getSiteModelForNode(nodeId: String, siteId: String): NodeModelTimeZone = {
      neo4jService.getSiteModelForNodeFromGraphdb(nodeId,
        siteId).headOption match {
        case Some(nodeModel) => nodeModel
        case None => throw EmptyResultSetException(nodeId)
      }
  }
}

object DbLayer {
  def apply(): DbLayer = new DbLayer(new PhantomService with ProductionDatabase{}, new Neo4jService(new Neo4jHelper) {})
}
