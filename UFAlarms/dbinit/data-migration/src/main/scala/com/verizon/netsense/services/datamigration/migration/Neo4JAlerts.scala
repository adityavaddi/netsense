package com.verizon.netsense.services.datamigration.migration

import java.util

import com.verizon.netsense.services.datamigration.connectors.CassandraConnector.session
import com.verizon.netsense.services.datamigration.connectors.Neo4jConnector.neo4jSession
import com.verizon.netsense.services.datamigration.cyphers.Cyphers.{alertsQuery, allAlertsCypher}
import org.neo4j.driver.v1.Record

import scala.collection.JavaConverters._

object Neo4JAlerts extends Migration {

  private var alertNeo4JCounter               = 0
  private var alertNeo4JFailureCounter        = 0

  def migrateAlerts(): Unit = {
    log.info("************************* Migrating Alerts from Neo4J to Cassandra *************************")

    val result: util.List[Record] = neo4jSession.run(allAlertsCypher).list()
    val resultVector = result.asScala.toVector
    val resultMap: Map[String, String] =
      try {
        resultVector.map { record =>
          log.debug(s"Neo4J record: $record")
          alertsQuery(record.asMap.asScala)
        }.toMap
      } catch {
        case e: Exception =>
          alertNeo4JFailureCounter = alertNeo4JFailureCounter + 1
          e.printStackTrace
          log.error(s"Exception while reading alerts from Neo4j: $e")
          Map()
      }

    resultMap.map { alert =>
      try {
        log.debug(s"Cassandra Query: ${alert._2}")
        session.execute(alert._2)
        alertNeo4JCounter = alertNeo4JCounter + 1
      } catch {
        case e: Exception =>
          e.printStackTrace()
          log.error(s"Exception while reading alerts from Neo4j: $e")
          alertNeo4JFailureCounter = alertNeo4JFailureCounter + 1
      }
    }
    printAlertsCounters
  }

  def printAlertsCounters = {
    log.info(s"Neo4J to Cassandra Alerts Success Count: $alertNeo4JCounter . " +
      s"Neo4J to Cassandra Alerts Failure Count: $alertNeo4JFailureCounter")
  }

  override def migrate(): Unit = {
    migrateAlerts()
  }
}
