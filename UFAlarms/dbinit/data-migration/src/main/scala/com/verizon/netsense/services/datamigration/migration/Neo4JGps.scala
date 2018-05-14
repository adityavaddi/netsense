package com.verizon.netsense.services.datamigration.migration

import java.util

import com.verizon.netsense.services.datamigration.connectors.CassandraConnector.session
import com.verizon.netsense.services.datamigration.connectors.Neo4jConnector.neo4jSession
import com.verizon.netsense.services.datamigration.cyphers.Cyphers.{gpsCypher, gpsQuery}
import org.neo4j.driver.v1.StatementResult

import scala.collection.JavaConverters._

object Neo4JGps extends Migration {

  private var gpsNeo4JCounter                 = 0
  private var gpsNeo4JFailureCounter          = 0

  def migrateGps(): Unit = {
    log.info("************************* Migrating GPS from Neo4J to Cassandra *************************")

    val result: StatementResult = neo4jSession.run(gpsCypher)
    while (result.hasNext) {
      try {
        val record: util.Map[String, AnyRef] = result.next().asMap()
        log.debug(s"Neo4J record: $record")
        val queryString = gpsQuery(record.asScala)
        log.debug(s"Cassandra Query: $queryString")
        session.execute(queryString)
        gpsNeo4JCounter = gpsNeo4JCounter + 1
      } catch {
        case e: Exception =>
          e.printStackTrace()
          log.error(s"Exception while reading gps from Neo4j: $e")
          gpsNeo4JFailureCounter = gpsNeo4JFailureCounter + 1
      }
    }
    printGpsCounters
  }

  def printGpsCounters = {
    log.info(s"Neo4J to Cassandra GPS Success Count: $gpsNeo4JCounter . " +
      s"Neo4J to Cassandra GPS Failure Count: $gpsNeo4JFailureCounter")
  }

  override def migrate(): Unit = {
    migrateGps()
  }
}
