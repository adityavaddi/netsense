package com.verizon.netsense.services.datamigration.migration

import java.util

import com.verizon.netsense.services.datamigration.connectors.CassandraConnector.session
import com.verizon.netsense.services.datamigration.connectors.Neo4jConnector.neo4jSession
import com.verizon.netsense.services.datamigration.cyphers.Cyphers.{allNotificationsCypher, notificationsQuery}
import org.neo4j.driver.v1.StatementResult

import scala.collection.JavaConverters._

object Neo4JNotifications extends Migration {

  private var notificationNeo4JCounter = 0
  private var notificationNeo4JFailureCounter = 0

  def migrateNotifications(): Unit = {
    log.info("************************* Migrating Notifications from Neo4J to Cassandra *************************")

    val result: StatementResult = neo4jSession.run(allNotificationsCypher)
    while (result.hasNext) {
      try {
        val record: util.Map[String, AnyRef] = result.next().asMap()
        log.debug(s"Neo4J record: $record")
        val queryString = notificationsQuery(record.asScala)
        log.debug(s"Cassandra Query: $queryString")
        session.execute(queryString)
        notificationNeo4JCounter = notificationNeo4JCounter + 1
      } catch {
        case e: Exception =>
          e.printStackTrace()
          log.error(
            s"Exception while persisting notifications to Cassandra: $e")
          notificationNeo4JFailureCounter = notificationNeo4JFailureCounter + 1
      }
    }
    printNotificationCounters
  }

  def printNotificationCounters = {
    log.info(s"Neo4J to Cassandra Notifications Success Count: $notificationNeo4JCounter . " +
      s"Neo4J to Cassandra Notifications Failure Count: $notificationNeo4JFailureCounter")
  }

  override def migrate(): Unit = {
    migrateNotifications()
  }
}
