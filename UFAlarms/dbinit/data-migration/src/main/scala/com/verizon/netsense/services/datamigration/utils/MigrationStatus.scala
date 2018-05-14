package com.verizon.netsense.services.datamigration.utils

import java.util.Calendar

import com.datastax.driver.core.{ResultSet, Row, SimpleStatement}

import scala.collection.JavaConverters._
import com.verizon.netsense.services.datamigration.connectors.CassandraConnector.session
import com.verizon.netsense.services.datamigration.utils.Configuration._

object MigrationStatus extends Logging {

  val migrationMap: Map[String, Boolean] = {
    try {
      val statement = new SimpleStatement(s"SELECT * FROM migration_status;")
      val rs: ResultSet = session.execute(statement)
      val resultVector: Vector[Row] = rs.iterator().asScala.toVector

      log.debug("Migration Status Data: " + resultVector)
      resultVector.map { row =>
        val key = row.getString("script_name").trim + row.getInt("version").toString
        val value = row.getBool("applied_successful")
        (key -> value)
      }.toMap
    } catch {
      case ex: Exception => log.error("ERROR: " + ex); throw ex
    }
  }

  def isMigrated(name: String, version: Int): Boolean = {
    val key = name.trim + version.toString
    if (migrationMap.isEmpty)
      false
    else
      migrationMap.getOrElse(key, false)
  }

  def updateMigration(job_name: String, version: Int, status: Boolean): Unit = {
    val query = s"""
                   | INSERT INTO $keyspace.migration_status (script_name, version, applied_successful, executed_at)
                   | VALUES ('${job_name.trim}',$version, $status,'${Calendar.getInstance().toInstant.toString}');
                   |
                 """.stripMargin
    log.debug(s"Inserting into Migration Status table: $query")
    session.execute(query)
  }

}
