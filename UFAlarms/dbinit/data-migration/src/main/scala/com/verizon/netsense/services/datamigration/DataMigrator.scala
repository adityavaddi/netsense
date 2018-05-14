package com.verizon.netsense.services.datamigration

import com.verizon.netsense.services.datamigration.connectors.{CassandraConnector, Neo4jConnector}
import com.verizon.netsense.services.datamigration.migration._
import com.verizon.netsense.services.datamigration.utils.Logging

/*
  Data Migration: A one-time execution script for data migration.
  Add New Migration Job: Define an Object extending `Migration` trait. Write the migration code under defined object.
    Give a unique job-name for your migration and add it to the `migrationsToBeApplied` list.
    Version should always be 1. Changing version would affect the Idempotence of the script
  Remove Migration Job: Remove the Migration job from `migrationsToBeApplied` list.
  Job Status: A record will be inserted into `migration_status` Cassandra table indicating the status of the job
  Job Failure: If the Job fails, verify that the status is 'false' in the database and re-run the job.
  Job Re-run: If you want to re-run the job again, delete the corresponding row from the `migration_status` table.
 */

object DataMigrator extends App with Logging {

  //Version should always be 1. It's a placeholder for future functionality.
  private val VERSION = 1

  //Data Migration Jobs for 3.0.6
  private val MIGRATE_NEO4J_TO_CASSANDRA_ALERTS = "alerts"
  private val MIGRATE_NEO4J_TO_CASSANDRA_NOTIFICATIONS = "notifications"

  //Data Migration Jobs for 3.0.7
  private val MIGRATE_NEO4J_TO_CASSANDRA_GPS = "neo4j_gps"
  private val MIGRATE_CASSANDRA_ALERTS_TO_DEVICE_ALERTS = "alerts_to_device_alerts"

  //Data Migration Jobs for 3.0.8 - To pre-load UF_Alarms
  private val LOAD_UF_ALARMS_V1 = "load_uf_alarms_v1"

  //Add the migration that needs to applied for a release
  private val migrationsToBeApplied: List[(String,Int, Migration)] =
      (LOAD_UF_ALARMS_V1,VERSION, UFAlarmLoader) :: Nil

  log.info("--------------------------------- Data Migration Started --------------------------------")
  migrationsToBeApplied.foreach(m => m._3.applyMigration(m._1, m._2))
  log.info("--------------------------- Data Migration Complete ------------------------")

  closeConnection

  log.warn("Data Migration Service Exiting....")
  System.exit(0)

  private def closeConnection() = {
    log.info("--------------------------- Closing Connections  ------------------------")
    CassandraConnector.closeConnection()
    Neo4jConnector.closeConnection()
  }

}
