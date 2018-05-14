package com.verizon.netsense.services.datamigration.utils

import com.typesafe.config.ConfigFactory

object Configuration extends Logging {
  lazy val config = ConfigFactory.load()

  private val neo4jConfig     = config.getConfig("neo4j")
  private val cassandraConfig = config.getConfig("cassandra")
  private val fileLocations   = config.getConfig("files")

  lazy val neo4jHosts    = neo4jConfig.getString("host").split(",").toSeq
  lazy val neo4jPort     = neo4jConfig.getInt("port")
  lazy val neo4jUser     = neo4jConfig.getString("username")
  lazy val neo4jPassword = neo4jConfig.getString("password")

  lazy val cassandraHosts  = cassandraConfig.getString("host").split(",").toSeq
  lazy val cassandraPort   = cassandraConfig.getInt("port")
  lazy val keyspace        = cassandraConfig.getString("keyspace")
  lazy val notifications   = cassandraConfig.getString("notifications-table-name")
  lazy val alerts          = cassandraConfig.getString("alerts-old-table-name")
  lazy val device_alerts   = cassandraConfig.getString("alerts-table-name")
  lazy val alert_mappings  = cassandraConfig.getString("alerts-by-nodeid-table")
  lazy val gps             = cassandraConfig.getString("gps-table-name")
  lazy val uf_alarms       = cassandraConfig.getString("uf-alarms-table-name")
  lazy val uf_alarmsById   = cassandraConfig.getString("uf-alarms-by-id-table-name")
  lazy val ufAlarmsFile    = fileLocations.getString("uf-alarms-file")
  lazy val migrationStatus = cassandraConfig.getString("migration-table")

  log.info("Neo4J Config: " + neo4jHosts + ":" + neo4jPort)
  log.info("Neo4J Credentials: " + neo4jUser + ":" + neo4jPassword)
  log.info("Cassandra Config: " + cassandraHosts + ":" + cassandraPort)
  log.info("Cassandra Keyspace: " + keyspace)
  log.info("Neo4J-to-Cassandra Tables: " + notifications + " , " + alerts + " , " + gps)
  log.info("Cassandra Migration Tables: ")
  log.info("Source: " + alerts + " , Target: " + device_alerts + " , " + alert_mappings)
  log.info("UF Alarms Table: " + uf_alarms + " , and " + uf_alarmsById + ". File name: " + ufAlarmsFile)
}
