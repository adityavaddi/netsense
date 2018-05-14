package com.verizon.netsense.services.datamigration.migration

import com.datastax.driver.core.{ResultSet, Row, SimpleStatement}
import com.verizon.netsense.services.datamigration.connectors.CassandraConnector.session
import com.verizon.netsense.services.datamigration.cyphers.Cyphers.{processBoolean, processString}
import com.verizon.netsense.services.datamigration.utils.Configuration.{alert_mappings, alerts, device_alerts, keyspace}
import com.verizon.netsense.services.datamigration.utils.Logging

import scala.collection.JavaConverters._

object CassandraAlerts extends Migration {

  private var alertCassandraCounter           = 0
  private var alertCassandraFailureCounter    = 0

  def migrateCassandraAlerts() = {
    log.info("************************* Migrating Alerts from alerts to device_alerts table *************************")

    val statement = new SimpleStatement(s"SELECT * FROM $alerts;")
    val rs: ResultSet = session.execute(statement)
    val resultVector: Vector[Row] = rs.iterator().asScala.toVector

    val alertMap: Map[String, Row] = resultVector.map { row =>
      val key = row.getString("nodeid").trim +
        row.getString("alarmtype").trim +
        row.getString("orgid").trim +
        row.getString("siteid").trim
      (key -> row)
    }.toMap

    alertMap.foreach { row =>
      try {
        val alertId = row._2.getString("alertid")
        val orgId = row._2.getString("orgid")
        val siteId = row._2.getString("siteid")
        val nodeId = processString(row._2.getString("nodeid"))
        val name = processString(row._2.getString("name"))
        val alarmType = processString(row._2.getString("alarmtype"))
        val msg = processString(row._2.getString("msg"))
        val active = processBoolean(row._2.getBool("active"))
        val category = processString(row._2.getString("category"))
        val severity = processString(row._2.getString("severity"))
        val orgName = processString(row._2.getString("orgname"))
        val siteName = processString(row._2.getString("sitename"))
        val nodeName = processString(row._2.getString("nodename"))
        val nodeHw = processString(row._2.getString("nodehw"))
        val bssId = processString(row._2.getString("bssid"))
        val siteAddress = processString(row._2.getString("siteaddress"))
        val created = processString(row._2.getString("created"))
        val updated = processString(row._2.getString("updated"))

        val colNames = "(alertid, orgid, siteid, name, alarmtype, nodeid, severity, msg, category, " +
          "active, orgname, sitename, nodename, nodehw, bssId, siteaddress, created, updated)"

        val queryString = s"""
                             | INSERT INTO $keyspace.$device_alerts ${colNames} VALUES
                             | ('${alertId}','${orgId}','${siteId}', '${name}', '${alarmType}','${nodeId}', '${severity}',
                             |  '${msg}', '${category}', ${active}, '${orgName}', '${siteName}', '${nodeName}', '${nodeHw}', '${bssId}',
                             |  '${siteAddress}', '${created}', '${updated}'
                             | );
                             |
      """.stripMargin
        val queryString1 = s"""
                              | INSERT INTO $keyspace.$alert_mappings ${colNames} VALUES
                              | ('${alertId}','${orgId}','${siteId}', '${name}', '${alarmType}','${nodeId}', '${severity}',
                              |  '${msg}', '${category}', ${active}, '${orgName}', '${siteName}', '${nodeName}', '${nodeHw}', '${bssId}',
                              |  '${siteAddress}', '${created}', '${updated}'
                              | );
                              |
      """.stripMargin

        log.debug(s"Device Alerts Cassandra Query: $queryString")
        log.debug(s"Alerts By Node Id Cassandra Query: $queryString1")

        session.execute(queryString)
        session.execute(queryString1)
        alertCassandraCounter = alertCassandraCounter + 1
      } catch {
        case e: Exception =>
          e.printStackTrace()
          log.error(s"Exception while reading alerts from Cassandra: $e")
          alertCassandraFailureCounter = alertCassandraFailureCounter + 1
      }
    }
    printCassandraAlertsCounters
  }

  def printCassandraAlertsCounters = {
    log.info(s"Cassandra to Cassandra Alerts Success Count: $alertCassandraCounter . " +
      s"Cassandra to Cassandra Alerts Failure Count: $alertCassandraFailureCounter")
  }

  override def migrate(): Unit = {
    migrateCassandraAlerts()
  }
}
