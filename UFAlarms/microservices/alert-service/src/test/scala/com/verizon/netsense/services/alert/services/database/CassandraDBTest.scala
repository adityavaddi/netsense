package com.verizon.netsense.services.alert.services.database

import com.outworkers.phantom.connectors.CassandraConnection
import com.outworkers.phantom.dsl._
import com.verizon.netsense.connector.CassandraConnector
import com.verizon.netsense.utils.ConfigLoader

class CassandraDBTest(val connection: CassandraConnection) extends Database[CassandraDBTest](connection) {

  val cassandraTableConfig = ConfigLoader.config.getConfig("cassandra-tables")
  val alarms               = cassandraTableConfig.getString("alarms-table")
  val alerts               = cassandraTableConfig.getString("alerts-table")
  val alertsByNodeId       = cassandraTableConfig.getString("alerts-by-nodeid-table")
  val ufAlarms             = cassandraTableConfig.getString("uf-alarms-table")
  val ufAlarmsById         = cassandraTableConfig.getString("uf-alarms-by-id-table")
  val notifications        = cassandraTableConfig.getString("notifications-table")

  val READ_CONSISTENCY_LEVEL  = ConsistencyLevel.ONE
  val WRITE_CONSISTENCY_LEVEL = ConsistencyLevel.ONE

  object DeviceAlerts       extends Alerts with Connector
  object DeviceAlarms       extends Alarms with Connector
  object Notifications      extends Notifications with Connector
  object OrgHierarchyByNode extends OrgHierarchyByNode with Connector
  object AlertsByNode       extends AlertsByNode with Connector
  object UFAlarms           extends UFAlarms with Connector
  object UFAlarmsById       extends UFAlarmsById with Connector

}

object CassandraDBTest extends CassandraDBTest(CassandraConnector.connector)
