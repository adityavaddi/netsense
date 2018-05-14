package com.verizon.netsense.businessalertservice.config

import com.typesafe.config.ConfigFactory

object CassandraConfig {
  lazy val config = ConfigFactory.load()

  lazy val cassandraConfig = config.getConfig("cassandra")

  lazy val businessAlertTable = cassandraConfig.getString("business-alert-table")
  lazy val businessTriggerTable = cassandraConfig.getString("business-trigger-table")
  lazy val businessAlertHistoryTable = cassandraConfig.getString("business-alert-history-table")
  require(!businessAlertTable.isEmpty, "business-alert table is missing in config")
  require(!businessTriggerTable.isEmpty, "business-trigger table is missing in config")
  require(!businessAlertHistoryTable.isEmpty, "business-alert-history table is missing in config")

}
