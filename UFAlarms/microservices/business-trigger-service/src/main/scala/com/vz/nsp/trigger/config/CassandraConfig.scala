package com.vz.nsp.trigger.config

object CassandraConfig {

  lazy val cassandraConfig = ServiceConfig.config.getConfig("cassandra")

  lazy val TriggerTableName = cassandraConfig.getString("trigger-table")
  require(!TriggerTableName.isEmpty, "trigger-table is missing in config")


}
