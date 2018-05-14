package com.vz.nsp.parking.config

object CassandraConfig {

  lazy val cassandraConfig = ServiceConfig.config.getConfig("cassandra")

  lazy val parkingTagTableName = cassandraConfig.getString("parking-tag-table")
  require(!parkingTagTableName.isEmpty, "parking-tag-table is missing in config")

  lazy val parkingUserDataTableName = cassandraConfig.getString("parking-userdata-table")
  require(!parkingUserDataTableName.isEmpty, "parking-userdata-table is missing in config")

  lazy val parkingPolicyTableName = cassandraConfig.getString("parking-policy-table")
  require(!parkingPolicyTableName.isEmpty, "parking-policy-table is missing in config")

  lazy val parkingGroupPolicyTableName = cassandraConfig.getString("parking-group-policy-table")
  require(!parkingGroupPolicyTableName.isEmpty, "parking-group-policy-table is missing in config")

  lazy val parkingSpaceTableName = cassandraConfig.getString("parking-space-table")
  require(!parkingSpaceTableName.isEmpty, "parking-spot-table is missing in config")

  lazy val whatIfTagTableName = cassandraConfig.getString("whatif-parking-tag-table")
  require(!whatIfTagTableName.isEmpty, "whatif-parking-tag-table is missing in config")

  lazy val whatIfPolicyTableName = cassandraConfig.getString("whatif-parking-policy-table")
  require(!whatIfPolicyTableName.isEmpty, "whatif-parking-policy-table is missing in config")

  lazy val whatIfTableName = cassandraConfig.getString("parking-Whatif-job-table")
  require(!whatIfTableName.isEmpty, "what_if_analysis_job is missing in config")



}
