package com.verizon.netsense.database

import com.outworkers.phantom.connectors.CassandraConnection
import com.verizon.netsense.connector.CassandraConnector._
import com.verizon.netsense.model.{ConcreteActivityLogUserModel}

class ActivityLogUserDatabase(override val connector: CassandraConnection)
  extends com.outworkers.phantom.database.Database[ActivityLogUserDatabase](connector) {

  object activityLogUserModel extends ConcreteActivityLogUserModel with connector.Connector

}

object ActivityLogUserProductionDb extends ActivityLogUserDatabase(connector)

trait ActivityLogUserProductionDatabaseProvider {

  def database: ActivityLogUserDatabase
}

trait ActivityLogUserProductionDatabase extends ActivityLogUserProductionDatabaseProvider {

  override val database = ActivityLogUserProductionDb
}

object ActivityLogUserEmbeddedDb extends ActivityLogUserDatabase(testConnector)

trait ActivityLogUserEmbeddedDatabaseProvider {
  def database: ActivityLogUserDatabase
}

trait ActivityLogUserEmbeddedDatabase extends ActivityLogUserEmbeddedDatabaseProvider {
  override val database = ActivityLogUserEmbeddedDb
}
