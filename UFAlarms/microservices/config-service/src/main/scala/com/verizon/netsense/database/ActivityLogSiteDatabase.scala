package com.verizon.netsense.database

import com.outworkers.phantom.connectors.CassandraConnection
import com.verizon.netsense.connector.CassandraConnector._
import com.verizon.netsense.model.{ConcreteActivityLogSiteModel}

class ActivityLogSiteDatabase(override val connector: CassandraConnection)
  extends com.outworkers.phantom.database.Database[ActivityLogSiteDatabase](connector) {

  object activityLogSiteModel extends ConcreteActivityLogSiteModel with connector.Connector

}

object ActivityLogSiteProductionDb extends ActivityLogSiteDatabase(connector)

trait ActivityLogSiteProductionDatabaseProvider {

  def database: ActivityLogSiteDatabase
}

trait ActivityLogSiteProductionDatabase extends ActivityLogSiteProductionDatabaseProvider {

  override val database = ActivityLogSiteProductionDb
}

object ActivityLogSiteEmbeddedDb extends ActivityLogSiteDatabase(testConnector)

trait ActivityLogSiteEmbeddedDatabaseProvider {
  def database: ActivityLogSiteDatabase
}

trait ActivityLogSiteEmbeddedDatabase extends ActivityLogSiteEmbeddedDatabaseProvider {
  override val database = ActivityLogSiteEmbeddedDb
}
