package com.verizon.netsense.database

import com.outworkers.phantom.connectors.CassandraConnection
import com.verizon.netsense.connector.CassandraConnector._
import com.verizon.netsense.model.ConcreteConfigModel

class ConfigDatabase(override val connector: CassandraConnection)
    extends com.outworkers.phantom.database.Database[ConfigDatabase](connector) {

  object configModel extends ConcreteConfigModel with connector.Connector

}

object ConfigProductionDb extends ConfigDatabase(connector)

trait ConfigProductionDatabaseProvider {

  def database: ConfigDatabase
}

trait ConfigProductionDatabase extends ConfigProductionDatabaseProvider {

  override val database = ConfigProductionDb
}

object ConfigEmbeddedDb extends ConfigDatabase(testConnector)

trait ConfigEmbeddedDatabaseProvider {
  def database: ConfigDatabase
}

trait ConfigEmbeddedDatabase extends ConfigEmbeddedDatabaseProvider {
  override val database = ConfigEmbeddedDb
}
