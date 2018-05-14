package com.verizon.netsense.database

import com.outworkers.phantom.connectors.CassandraConnection
import com.outworkers.phantom.database.Database
import com.verizon.netsense.connector.CassandraConnector._
import com.verizon.netsense.model.{ConcreteNodeStatusModel}

/**
 * Created by davor on 7/14/17.
 */
class NodeStatusDatabase(override val connector: CassandraConnection) extends Database[NodeStatusDatabase](connector) {

  object nodeStatusModel extends ConcreteNodeStatusModel with connector.Connector

}

object NodeStatusProductionDb extends NodeStatusDatabase(connector)

trait NodeStatusProductionDatabaseProvider {

  def database: NodeStatusDatabase
}

trait NodeStatusProductionDatabase extends NodeStatusProductionDatabaseProvider {

  override val database = NodeStatusProductionDb
}

object NodeStatusEmbeddedDb extends NodeStatusDatabase(testConnector)

trait NodeStatusEmbeddedDatabaseProvider {
  def database: NodeStatusDatabase
}

trait NodeStatusEmbeddedDatabase extends NodeStatusEmbeddedDatabaseProvider {
  override val database = NodeStatusEmbeddedDb
}
