package com.verizon.netsense.database

import com.outworkers.phantom.connectors.CassandraConnection
import com.outworkers.phantom.dsl._
import com.verizon.netsense.connector.CassandraConnector
import com.verizon.netsense.model.Light

/**
  * Created by maidapr on 1/10/18.
  */
class MicroServicesDb (val connection: CassandraConnection) extends Database[MicroServicesDb](connection) {

  object lightTable extends ConcreteLightTable with connector.Connector
}

trait MyDbProvider extends DatabaseProvider[MicroServicesDb] {
  def database: MicroServicesDb
}

object ProductionDb extends MicroServicesDb(CassandraConnector.connector)

trait ProductionDatabase extends MyDbProvider {
  override val database = ProductionDb
}

object EmbeddedDb extends MicroServicesDb(CassandraConnector.testConnector)

trait EmbeddedDatabase extends MyDbProvider {
  override val database = EmbeddedDb
}

