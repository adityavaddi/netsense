package com.vz.nsp.trigger.db

/**
  * Created by maleva on 2/20/18.
  */


import com.outworkers.phantom.dsl._
import com.outworkers.phantom.dsl.DatabaseProvider

import com.verizon.netsense.connector.CassandraConnector


class MicroServicesDb(override val connector: KeySpaceDef) extends Database[MicroServicesDb](connector) {
  object  triggerServiceTableTest extends ConcreteTrigger with connector.Connector
}

trait MyDbProvider extends DatabaseProvider[MicroServicesDb] {
  def database: MicroServicesDb
}

object EmbeddedDb extends MicroServicesDb(CassandraConnector.testConnector)

trait EmbeddedDatabase extends MyDbProvider {
  override val database = EmbeddedDb
}