package com.vz.nsp.trigger.db

import com.outworkers.phantom.dsl._
import com.verizon.netsense.connector.CassandraConnector

class TriggerServicesDb(override val connector: KeySpaceDef) extends Database[TriggerServicesDb](connector) {

  object TriggerTable extends ConcreteTrigger with connector.Connector
}

trait MyDbProvider extends DatabaseProvider[TriggerServicesDb] {
  def database: TriggerServicesDb
}

object ProductionDb extends TriggerServicesDb(CassandraConnector.connector)

trait ProductionDatabase extends MyDbProvider {
  override val database = ProductionDb
}

