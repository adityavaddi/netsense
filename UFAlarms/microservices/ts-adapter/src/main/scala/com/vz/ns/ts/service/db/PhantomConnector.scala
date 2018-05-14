package com.vz.ns.ts.service.db

import com.outworkers.phantom.dsl._
import com.vz.ns.ts.service.config.CassandraConnector

class TsAdapterDB(override val connector: KeySpaceDef) extends Database[TsAdapterDB](connector) {

  object deviceMappingTable extends ConcreteDeviceMapping with connector.Connector
}

object ProductionDb extends TsAdapterDB(CassandraConnector.connector)

trait MyDbProvider {
  def database: TsAdapterDB
}

trait ProductionDatabase extends MyDbProvider {
  override val database = ProductionDb
}
