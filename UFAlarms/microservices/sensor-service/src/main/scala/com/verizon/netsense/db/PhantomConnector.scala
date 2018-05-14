package com.verizon.netsense.db

import com.outworkers.phantom.dsl._
import com.verizon.netsense.connector.CassandraConnector

class MicroServicesDb(override val connector: KeySpaceDef) extends Database[MicroServicesDb](connector) {

  object deviceSensorSampleTable extends ConcreteDeviceEvent with connector.Connector

  object energySavingsNodeTable extends ConcreteEnergySavingsNode with connector.Connector

  object energySavingSiteTable extends ConcreteEnergySavingsSite with connector.Connector

  object lightTable extends ConcreteLightTable with connector.Connector

  object nodeStatusTable extends ConcreteNodeStatusModel with connector.Connector

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
