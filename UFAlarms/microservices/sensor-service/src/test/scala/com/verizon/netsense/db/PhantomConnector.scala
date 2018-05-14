package com.vz.nsp.datasample.service.db

import com.outworkers.phantom.dsl._
import com.verizon.netsense.connector.CassandraConnector
import com.verizon.netsense.db.ConcreteDeviceEvent

class MicroServicesDb(override val connector: KeySpaceDef) extends Database[MicroServicesDb](connector) {

  object deviceSensorSampleTableTest extends ConcreteDeviceEvent with connector.Connector

  object energySavingsNodeTableTest extends ConcreteEnergySavingsNodeTest with connector.Connector

  object energySavingSiteTableTest extends ConcreteEnergySavingsSiteTest with connector.Connector

}

trait MyDbProvider extends DatabaseProvider[MicroServicesDb] {
  def database: MicroServicesDb
}

object EmbeddedDb extends MicroServicesDb(CassandraConnector.testConnector)

trait EmbeddedDatabase extends MyDbProvider {
  override val database = EmbeddedDb
}
