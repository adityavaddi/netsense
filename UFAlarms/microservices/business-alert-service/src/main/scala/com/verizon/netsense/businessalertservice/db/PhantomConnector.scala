package com.verizon.netsense.businessalertservice.db

import com.outworkers.phantom.dsl._
import com.verizon.netsense.connector.CassandraConnector

class BusinessAlertServicesDb(override val connector: KeySpaceDef)
    extends Database[BusinessAlertServicesDb](connector) {

  object BusinessAlertTable extends ConcreteBusinessAlert with connector.Connector

  object BusinessTriggerTable extends ConcreteBusinessTrigger with connector.Connector

  object BusinessAlertHistoryTable extends ConcreteBusinessAlertHistory with connector.Connector

}

trait BusinessAlertProvider extends DatabaseProvider[BusinessAlertServicesDb] {
  def database: BusinessAlertServicesDb
}

object ProductionDb extends BusinessAlertServicesDb(CassandraConnector.connector)

trait ProductionDatabase extends BusinessAlertProvider {
  override val database = ProductionDb
}

object EmbeddedDb extends BusinessAlertServicesDb(CassandraConnector.testConnector)

trait EmbeddedDatabase extends BusinessAlertProvider {
  override val database = EmbeddedDb
}
