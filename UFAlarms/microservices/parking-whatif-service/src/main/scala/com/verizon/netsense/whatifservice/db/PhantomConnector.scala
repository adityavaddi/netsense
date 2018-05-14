package com.verizon.netsense.whatifservice.db

import com.outworkers.phantom.dsl._
import com.verizon.netsense.connector.CassandraConnector

/**
 * Created by maleva on 3/17/18.
 */
class WhatIfServicesDb(override val connector: KeySpaceDef) extends Database[WhatIfServicesDb](connector) {
  object WhatIfJobTable     extends ConcreteWhatIfJob with connector.Connector
  object policyMappingTable extends ConcretePolicy with connector.Connector

}

trait MyDbProvider extends DatabaseProvider[WhatIfServicesDb] {
  def database: WhatIfServicesDb
}

object ProductionDb extends WhatIfServicesDb(CassandraConnector.connector)

trait ProductionDatabase extends MyDbProvider {
  override val database = ProductionDb
}

object EmbeddedDb extends WhatIfServicesDb(CassandraConnector.testConnector)

trait EmbeddedDatabase extends MyDbProvider {
  override val database = EmbeddedDb
}
