package com.vz.nsp.parking.db

import com.outworkers.phantom.dsl._
import com.verizon.netsense.connector.CassandraConnector

class parkingServicesDb(override val connector: KeySpaceDef) extends Database[parkingServicesDb](connector) {

  object TagTable                    extends ConcreteTag with connector.Connector
  object policyMappingTable          extends ConcretePolicy with connector.Connector
  object parkingGroupPolicyLinkTable extends ConcreteGroupPolicy with connector.Connector
  object ParkingSpaceTable           extends ConcreteParkingSpace with connector.Connector
  object UserDataTable               extends ConcreteUserData with connector.Connector
  object WhatIfParkingTagTable            extends ConcreteWhatIfParkingTag with connector.Connector
  object WhatIfPolicyTable                extends ConcreteWhatIfPolicy with connector.Connector
  object WhatIfJobTable               extends ConcreteWhatIfJob with connector.Connector

}

trait MyDbProvider extends DatabaseProvider[parkingServicesDb] {
  def database: parkingServicesDb
}

object ProductionDb extends parkingServicesDb(CassandraConnector.connector)

trait ProductionDatabase extends MyDbProvider {
  override val database = ProductionDb
}

object EmbeddedDb extends parkingServicesDb(CassandraConnector.testConnector)

trait EmbeddedDatabase extends MyDbProvider {
  override val database = EmbeddedDb
}
