package com.verizon.netsense.sse.db.database

import com.outworkers.phantom.dsl._
import com.verizon.netsense.sse.db.model.{ConcreteBusinessAlertFilterEvent, ConcreteFilterEvent, ConcreteOrgByNode}

class SSEDatabase(val keyspace: KeySpaceDef) extends Database[SSEDatabase](keyspace) {
  object orgByNode$     extends ConcreteOrgByNode with keyspace.Connector
  object sseFilterEvent extends ConcreteFilterEvent with keyspace.Connector
  object sseBusinessAlertFilterEvent extends ConcreteBusinessAlertFilterEvent with keyspace.Connector
}

trait MicroservicesDatabaseProvider {
  def database: SSEDatabase
}
