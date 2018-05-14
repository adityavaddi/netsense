package com.verizon.netsense.sse.simulator.database

import com.outworkers.phantom.dsl._

/**
 *
 * Created by subrsi9 on 11/24/17.
 */
class SSESimulatorDatabase(val keyspace: KeySpaceDef) extends Database[SSESimulatorDatabase](keyspace) {
  object orgByNode              extends ConcreteOrgByNode with keyspace.Connector
  object sseFilterEvent         extends ConcreteFilterEvent with keyspace.Connector
  object sseBusinessFilterEvent extends ConcreteBusinessAlertFilterEvent with keyspace.Connector
}

trait SSESimulatorDBProvider {
  def database: SSESimulatorDatabase
}
