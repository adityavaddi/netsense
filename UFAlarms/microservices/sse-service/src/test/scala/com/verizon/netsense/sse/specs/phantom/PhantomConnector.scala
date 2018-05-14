package com.verizon.netsense.sse.specs.phantom

import com.outworkers.phantom.database.Database
import com.outworkers.phantom.dsl._
import com.verizon.netsense.sse.db.model.{ConcreteBusinessAlertFilterEvent, ConcreteFilterEvent, ConcreteOrgByNode}

/**
 * Created by muppasw on 9/19/17.
 */
class PhantomConnector(override val connector: KeySpaceDef) extends Database[PhantomConnector](connector) {
  object orgtabletest                    extends ConcreteOrgByNode with connector.Connector
  object ssefiltertest                   extends ConcreteFilterEvent with connector.Connector
  object sseBusinessAlertFilterEventtest extends ConcreteBusinessAlertFilterEvent with connector.Connector
}

trait MyDbProvider {
  def databaseTest: PhantomConnector
}
