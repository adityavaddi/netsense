package com.verizon.netsense.services.eventsimulator.database

import com.outworkers.phantom.dsl._
import com.verizon.netsense.services.eventsimulator.model.OrgHierarchy

import scala.concurrent.Future

abstract class Orghierarchy_By_Nodeid extends CassandraTable[Orghierarchy_By_Nodeid, OrgHierarchy] with RootConnector {

  def truncateTable(): Future[ResultSet] =
    truncate().future()

  def createTable() = create.ifNotExists().future();

  def store(org: OrgHierarchy): Future[ResultSet] =
    insert
      .value(_.nodeId, org.nodeId)
      .value(_.nodeName, org.nodeName)
      .value(_.orgId, org.orgId)
      .value(_.orgName, org.orgName)
      .value(_.siteId, org.siteId)
      .value(_.siteName, org.siteName)
      .value(_.siteAddress, org.siteAddress)
      .value(_.bssId, org.bssId)
      .value(_.nodeHw, org.nodeHw)
      .future()

  object nodeId extends StringColumn(this) with PartitionKey

  object nodeName extends OptionalStringColumn(this)

  object orgId extends StringColumn(this)

  object orgName extends OptionalStringColumn(this)

  object siteId extends StringColumn(this)

  object siteName extends OptionalStringColumn(this)

  object siteAddress extends OptionalStringColumn(this)

  object bssId extends OptionalStringColumn(this)

  object nodeHw extends OptionalStringColumn(this)
}
