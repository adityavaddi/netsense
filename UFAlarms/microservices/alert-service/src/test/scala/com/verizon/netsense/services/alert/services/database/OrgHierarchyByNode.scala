package com.verizon.netsense.services.alert.services.database

import com.outworkers.phantom.dsl._
import com.verizon.netsense.services.alert.model.OrgHierarchy

import scala.concurrent.Future

abstract class OrgHierarchyByNode extends Table[OrgHierarchyByNode, OrgHierarchy] with RootConnector {

  def truncateTable(): Future[ResultSet] = truncate().future()

  def createTable() = create.ifNotExists().future()

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

  override def tableName: String = "orghierarchy_by_nodeid"

  object nodeId extends StringColumn with PartitionKey

  object nodeName extends OptionalStringColumn

  object orgId extends OptionalStringColumn

  object orgName extends OptionalStringColumn

  object siteId extends OptionalStringColumn

  object siteName extends OptionalStringColumn

  object siteAddress extends OptionalStringColumn

  object bssId extends OptionalStringColumn

  object nodeHw extends OptionalStringColumn
}
