package com.verizon.netsense.services.gps.db

import com.outworkers.phantom.dsl._
import com.verizon.netsense.services.gps.model.OrgHierarchy

import scala.concurrent.Future

abstract class Orghierarchy_By_Nodeid extends CassandraTable[Orghierarchy_By_Nodeid, OrgHierarchy] with RootConnector {

  def getById(nodeId: String): Future[Option[OrgHierarchy]] =
    select
      .where(_.nodeId eqs nodeId)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .one()

  def lookupOrgByOrgAndSite(orgId: String, siteId: String): Future[Option[OrgHierarchy]] =
    select
      .where(_.orgId is orgId)
      .and(_.siteId is siteId)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .one()

  def storeHierArchy(orgHierarchy: OrgHierarchy): Future[ResultSet] = {
    insert.value(_.nodeId,orgHierarchy.nodeId)
      .value(_.nodeName,orgHierarchy.nodeName)
      .value(_.orgId,orgHierarchy.orgId)
      .value(_.orgName,orgHierarchy.orgName)
      .value(_.siteId,orgHierarchy.siteId)
      .value(_.siteName,orgHierarchy.siteName)
      .value(_.siteAddress,orgHierarchy.siteAddress)
      .value(_.bssId,orgHierarchy.bssId)
      .value(_.nodeHw,orgHierarchy.nodeHw)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()
  }

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
