package com.verizon.netsense.services.alert.database

import com.outworkers.phantom.dsl._
import com.verizon.netsense.services.alert.database.CassandraDB.READ_CONSISTENCY_LEVEL
import com.verizon.netsense.services.alert.model.OrgHierarchy

import scala.concurrent.Future

abstract class OrgHierarchyByNode extends Table[OrgHierarchyByNode, OrgHierarchy] with RootConnector {

  def getById(nodeId: String): Future[Option[OrgHierarchy]] =
    select
      .where(_.nodeId eqs nodeId)
      .consistencyLevel_=(READ_CONSISTENCY_LEVEL)
      .one()

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
