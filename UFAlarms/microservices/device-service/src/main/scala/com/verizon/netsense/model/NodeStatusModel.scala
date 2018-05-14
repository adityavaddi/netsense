package com.verizon.netsense.model

import com.outworkers.phantom.CassandraTable
import com.outworkers.phantom.connectors.RootConnector
import com.outworkers.phantom.dsl._
import com.verizon.netsense.entity._

import scala.concurrent.Future

/**
 * Created by davor on 7/14/17.
 */
class NodeStatusModel extends CassandraTable[ConcreteNodeStatusModel, NodeStatus] {
  override def tableName: String = "node_status"

  object nodeid extends StringColumn(this) with PartitionKey

  object orgid    extends StringColumn(this)
  object siteid   extends StringColumn(this)
  object net_stat extends IntColumn(this)
  object lig_stat extends StringColumn(this)
  object sen_stat extends StringColumn(this)

  override def fromRow(r: Row): NodeStatus =
    NodeStatus("NodeStatus", nodeid(r), orgid(r), siteid(r), net_stat(r), lig_stat(r), sen_stat(r))
}

abstract class ConcreteNodeStatusModel extends NodeStatusModel with RootConnector {

  def getNodeStatusByNodeid(nodeid: String): Future[Option[NodeStatus]] =
    select
      .where(_.nodeid eqs nodeid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .one()

  def storeNodeStatus(nodeId: String,
                      orgId: String,
                      siteId: String,
                      netStat: Int,
                      ligStat: String,
                      senStat: String): Future[ResultSet] =
    if (orgId.isEmpty)
      insert
      .value(_.nodeid, nodeId)
//      .value(_.orgid, orgId)
//      .value(_.siteid, siteId)
      .value(_.net_stat, netStat)
      .value(_.lig_stat, ligStat)
      .value(_.sen_stat, senStat)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()
    else
      insert
        .value(_.nodeid, nodeId)
        .value(_.orgid, orgId)
        .value(_.siteid, siteId)
        .value(_.net_stat, netStat)
        .value(_.lig_stat, ligStat)
        .value(_.sen_stat, senStat)
        .consistencyLevel_=(ConsistencyLevel.ONE)
        .future()

}
