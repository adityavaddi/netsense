package com.verizon.netsense.services.mock

import com.outworkers.phantom.dsl.ResultSet
import com.verizon.netsense.database.NodeStatusDbServiceBase
import com.verizon.netsense.entity.NodeStatus

import scala.concurrent.Future

class CassandraDatabaseMock extends NodeStatusDbServiceBase {
  override def getNodeStatusByNodeId(nodeid: String): Future[Option[NodeStatus]] = {
    Future.successful(Some(NodeStatus("", "", "", "", 0, "", "")))
  }

  override def storeNodeStatus(nodeId: String,
                               orgId: String,
                               siteId: String,
                               netStat: Int,
                               ligStat: String,
                               senStat: String) : Future[ResultSet] = {
    Future.failed(new Exception("Temporary"))
  }
}
