package com.verizon.netsense.database

import com.outworkers.phantom.dsl.{ResultSet, UUID}
import com.verizon.netsense.entity.{DeviceConnectionStatus, NodeStatus}
import com.verizon.netsense.metrics.Instrumented
import nl.grons.metrics.scala.Timer

import scala.concurrent._
import scala.concurrent.duration._

/**
 * Created by davor on 7/14/17.
 */

trait NodeStatusDbServiceBase {
  def getNodeStatusByNodeId(nodeid: String): Future[Option[NodeStatus]]

  def storeNodeStatus(nodeId: String,
                      orgId: String,
                      siteId: String,
                      netStat: Int,
                      ligStat: String,
                      senStat: String): Future[ResultSet]
}

trait NodeStatusDbService extends NodeStatusDbServiceBase with NodeStatusProductionDatabase with Instrumented {

  private[this] val cassandraGetNodeStatusTimer: Timer = metrics.timer("cassandra-get-nodestatus-timer")
  private[this] val cassandraStoreNodeStatusTimer: Timer = metrics.timer("cassandra-store-nodestatus-timer")

  def start(): Unit = {
    val ec = scala.concurrent.ExecutionContext.Implicits.global

    database.create(3.seconds)(ec.asInstanceOf[ExecutionContextExecutor])
  }

  def getNodeStatusByNodeId(nodeid: String): Future[Option[NodeStatus]] =
    cassandraGetNodeStatusTimer.time {
      database.nodeStatusModel.getNodeStatusByNodeid(nodeid)
    }

  def storeNodeStatus(nodeId: String,
                      orgId: String,
                      siteId: String,
                      netStat: Int,
                      ligStat: String,
                      senStat: String): Future[ResultSet] =
    cassandraStoreNodeStatusTimer.time {
      database.nodeStatusModel.storeNodeStatus(nodeId, orgId, siteId, netStat, ligStat, senStat)
    }

}

class NodeStatusDbServiceImpl extends NodeStatusDbService with NodeStatusProductionDatabase

object NodeStatusDbService extends NodeStatusDbService with NodeStatusProductionDatabase
