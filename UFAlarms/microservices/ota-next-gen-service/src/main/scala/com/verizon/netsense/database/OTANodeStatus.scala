package com.verizon.netsense.database

import com.outworkers.phantom.CassandraTable
import com.outworkers.phantom.connectors.CassandraConnection
import com.outworkers.phantom.dsl._
import com.verizon.netsense.entity.OTANodeStatus
import com.verizon.netsense.metrics.Instrumented

import scala.concurrent.Future

case class OTANodeStatusDB(override val connector: CassandraConnection)
  extends Database[OTANodeStatusDB](connector) {

  object model extends OTANodeStatusModel with Connector

}

trait OTANodeStatusModel
  extends CassandraTable[OTANodeStatusModel, OTANodeStatus]
    with RootConnector
    with Instrumented {

  override def tableName: String = "ota_node_status"

  val selectTimer = metrics.timer("select")
  val insertTimer = metrics.timer("insert")

  object jobid extends StringColumn(this) with PartitionKey

  object nodeid extends StringColumn(this)

  object status extends StringColumn(this)

  object progress extends LongColumn(this)

  object when extends TimeUUIDColumn(this) with ClusteringOrder with Ascending

  def store(nodeStatus: OTANodeStatus): Future[ResultSet] = {
    selectTimer.timeFuture {
      insert
        .value(_.jobid, nodeStatus.jobid)
        .value(_.nodeid, nodeStatus.nodeid)
        .value(_.status, nodeStatus.status)
        .value(_.progress, nodeStatus.progress)
        .value(_.when, nodeStatus.when)
        .future()
    }
  }

  def get(jobid: String): Future[List[OTANodeStatus]] = {
    insertTimer.timeFuture {
      select
        .where(_.jobid eqs jobid)
        .consistencyLevel_=(ConsistencyLevel.ONE)
        .fetch()
    }
  }
}
