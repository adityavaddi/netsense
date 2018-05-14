package com.verizon.netsense.database

import com.outworkers.phantom.CassandraTable
import com.outworkers.phantom.connectors.CassandraConnection
import com.outworkers.phantom.dsl._
import com.verizon.netsense.entity.OTAJobStatus
import com.verizon.netsense.metrics.Instrumented

import scala.concurrent.Future

case class OTAJobStatusDB(override val connector: CassandraConnection)
  extends Database[OTAJobStatusDB](connector) {

  object model extends OTAJobStatusModel with Connector

}

trait OTAJobStatusModel
  extends CassandraTable[OTAJobStatusModel, OTAJobStatus]
    with RootConnector
    with Instrumented {

  override def tableName: String = "ota_job_status"

  val selectTimer = metrics.timer("select")
  val insertTimer = metrics.timer("insert")

  object jobid extends StringColumn(this) with PartitionKey

  object status extends StringColumn(this)

  object when extends TimeUUIDColumn(this) with ClusteringOrder with Ascending

  def store(jobStatus: OTAJobStatus): Future[ResultSet] = {
    selectTimer.timeFuture {
      insert
        .value(_.jobid, jobStatus.jobid)
        .value(_.status, jobStatus.status)
        .value(_.when, jobStatus.when)
        .future()
    }
  }

  def get(jobid: String): Future[List[OTAJobStatus]] = {
    insertTimer.timeFuture {
      select
        .where(_.jobid eqs jobid)
        .consistencyLevel_=(ConsistencyLevel.ONE)
        .fetch()
    }
  }
}
