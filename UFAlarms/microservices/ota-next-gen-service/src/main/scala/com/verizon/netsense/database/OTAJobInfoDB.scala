package com.verizon.netsense.database

import com.outworkers.phantom.CassandraTable
import com.outworkers.phantom.connectors.CassandraConnection
import com.outworkers.phantom.dsl._
import com.verizon.netsense.entity.OTAJobInfo
import com.verizon.netsense.metrics.Instrumented

import scala.concurrent.Future

case class OTAJobInfoDB(override val connector: CassandraConnection)
  extends Database[OTAJobInfoDB](connector) {

  object model extends OTAJobInfoModel with Connector

}

trait OTAJobInfoModel
  extends CassandraTable[OTAJobInfoModel, OTAJobInfo]
    with RootConnector
    with Instrumented {

  override def tableName: String = "ota_job_info"

  val selectTimer = metrics.timer("select")
  val insertTimer = metrics.timer("insert")
  val updateTimer = metrics.timer("update")

  object jobid extends StringColumn(this) with PartitionKey

  object firmwareid extends StringColumn(this)

  object targetid extends StringColumn(this)

  object targettype extends StringColumn(this)

  object count extends IntColumn(this)

  object description extends StringColumn(this)

  object when extends TimeUUIDColumn(this) with ClusteringOrder with Ascending

  def get(jobid: String): Future[List[OTAJobInfo]] = {
    selectTimer.timeFuture {
      select
        .where(_.jobid eqs jobid)
        .consistencyLevel_=(ConsistencyLevel.ONE)
        .fetch()
    }
  }

  def store(jobInfo: OTAJobInfo): Future[ResultSet] = {
    insertTimer.timeFuture {
      insert
        .value(_.jobid, jobInfo.jobid)
        .value(_.firmwareid, jobInfo.firmwareid)
        .value(_.targetid, jobInfo.targetid)
        .value(_.targettype, jobInfo.targettype)
        .value(_.count, jobInfo.count)
        .value(_.description, jobInfo.description)
        .value(_.when, jobInfo.when)
        .future()
    }
  }

  def updateCount(jobid: String, when: UUID, count: Int): Future[ResultSet] = {
    updateTimer.timeFuture {
      update()
        .where(_.jobid eqs jobid)
        .and(_.when eqs when)
        .modify(_.count setTo count)
        .future()
    }
  }

}