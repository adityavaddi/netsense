package com.verizon.netsense.database

import com.outworkers.phantom.CassandraTable
import com.outworkers.phantom.connectors.CassandraConnection
import com.outworkers.phantom.dsl._
import com.verizon.netsense.entity.OTAJobRelation
import com.verizon.netsense.metrics.Instrumented

import scala.concurrent.Future

case class OTAJobRelationDB(override val connector: CassandraConnection)
  extends Database[OTAJobRelationDB](connector) {

  object model extends OTAJobRelationModel with Connector

}

trait OTAJobRelationModel
  extends CassandraTable[OTAJobRelationModel, OTAJobRelation]
    with RootConnector
    with Instrumented {

  override def tableName: String = "ota_job_relation"

  val selectTimer = metrics.timer("select")
  val insertTimer = metrics.timer("insert")

  object orgid extends StringColumn(this) with PartitionKey

  object siteid extends StringColumn(this) with PartitionKey

  object jobid extends StringColumn(this)

  object when extends TimeUUIDColumn(this) with ClusteringOrder with Ascending

  def get(orgid: String, siteid: String): Future[List[OTAJobRelation]] =
    selectTimer.timeFuture {
      select
        .where(_.orgid eqs orgid)
        .and(_.siteid eqs siteid)
        .consistencyLevel_=(ConsistencyLevel.ONE)
        .fetch()
    }

  def store(jobRelation: OTAJobRelation): Future[ResultSet] = {
    insertTimer.timeFuture {
      insert
        .value(_.orgid, jobRelation.orgid)
        .value(_.siteid, jobRelation.siteid)
        .value(_.jobid, jobRelation.jobid)
        .value(_.when, jobRelation.when)
        .future()
    }
  }

}