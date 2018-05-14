package com.verizon.netsense.helper

import java.util.UUID

import com.datastax.driver.core.ResultSet
import com.datastax.driver.core.utils.UUIDs
import com.verizon.netsense.database._
import com.verizon.netsense.database.CassandraConnector.testConnector
import com.verizon.netsense.entity._
import com.verizon.netsense.graph.JobGraphData.DataFlow
import org.cassandraunit.utils.EmbeddedCassandraServerHelper

import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor}
import scala.concurrent.duration._

object DBHelper {

  implicit val ec = ExecutionContext.Implicits.global
  val waiting = 5.seconds

  def initDBs(): Unit = {
    EmbeddedCassandraServerHelper.startEmbeddedCassandra(20000)

    OTAJobStatusDB(testConnector).create(10.seconds)(ec.asInstanceOf[ExecutionContextExecutor])
    OTANodeStatusDB(testConnector).create(10.seconds)(ec.asInstanceOf[ExecutionContextExecutor])
    OTAJobInfoDB(testConnector).create(10.seconds)(ec.asInstanceOf[ExecutionContextExecutor])
    OTAJobRelationDB(testConnector).create(10.seconds)(ec.asInstanceOf[ExecutionContextExecutor])
  }

  def cleanupDBs(): Unit = {
    EmbeddedCassandraServerHelper.cleanEmbeddedCassandra()
  }

  def insertJobInfo(jobID: String, firmwareID: String, count: Int): ResultSet = {
    val future = OTAJobInfoDB(testConnector).model.store(OTAJobInfo(
      jobid = jobID,
      firmwareid = firmwareID,
      targetid = UUID.randomUUID().toString,
      targettype = "site",
      count = count,
      description = "desc",
      when = UUIDs.timeBased()
    ))

    Await.result(future, waiting)
  }

  def getJobInfo(jobID: String): List[OTAJobInfo]= {
    val future = OTAJobInfoDB(testConnector).model.get(jobID)

    Await.result(future, waiting)
  }

  def getNodeStatus(jobID: String): List[OTANodeStatus] = {
    val future = OTANodeStatusDB(testConnector).model.get(jobID)

    Await.result(future, waiting)
  }

  def insertNodeStatus(status: OTANodeStatus): ResultSet = {
    val future = OTANodeStatusDB(testConnector).model.store(status)

    Await.result(future, waiting)
  }

  def getJobStatus(jobID: String): List[OTAJobStatus] = {
    val future = OTAJobStatusDB(testConnector).model.get(jobID)

    Await.result(future, waiting)
  }

  def insertJobStatus(jobID: String, status: String): ResultSet = {
    val future = OTAJobStatusDB(testConnector).model.store(OTAJobStatus(
      jobid = jobID,
      status = status,
      when = UUIDs.timeBased()
    ))

    Await.result(future, waiting)
  }

  def insertJobRelation(relation: OTAJobRelation): ResultSet = {
    val future = OTAJobRelationDB(testConnector).model.store(relation)

    Await.result(future, waiting)
  }

  def createDataFlowsFromState(states: List[String]): List[DataFlow] = {
    states.map(state => {
      val job: Job = Job(
        jobid = UUID.randomUUID().toString,
        nodeid = "N001",
        model = "unode-v7",
        firmwareid = "F001"
      )

      insertJobStatus(job.jobid, state)

      DataFlow(
        job = job,
        error = None,
        firmware = None,
        isTerminated = false
      )
    })
  }
}