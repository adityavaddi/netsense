package com.verizon.netsense.graph

import java.util.UUID

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Sink, Source}
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import akka.testkit.TestKit
import com.verizon.netsense.database.CassandraConnector.testConnector
import com.verizon.netsense.entity._
import com.verizon.netsense.helper.DBHelper
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FunSuiteLike, MustMatchers}
import com.verizon.netsense.graph.JobActionGraphData.DataFlow

import scala.concurrent.Await
import scala.concurrent.duration._

class JobActionGraphSpec extends TestKit(ActorSystem("JobActionGraphSpec-System"))
  with FunSuiteLike
  with MustMatchers
  with BeforeAndAfterAll
  with BeforeAndAfterEach
  with Constants {

  override def beforeAll(): Unit = {
    DBHelper.initDBs()
  }

  override def afterAll(): Unit = {
    DBHelper.cleanupDBs()
  }

  implicit val materializer = ActorMaterializer(ActorMaterializerSettings(system))
  val waiting = 5.seconds

  test("CheckTopicFlow positive") {
    val flowUnderTest = JobActionGraph(testConnector).IsJobTerminatedFlow

    val jobID: String = UUID.randomUUID().toString
    val dataFlow: APIDataFlow = createAPIDataFlowFromJobID(jobID)

    DBHelper.insertJobStatus(jobID, JOB_STARTED)

    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting).head

    assert(result.data == dataFlow)
    assert(result.state == JOB_STARTED)
  }

  test("CheckTopicFlow negative") {
    val flowUnderTest = JobActionGraph(testConnector).IsJobTerminatedFlow

    val jobID: String = UUID.randomUUID().toString
    val dataFlow: APIDataFlow = createAPIDataFlowFromJobID(jobID)


    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting).head

    assert(result.data == dataFlow)
    assert(result.state == JOB_404)
  }

  test("APIRespFlow positive") {
    val flowUnderTest = JobActionGraph(testConnector).APIRespFlow

    val jobID: String = UUID.randomUUID().toString
    val dataFlow: DataFlow = DataFlow(
      data = createAPIDataFlowFromJobID(jobID),
      state = JOB_STARTED
    )

    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting).head

    assert(result.res.get.success)
  }

  test("APIRespFlow negative") {
    val flowUnderTest = JobActionGraph(testConnector).APIRespFlow

    val jobID: String = UUID.randomUUID().toString
    val dataFlows: List[DataFlow] = List(
      DataFlow(
        data = createAPIDataFlowFromJobID(jobID),
        state = JOB_STOPPED
      ),
      DataFlow(
        data = createAPIDataFlowFromJobID(jobID),
        state = JOB_DONE
      ),
      DataFlow(
        data = createAPIDataFlowFromJobID(jobID),
        state = JOB_404
      )
    )

    val future = Source(dataFlows).via(flowUnderTest).runWith(Sink.seq)
    val results = Await.result(future, waiting)

    results.foreach(result => {
      assert(result.res.get.error.isDefined)
    })
  }

  test("JobStatusDB positive") {
    val flowUnderTest = JobActionGraph(testConnector).JobStatusDB

    val jobID: String = UUID.randomUUID().toString
    val dataFlow: DataFlow = DataFlow(
      data = createAPIDataFlowFromJobID(jobID),
      state = JOB_STARTED
    )

    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    Await.result(future, waiting)

    val result = DBHelper.getJobStatus(jobID).head

    assert(result.jobid == jobID)
    assert(result.status == JOB_STOPPED)
  }

  test("JobStatusDB negative") {
    val flowUnderTest = JobActionGraph(testConnector).JobStatusDB

    val jobID: String = UUID.randomUUID().toString

    val dataFlows: List[DataFlow] = List(
      DataFlow(
        data = createAPIDataFlowFromJobID(jobID),
        state = JOB_STOPPED
      ),
      DataFlow(
        data = createAPIDataFlowFromJobID(jobID),
        state = JOB_DONE
      ),
      DataFlow(
        data = createAPIDataFlowFromJobID(jobID),
        state = JOB_404
      ),
      DataFlow(
        data = createAPIDataFlowFromJobID(jobID),
        state = JOB_TIMEOUT
      ),
      DataFlow(
        data = createAPIDataFlowFromJobID(jobID),
        state = JOB_DS_ERR
      ),
      DataFlow(
        data = createAPIDataFlowFromJobID(jobID),
        state = FW_NOT_FOUND
      )
    )

    val future = Source(dataFlows).via(flowUnderTest).runWith(Sink.seq)
    val results = Await.result(future, waiting)

    assert(results.isEmpty)

    val dbResults = DBHelper.getJobStatus(jobID)

    assert(dbResults.isEmpty)
  }

  def createAPIDataFlowFromJobID(jobID: String): APIDataFlow = {
    val req: OTAReq = OTAReq(
      `type` = "t",
      model = "",
      action = "",
      user = UUID.randomUUID().toString,
      firmwareprops = FirmwareProps(firmwareid = UUID.randomUUID().toString),
      otaprops = OTAProps(
        jobid = jobID,
        description = "desc",
        action = "stop"
      ),
      nodeprops = NodeProps(nodeid = "N001"),
      groupprops = GroupProps(groupid = UUID.randomUUID().toString),
      orgprops = OrgProps(orgid = UUID.randomUUID().toString),
      siteprops = SiteProps(siteid = UUID.randomUUID().toString)
    )

    APIDataFlow(
      id = UUID.randomUUID().toString,
      req = req,
      envelope = Envelope(
        messageid = UUID.randomUUID().toString,
        responsetopic = "topic_resp",
        request = req
      )
    )
  }

}
