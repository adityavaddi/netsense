package com.verizon.netsense.graph

import java.util.UUID
import akka.actor.ActorSystem
import akka.stream.scaladsl.{Sink, Source}
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import akka.testkit.TestKit
import com.verizon.netsense.entity.Constants
import com.verizon.netsense.helper.DBHelper
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FunSuiteLike, MustMatchers}
import com.verizon.netsense.entity._
import scala.concurrent.duration._
import com.verizon.netsense.database.CassandraConnector.testConnector
import com.verizon.netsense.graph.DSResGraphData._
import org.json4s.JValue
import scala.concurrent.Await

class DSResGraphSpec extends TestKit(ActorSystem("DSResGraphSpec-System"))
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

  test("GetJobInfoFlow positive") {
    val flowUnderTest = DSResGraph(testConnector).GetJobInfoFlow

    val jobID: String = UUID.randomUUID().toString

    DBHelper.insertJobInfo(
      jobID = jobID,
      firmwareID = UUID.randomUUID().toString,
      count = 0
    )

    val dsResps: List[DSRes] = List(
      DSRes(
        messageid = jobID,
        response = encodeJson(Map(
          "success" -> true,
          "node" -> Map(
            "nodeid" -> "N001",
            "model" -> "unode-v7"
          )
        ))
      ),
      DSRes(
        messageid = jobID,
        response = encodeJson(Map(
          "success" -> true,
          "group" -> Map(
            "nodes" -> List(
              Map(
                "nodeid" -> "N001",
                "model" -> "unode-v7"
              )
            )
          )
        ))
      ),
      DSRes(
        messageid = jobID,
        response = encodeJson(Map(
          "success" -> true,
          "items" -> List(
            Map(
              "nodeid" -> "N001",
              "model" -> "unode-v7"
            )
          )
        ))
      )
    )

    val future = Source(dsResps).via(flowUnderTest).runWith(Sink.seq)
    val results = Await.result(future, waiting)

    results.foreach(result => {
      assert(!result.error)
      assert(result.nodes.head.nodeid == "N001")
      assert(result.nodes.head.model == "unode-v7")
      assert(result.job.isDefined)
    })
  }

  test("GetJobInfoFlow negative") {
    val flowUnderTest = DSResGraph(testConnector).GetJobInfoFlow

    val jobID: String = UUID.randomUUID().toString

    val dsResps: List[DSRes] = List(
      DSRes(
        messageid = jobID,
        response = encodeJson(Map(
          "success" -> false,
          "node" -> Map()
        ))
      ),
      DSRes(
        messageid = jobID,
        response = encodeJson(Map(
          "success" -> true,
          "group" -> Map(
            "nodes" -> List()
          )
        ))
      )
      ,
      DSRes(
        messageid = jobID,
        response = encodeJson(Map(
          "success" -> true,
          "items" -> List()
        ))
      )
    )

    val future = Source(dsResps).via(flowUnderTest).runWith(Sink.seq)
    val results = Await.result(future, waiting)

    results.foreach(result => {
      assert(result.error)
      assert(result.nodes.isEmpty)
      assert(result.job.isEmpty)
    })
  }

  test("FilterResError positive") {
    val flowUnderTest = DSResGraph(testConnector).FilterResError

    val jobID: String = UUID.randomUUID().toString

    val dataFlow: DataFlow = DataFlow(
      error = true,
      nodes = List(),
      status = None,
      job = None
    )

    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting).head

    assert(result.status.get == JOB_DS_ERR)
  }

  test("FilterResError negative") {
    val flowUnderTest = DSResGraph(testConnector).FilterResError

    val jobID: String = UUID.randomUUID().toString

    val dataFlow: DataFlow = DataFlow(
      nodes = List(),
      job = None
    )

    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    val results = Await.result(future, waiting)

    assert(results.isEmpty)
  }

  test("FilterJobStatusFlow positive") {
    val flowUnderTest = DSResGraph(testConnector).FilterJobStatusFlow

    val jobID: String = UUID.randomUUID().toString

    DBHelper.insertJobInfo(
      jobID = jobID,
      firmwareID = UUID.randomUUID().toString,
      count = 0
    )
    DBHelper.insertJobStatus(
      jobID = jobID,
      status = JOB_INIT
    )

    val info: OTAJobInfo = DBHelper.getJobInfo(jobID).head

    val dataFlow: DataFlow = DataFlow(
      nodes = List(),
      job = Some(info)
    )

    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting).head

    assert(result.status.get == JOB_STARTED)
  }

  test("FilterJobStatusFlow negative") {
    val flowUnderTest = DSResGraph(testConnector).FilterJobStatusFlow

    val jobID: String = UUID.randomUUID().toString

    DBHelper.insertJobInfo(
      jobID = jobID,
      firmwareID = UUID.randomUUID().toString,
      count = 0
    )
    DBHelper.insertJobStatus(
      jobID = jobID,
      status = JOB_STARTED
    )

    val info: OTAJobInfo = DBHelper.getJobInfo(jobID).head
    val dataFlow: DataFlow = DataFlow(
      nodes = List(),
      job = Some(info)
    )

    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    val results = Await.result(future, waiting)

    assert(results.isEmpty)
  }

  test("DivideJobFlow positive") {
    val flowUnderTest = DSResGraph(testConnector).DivideJobFlow

    val jobID: String = UUID.randomUUID().toString
    val firmwareID: String = UUID.randomUUID().toString

    DBHelper.insertJobInfo(
      jobID = jobID,
      firmwareID = firmwareID,
      count = 0
    )

    val info: OTAJobInfo = DBHelper.getJobInfo(jobID).head
    val dataFlow: DataFlow = DataFlow(
      nodes = List(
        Node(
          nodeid = "N001",
          model = "unode-v7"
        ),
        Node(
          nodeid = "N002",
          model = "unode-v7"
        )),
      job = Some(info)
    )

    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    val results = Await.result(future, waiting)

    results.foreach(result => {
      assert(result.isInstanceOf[Job])
      assert(result.jobid == jobID)
      assert(result.firmwareid == firmwareID)
      assert(result.model == "unode-v7")
      assert(result.nodeid == "N001" || result.nodeid == "N002")
    })
  }

  test("DivideJobFlow negative") {
    val flowUnderTest = DSResGraph(testConnector).DivideJobFlow

    val dataFlow: DataFlow = DataFlow(
      nodes = List(),
      job = None
    )

    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    val results = Await.result(future, waiting)

    assert(results.isEmpty)
  }

  test("JobStatusDB positive") {
    val flowUnderTest = DSResGraph(testConnector).JobStatusDB

    val jobID: String = UUID.randomUUID().toString
    val firmwareID: String = UUID.randomUUID().toString

    DBHelper.insertJobInfo(
      jobID = jobID,
      firmwareID = firmwareID,
      count = 0
    )

    val info: OTAJobInfo = DBHelper.getJobInfo(jobID).head
    val dataFlow: DataFlow = DataFlow(
      nodes = List(),
      job = Some(info)
    )

    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    Await.result(future, waiting)

    val result = DBHelper.getJobStatus(jobID).head

    assert(result.jobid == jobID)
    assert(result.status == JOB_STARTED)
  }

  test("JobStatusDB negative") {
    val flowUnderTest = DSResGraph(testConnector).JobStatusDB

    val jobID: String = UUID.randomUUID().toString
    val firmwareID: String = UUID.randomUUID().toString

    DBHelper.insertJobInfo(
      jobID = jobID,
      firmwareID = firmwareID,
      count = 0
    )

    val info: OTAJobInfo = DBHelper.getJobInfo(jobID).head
    val dataFlow: DataFlow = DataFlow(
      nodes = List(),
      job = Some(info),
      status = Some(JOB_DS_ERR)
    )

    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    Await.result(future, waiting)

    val result = DBHelper.getJobStatus(jobID).head

    assert(result.jobid == jobID)
    assert(result.status == JOB_DS_ERR)
  }

  test("JobInfoDB positive") {
    val flowUnderTest = DSResGraph(testConnector).JobInfoDB

    val jobID: String = UUID.randomUUID().toString
    val firmwareID: String = UUID.randomUUID().toString

    DBHelper.insertJobInfo(
      jobID = jobID,
      firmwareID = firmwareID,
      count = 0
    )

    val info: OTAJobInfo = DBHelper.getJobInfo(jobID).head
    val dataFlow: DataFlow = DataFlow(
      nodes = List(Node(
        nodeid = "N001",
        model = "unode-v7"
      )),
      job = Some(info)
    )

    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    Await.result(future, waiting)

    val result = DBHelper.getJobInfo(jobID).head

    assert(result.jobid == jobID)
    assert(result.count == 1)
  }

  test("JobInfoDB negative") {
    val flowUnderTest = DSResGraph(testConnector).JobInfoDB

    val jobID: String = UUID.randomUUID().toString
    val firmwareID: String = UUID.randomUUID().toString

    DBHelper.insertJobInfo(
      jobID = jobID,
      firmwareID = firmwareID,
      count = 0
    )

    val info: OTAJobInfo = DBHelper.getJobInfo(jobID).head
    val dataFlow: DataFlow = DataFlow(
      nodes = List(),
      job = Some(info),
      status = Some(JOB_DS_ERR)
    )

    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    val results = Await.result(future, waiting)

    assert(results.isEmpty)

    val dbResult = DBHelper.getJobInfo(jobID).head

    assert(dbResult.jobid == jobID)
    assert(dbResult.count == 0)
  }

  def encodeJson(src: AnyRef): JValue = {
    import org.json4s.{Extraction, NoTypeHints}
    import org.json4s.jackson.Serialization
    implicit val formats = Serialization.formats(NoTypeHints)

    Extraction.decompose(src)
  }

}
