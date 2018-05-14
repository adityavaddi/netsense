package com.verizon.netsense.graph

import java.util.UUID

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import akka.stream.scaladsl.{Sink, Source}
import akka.testkit.TestKit
import com.datastax.driver.core.utils.UUIDs
import com.verizon.netsense.config.OTAKafkaConfig
import com.verizon.netsense.entity._
import com.verizon.netsense.graph.JobGraphData.{DataFlow, DataFlowWrapper}
import com.verizon.netsense.database.CassandraConnector.testConnector
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FunSuiteLike, MustMatchers}
import com.verizon.netsense.helper.{DBHelper, OTAFirmwareMock}
import com.verizon.netsense.utils.Deserializer

import scala.concurrent.Await
import scala.concurrent.duration._

class JobGraphSpec extends TestKit(ActorSystem("JobGraphSpec-System"))
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

  test("CheckJobFlow positive") {
    val flowUnderTest = JobGraph(OTAFirmwareMock, testConnector).CheckJobFlow

    val states = List(JOB_INIT, JOB_STARTED)
    val dataFlows: List[DataFlow] = DBHelper.createDataFlowsFromState(states)

    val future = Source(dataFlows).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting)

    assert(result == dataFlows)
  }

  test("CheckJobFlow negative") {
    val flowUnderTest = JobGraph(OTAFirmwareMock, testConnector).CheckJobFlow

    val states = List(JOB_STOPPED, JOB_DONE, JOB_404, JOB_TIMEOUT, JOB_DS_ERR, FW_NOT_FOUND)
    val dataFlows: List[DataFlow] = DBHelper.createDataFlowsFromState(states)

    val future = Source(dataFlows).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting)

    assert(result.isEmpty)
  }

  test("CheckFirmwareFlow positive") {
    val flowUnderTest = JobGraph(OTAFirmwareMock, testConnector).CheckFirmwareFlow

    val firmwares = OTAFirmwareMock.getFirmwares()
    val firmware: Firmware = firmwares.head

    val dataFlow: DataFlow = DataFlow(
      job = Job(
        jobid = UUID.randomUUID().toString,
        nodeid = "N001",
        model = firmware.`type`,
        firmwareid = firmware.firmwareid
      ),
      error = None,
      firmware = None,
      isTerminated = false
    )

    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting)

    assert(result.head.firmware.get.commitid == firmware.commitid)
  }

  test("CheckFirmwareFlow negative") {
    val flowUnderTest = JobGraph(OTAFirmwareMock, testConnector).CheckFirmwareFlow

    val dataFlow: DataFlow = DataFlow(
      job = Job(
        jobid = UUID.randomUUID().toString,
        nodeid = "N001",
        model = "unknown",
        firmwareid = "unknown"
      ),
      error = None,
      firmware = None,
      isTerminated = false
    )

    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting)

    assert(result.head.error.get == FW_NOT_FOUND)
  }

  test("MQTTFlow positive") {
    val flowUnderTest = JobGraph(OTAFirmwareMock, testConnector).MQTTFlow

    val firmwares = OTAFirmwareMock.getFirmwares()
    val firmware: Firmware = firmwares.head

    val dataFlow: DataFlow = DataFlow(
      job = Job(
        jobid = UUID.randomUUID().toString,
        nodeid = "N001",
        model = firmware.`type`,
        firmwareid = firmware.commitid
      ),
      error = None,
      firmware = Some(firmware),
      isTerminated = false
    )

    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting)
    val record = result.head
    val map = Deserializer.msgpackMapper.readValue(record.value(), classOf[Map[String, Any]])

    assert(record.topic() == OTAKafkaConfig.mqttReqTopic)
    assert(new String(record.key()) == dataFlow.job.jobid)
    assert(map.getOrElse("uuid", "") == dataFlow.job.jobid)
    assert(map.getOrElse("a", "") == "EXEC")
    assert(map.getOrElse("p", "") == s"v1/${dataFlow.job.nodeid}/in/EXEC/ota/software/download")
  }

  test("MQTTFlow negative") {
    val flowUnderTest = JobGraph(OTAFirmwareMock, testConnector).MQTTFlow

    val firmwares = OTAFirmwareMock.getFirmwares()
    val firmware: Firmware = firmwares.head

    val dataFlow1: DataFlow = DataFlow(
      job = Job(
        jobid = UUID.randomUUID().toString,
        nodeid = "N001",
        model = "unode-v7",
        firmwareid = "F007"
      ),
      error = None,
      firmware = None,
      isTerminated = false
    )

    val dataFlow2: DataFlow = DataFlow(
      job = Job(
        jobid = UUID.randomUUID().toString,
        nodeid = "N001",
        model = "unode-v7",
        firmwareid = "F007"
      ),
      error = Some(""),
      firmware = Some(firmware),
      isTerminated = false
    )

    val future = Source(List(dataFlow1, dataFlow2)).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting)
    assert(result.isEmpty)
  }

  test("NodeStatusFlow positive") {
    val flowUnderTest = JobGraph(OTAFirmwareMock, testConnector).NodeStatusFlow

    val dataFlow: DataFlow = DataFlow(
      job = Job(
        jobid = UUID.randomUUID().toString,
        nodeid = "N001",
        model = "unode-v7",
        firmwareid = "F007"
      ),
      error = None,
      firmware = None,
      isTerminated = false
    )

    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting)

    val status = DBHelper.getNodeStatus(dataFlow.job.jobid).head

    assert(dataFlow.job.jobid == status.jobid)
    assert(dataFlow.job.nodeid == status.nodeid)
    assert(DOWNLOAD_SENT == status.status)
    assert(-1 == status.progress)
  }

  test("NodeStatusFlow negative") {
    val flowUnderTest = JobGraph(OTAFirmwareMock, testConnector).NodeStatusFlow

    val dataFlow: DataFlow = DataFlow(
      job = Job(
        jobid = UUID.randomUUID().toString,
        nodeid = "N001",
        model = "unode-v7",
        firmwareid = "F007"
      ),
      error = Some("ERROR"),
      firmware = None,
      isTerminated = false
    )

    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting)

    val status = DBHelper.getNodeStatus(dataFlow.job.jobid).head

    assert(dataFlow.job.jobid == status.jobid)
    assert(dataFlow.job.nodeid == status.nodeid)
    assert("ERROR" == status.status)
    assert(-1 == status.progress)
  }

  test("ShouldUpdateJobStatusFlow positive") {
    val flowUnderTest = JobGraph(OTAFirmwareMock, testConnector).ShouldUpdateJobStatusFlow
    val states: List[String] = List(INSTALL_SUCCESSFUL, INSTALL_FAILED, INSTALL_INVALID, INSTALL_ERROR, DOWNLOAD_FAILED,
      DOWNLOAD_ERROR, DOWNLOAD_INVALID, FW_NOT_FOUND)
    val jobID: String = UUID.randomUUID().toString
    val dataFlows: List[DataFlow] = states.map(state => {
      val job: Job = Job(
        jobid = jobID,
        nodeid = UUID.randomUUID().toString,
        model = "unode-v7",
        firmwareid = "F001"
      )

      DataFlow(
        job = job,
        error = Some(state),
        firmware = None,
        isTerminated = false
      )
    })

    DBHelper.insertJobInfo(jobID, "F001", states.length + 1)
    dataFlows.foreach(data => {
      DBHelper.insertNodeStatus(OTANodeStatus(
        jobid = jobID,
        nodeid = data.job.nodeid,
        status = data.error.get,
        progress = -1,
        when = UUIDs.timeBased()
      ))
    })
    DBHelper.insertNodeStatus(OTANodeStatus(
      jobid = jobID,
      nodeid = "N001",
      status = INSTALL_SUCCESSFUL,
      progress = -1,
      when = UUIDs.timeBased()
    ))

    val future = Source(dataFlows).via(flowUnderTest).runWith(Sink.seq)
    val results = Await.result(future, waiting)

    results.foreach(result => {
      assert(result.done)
    })
  }

  test("ShouldUpdateJobStatusFlow negative") {
    val flowUnderTest = JobGraph(OTAFirmwareMock, testConnector).ShouldUpdateJobStatusFlow
    val dataFlow: DataFlow = DataFlow(
      job = Job(
        jobid = UUID.randomUUID().toString,
        nodeid = "N001",
        model = "unode-v7",
        firmwareid = "F001"
      ),
      error = Some(DOWNLOAD_IN_PROGRESS),
      firmware = None,
      isTerminated = false
    )

    DBHelper.insertJobInfo(dataFlow.job.jobid, "F001", 1)
    DBHelper.insertNodeStatus(OTANodeStatus(
      jobid = dataFlow.job.jobid,
      nodeid = dataFlow.job.nodeid,
      status = DOWNLOAD_SENT,
      progress = -1,
      when = UUIDs.timeBased()
    ))

    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    val results = Await.result(future, waiting)

    assert(results.head.done == false)
  }

  test("JobStatusFlow positive") {
    val flowUnderTest = JobGraph(OTAFirmwareMock, testConnector).JobStatusFlow

    val nodeID = "N001"
    val jobID = UUID.randomUUID().toString
    val firmwareID = "F001"

    val data: DataFlowWrapper = DataFlowWrapper(
      data = DataFlow(
        job = Job(
          jobid = jobID,
          nodeid = nodeID,
          model = "unode-v7",
          firmwareid = firmwareID
        ),
        error = None,
        firmware = None,
        isTerminated = false
      ),
      info = OTAJobInfo(
        jobid = jobID,
        firmwareid = firmwareID,
        targetid = UUID.randomUUID().toString,
        targettype = "site",
        count = 1,
        description = "desc",
        when = UUIDs.timeBased()
      ),
      done = true
    )

    val future = Source(List(data)).via(flowUnderTest).runWith(Sink.seq)
    val results = Await.result(future, waiting)

    val status = DBHelper.getJobStatus(jobID).head

    assert(jobID == status.jobid)
    assert(JOB_DONE == status.status)
  }

  test("JobStatusFlow negative") {
    val flowUnderTest = JobGraph(OTAFirmwareMock, testConnector).JobStatusFlow

    val nodeID = "N001"
    val jobID = UUID.randomUUID().toString
    val firmwareID = "F001"

    val data: DataFlowWrapper = DataFlowWrapper(
      data = DataFlow(
        job = Job(
          jobid = jobID,
          nodeid = nodeID,
          model = "unode-v7",
          firmwareid = firmwareID
        ),
        error = None,
        firmware = None,
        isTerminated = false
      ),
      info = OTAJobInfo(
        jobid = jobID,
        firmwareid = firmwareID,
        targetid = UUID.randomUUID().toString,
        targettype = "site",
        count = 1,
        description = "desc",
        when = UUIDs.timeBased()
      ),
      done = false
    )

    val future = Source(List(data)).via(flowUnderTest).runWith(Sink.seq)
    val results = Await.result(future, waiting)

    assert(DBHelper.getJobStatus(jobID).isEmpty)
  }
}