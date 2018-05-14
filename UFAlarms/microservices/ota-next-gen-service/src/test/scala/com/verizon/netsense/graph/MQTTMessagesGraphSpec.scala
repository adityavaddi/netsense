package com.verizon.netsense.graph


import java.util.UUID

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Sink, Source}
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import akka.testkit.TestKit
import com.datastax.driver.core.utils.UUIDs
import com.verizon.netsense.config.OTAKafkaConfig
import com.verizon.netsense.entity._
import com.verizon.netsense.helper.{DBHelper, OTAFirmwareMock}
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FunSuiteLike, MustMatchers}
import com.verizon.netsense.database.CassandraConnector.testConnector
import com.verizon.netsense.graph.MQTTMessagesGraphData.DataFlow
import com.verizon.netsense.utils.Deserializer

import scala.concurrent.Await
import scala.concurrent.duration._

class MQTTMessagesGraphSpec extends TestKit(ActorSystem("MQTTMessagesGraphSpec-System"))
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
    val flowUnderTest = MQTTMessagesGraph(OTAFirmwareMock, testConnector).CheckTopicFlow

    val dataFlows: List[MQTTRes] = List(
      MQTTRes(
        a = "",
        p = "global/UNSOL/ota/software/download",
        sid = "",
        e = "",
        d = "",
        s = 1,
        uuid = "",
        l = Map()
      ),
      MQTTRes(
        a = "",
        p = "v1/N001/out/EXEC/REPLY/123/ota/software/install",
        sid = "",
        e = "",
        d = "",
        s = 1,
        uuid = "",
        l = Map()
      )
    )

    val future = Source(dataFlows).via(flowUnderTest).runWith(Sink.seq)
    val results = Await.result(future, waiting)

    assert(results.length == 2)
    assert(results.head == dataFlows.head)
    assert(results.last == dataFlows.last)
  }

  test("CheckTopicFlow negative") {
    val flowUnderTest = MQTTMessagesGraph(OTAFirmwareMock, testConnector).CheckTopicFlow

    val dataFlows: List[MQTTRes] = List(
      MQTTRes(
        a = "",
        p = "global/UNSOL/random/123",
        sid = "",
        e = "",
        d = "",
        s = 1,
        uuid = "",
        l = Map()
      ),
      MQTTRes(
        a = "",
        p = "v1/N001/out/EXEC/REPLY/123/ota/software/error",
        sid = "",
        e = "",
        d = "",
        s = 1,
        uuid = "",
        l = Map()
      )
    )

    val future = Source(dataFlows).via(flowUnderTest).runWith(Sink.seq)
    val results = Await.result(future, waiting)

    assert(results.isEmpty)
  }

  test("CheckJobFlow positive") {
    val flowUnderTest = MQTTMessagesGraph(OTAFirmwareMock, testConnector).CheckJobFlow

    val jobID: String = UUID.randomUUID().toString

    DBHelper.insertJobStatus(jobID, JOB_STARTED)

    val dataFlow: MQTTRes = MQTTRes(
      a = "",
      p = "global/UNSOL/ota/software/download",
      sid = "",
      e = "",
      d = "",
      s = 1,
      uuid = jobID,
      l = Map()
    )

    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting).head

    assert(result.isTerminated == false)
    assert(result.res == dataFlow)
    assert(result.status.isEmpty)
    assert(result.nodeid.isEmpty)
    assert(result.info.isEmpty)
    assert(result.done == false)
    assert(result.progress == -1)
  }

  test("CheckJobFlow negative") {
    val flowUnderTest = MQTTMessagesGraph(OTAFirmwareMock, testConnector).CheckJobFlow

    val jobID: String = UUID.randomUUID().toString

    DBHelper.insertJobStatus(jobID, JOB_DONE)

    val dataFlow: MQTTRes = MQTTRes(
      a = "",
      p = "global/UNSOL/ota/software/download",
      sid = "",
      e = "",
      d = "",
      s = 1,
      uuid = jobID,
      l = Map()
    )

    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting)

    assert(result.isEmpty)
  }

  test("SoftwareDownloadFlow positive") {
    val flowUnderTest = MQTTMessagesGraph(OTAFirmwareMock, testConnector).SoftwareDownloadFlow

    val jobID: String = UUID.randomUUID().toString

    val dataFlows: List[DataFlow] = List(
      DataFlow(res = MQTTRes(
        a = "",
        p = "global/UNSOL/ota/software/download",
        sid = "N001",
        e = "",
        d = "",
        s = 1,
        uuid = jobID,
        l = Map("s" -> 0)
      )),
      DataFlow(res = MQTTRes(
        a = "",
        p = "global/UNSOL/ota/software/download",
        sid = "N001",
        e = "",
        d = "",
        s = 1,
        uuid = jobID,
        l = Map("s" -> 1, "p" -> 200)
      )),
      DataFlow(res = MQTTRes(
        a = "",
        p = "global/UNSOL/ota/software/download",
        sid = "N001",
        e = "",
        d = "",
        s = 1,
        uuid = jobID,
        l = Map("s" -> 2)
      )),
      DataFlow(res = MQTTRes(
        a = "",
        p = "global/UNSOL/ota/software/download",
        sid = "N001",
        e = "",
        d = "",
        s = 1,
        uuid = jobID,
        l = Map("s" -> 3)
      )),
      DataFlow(res = MQTTRes(
        a = "",
        p = "global/UNSOL/ota/software/download",
        sid = "N001",
        e = "",
        d = "",
        s = 1,
        uuid = jobID,
        l = Map()
      ))
    )

    val future = Source(dataFlows).via(flowUnderTest).runWith(Sink.seq)
    val results = Await.result(future, waiting)

    results.foreach(result => {
      val default = 4

      assert(result.nodeid.get == "N001")
      assert(result.progress == result.res.l.getOrElse("p", -1).asInstanceOf[Int].asInstanceOf[Long])

      result.res.l.getOrElse("s", default) match {
        case 0 => assert(result.status.get == DOWNLOAD_INVALID)
        case 1 => assert(result.status.get == DOWNLOAD_IN_PROGRESS)
        case 2 => assert(result.status.get == DOWNLOAD_SUCCESSFUL)
        case 3 => assert(result.status.get == DOWNLOAD_FAILED)
        case _ => assert(result.status.get == DOWNLOAD_STARTED)
      }
    })
  }

  test("SoftwareInstallFlow positive") {
    val flowUnderTest = MQTTMessagesGraph(OTAFirmwareMock, testConnector).SoftwareInstallFlow

    val jobID: String = UUID.randomUUID().toString

    val dataFlows: List[DataFlow] = List(
      DataFlow(res = MQTTRes(
        a = "",
        p = "global/UNSOL/ota/software/install",
        sid = "N001",
        e = "",
        d = "",
        s = 1,
        uuid = jobID,
        l = Map("s" -> 0)
      )),
      DataFlow(res = MQTTRes(
        a = "",
        p = "global/UNSOL/ota/software/install",
        sid = "N001",
        e = "",
        d = "",
        s = 1,
        uuid = jobID,
        l = Map("s" -> 1, "p" -> 200)
      )),
      DataFlow(res = MQTTRes(
        a = "",
        p = "global/UNSOL/ota/software/install",
        sid = "N001",
        e = "",
        d = "",
        s = 1,
        uuid = jobID,
        l = Map("s" -> 2)
      )),
      DataFlow(res = MQTTRes(
        a = "",
        p = "global/UNSOL/ota/software/install",
        sid = "N001",
        e = "",
        d = "",
        s = 1,
        uuid = jobID,
        l = Map("s" -> 3)
      )),
      DataFlow(res = MQTTRes(
        a = "",
        p = "global/UNSOL/ota/software/install",
        sid = "N001",
        e = "",
        d = "",
        s = 1,
        uuid = jobID,
        l = Map()
      ))
    )

    val future = Source(dataFlows).via(flowUnderTest).runWith(Sink.seq)
    val results = Await.result(future, waiting)

    results.foreach(result => {
      val default = 4

      assert(result.nodeid.get == "N001")

      result.res.l.getOrElse("s", default) match {
        case 0 => assert(result.status.get == INSTALL_INVALID)
        case 1 => assert(result.status.get == INSTALL_IN_PROGRESS)
        case 2 => assert(result.status.get == INSTALL_SUCCESSFUL)
        case 3 => assert(result.status.get == INSTALL_FAILED)
        case _ => assert(result.status.get == INSTALL_STARTED)
      }
    })
  }

  test("GetJobInfoIfSuccessFlow positive") {
    val flowUnderTest = MQTTMessagesGraph(OTAFirmwareMock, testConnector).GetJobInfoIfSuccessFlow

    val jobID: String = UUID.randomUUID().toString
    val firmwareID: String = UUID.randomUUID().toString

    DBHelper.insertJobInfo(jobID, firmwareID, 1)

    val dataFlows: List[DataFlow] = List(
      DataFlow(
        res = MQTTRes(
          a = "",
          p = "global/UNSOL/ota/software/download",
          sid = "N001",
          e = "",
          d = "",
          s = 1,
          uuid = jobID,
          l = Map("s" -> 2)
        ),
        status = Some(DOWNLOAD_SUCCESSFUL)
      )
    )

    val future = Source(dataFlows).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting).head

    assert(result.info.get.jobid == jobID)
    assert(result.info.get.firmwareid == firmwareID)
    assert(result.info.get.count == 1)
  }

  test("GetJobInfoIfSuccessFlow negative") {
    val flowUnderTest = MQTTMessagesGraph(OTAFirmwareMock, testConnector).GetJobInfoIfSuccessFlow

    val jobID: String = UUID.randomUUID().toString

    val dataFlows: List[DataFlow] = List(
      DataFlow(
        res = MQTTRes(
          a = "",
          p = "global/UNSOL/ota/software/download",
          sid = "N001",
          e = "",
          d = "",
          s = 1,
          uuid = jobID,
          l = Map("s" -> 2)
        ),
        status = Some(DOWNLOAD_IN_PROGRESS)
      )
    )

    val future = Source(dataFlows).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting)

    assert(result.isEmpty)
  }

  test("InstallSoftwareFlow positive") {
    val flowUnderTest = MQTTMessagesGraph(OTAFirmwareMock, testConnector).InstallSoftwareFlow

    val jobID: String = UUID.randomUUID().toString
    val firmwareID: String = UUID.randomUUID().toString

    DBHelper.insertJobInfo(jobID, firmwareID, 1)

    val jobInfo: OTAJobInfo = DBHelper.getJobInfo(jobID).head
    val dataFlow: DataFlow = DataFlow(
      res = MQTTRes(
        a = "",
        p = "global/UNSOL/ota/software/download",
        sid = "N001",
        e = "",
        d = "",
        s = 1,
        uuid = jobID,
        l = Map("s" -> 2)
      ),
      status = Some(DOWNLOAD_SUCCESSFUL),
      nodeid = Some("N001"),
      info = Some(jobInfo)
    )

    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting)
    val record = result.head
    val map = Deserializer.msgpackMapper.readValue(record.value(), classOf[Map[String, Any]])

    assert(record.topic() == OTAKafkaConfig.mqttReqTopic)
    assert(new String(record.key()) == jobID)
    assert(map.getOrElse("uuid", "") == jobID)
    assert(map.getOrElse("a", "") == "EXEC")
    assert(map.getOrElse("p", "") == s"v1/${dataFlow.nodeid.get}/in/EXEC/ota/software/install")
  }

  test("NodeStatusDB positive") {
    val flowUnderTest = MQTTMessagesGraph(OTAFirmwareMock, testConnector).NodeStatusDB

    val jobID: String = UUID.randomUUID().toString
    val dataFlow: DataFlow = DataFlow(
      res = MQTTRes(
        a = "",
        p = "global/UNSOL/ota/software/download",
        sid = "N001",
        e = "",
        d = "",
        s = 1,
        uuid = jobID,
        l = Map("s" -> 2)
      ),
      status = Some(DOWNLOAD_SUCCESSFUL),
      nodeid = Some("N001"),
      progress = 2
    )

    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    Await.result(future, waiting)

    val results = DBHelper.getNodeStatus(jobID)
    val status = results.head

    assert(status.jobid == jobID)
    assert(status.progress == dataFlow.progress)
    assert(status.status == dataFlow.status.get)
    assert(status.nodeid == dataFlow.nodeid.get)
  }

  test("NodeStatusDB negative") {
    val flowUnderTest = MQTTMessagesGraph(OTAFirmwareMock, testConnector).NodeStatusDB

    val jobID: String = UUID.randomUUID().toString
    val dataFlows: List[DataFlow] = List(
      DataFlow(
        res = MQTTRes(
          a = "",
          p = "global/UNSOL/ota/software/download",
          sid = "N001",
          e = "",
          d = "",
          s = 1,
          uuid = jobID,
          l = Map("s" -> 2)
        ),
        nodeid = Some("N001"),
        progress = 2
      ),
      DataFlow(
        res = MQTTRes(
          a = "",
          p = "global/UNSOL/ota/software/download",
          sid = "N001",
          e = "",
          d = "",
          s = 1,
          uuid = jobID,
          l = Map("s" -> 2)
        ),
        status = Some(DOWNLOAD_SUCCESSFUL),
        progress = 2
      )
    )

    val future = Source(dataFlows).via(flowUnderTest).runWith(Sink.seq)
    Await.result(future, waiting)

    val result = DBHelper.getNodeStatus(jobID)

    assert(result.isEmpty)
  }

  test("ShouldUpdateJobStatusFlow positive") {
    val flowUnderTest = MQTTMessagesGraph(OTAFirmwareMock, testConnector).ShouldUpdateJobStatusFlow

    val jobID: String = UUID.randomUUID().toString
    val firmwareID: String = UUID.randomUUID().toString

    DBHelper.insertJobInfo(jobID, firmwareID, 1)
    DBHelper.insertNodeStatus(OTANodeStatus(
      jobid = jobID,
      nodeid = "N001",
      status = INSTALL_STARTED,
      progress = -1,
      when = UUIDs.timeBased()
    ))

    val dataFlow: DataFlow = DataFlow(
      res = MQTTRes(
        a = "",
        p = "global/UNSOL/ota/software/download",
        sid = "N001",
        e = "",
        d = "",
        s = 1,
        uuid = jobID,
        l = Map("s" -> 2)
      ),
      nodeid = Some("N001"),
      status = Some(INSTALL_SUCCESSFUL)
    )

    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting).head

    assert(result.done)
  }

  test("ShouldUpdateJobStatusFlow negative") {
    val flowUnderTest = MQTTMessagesGraph(OTAFirmwareMock, testConnector).ShouldUpdateJobStatusFlow

    val jobID: String = UUID.randomUUID().toString
    val firmwareID: String = UUID.randomUUID().toString

    DBHelper.insertJobInfo(jobID, firmwareID, 1)
    DBHelper.insertNodeStatus(OTANodeStatus(
      jobid = jobID,
      nodeid = "N001",
      status = INSTALL_STARTED,
      progress = -1,
      when = UUIDs.timeBased()
    ))

    val dataFlow: DataFlow = DataFlow(
      res = MQTTRes(
        a = "",
        p = "global/UNSOL/ota/software/download",
        sid = "N001",
        e = "",
        d = "",
        s = 1,
        uuid = jobID,
        l = Map("s" -> 2)
      ),
      nodeid = Some("N001"),
      status = Some(INSTALL_IN_PROGRESS)
    )

    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting)

    assert(result.isEmpty)
  }

  test("JobStatusDB positive") {
    val flowUnderTest = MQTTMessagesGraph(OTAFirmwareMock, testConnector).JobStatusDB

    val jobID: String = UUID.randomUUID().toString
    val dataFlow: DataFlow = DataFlow(
      res = MQTTRes(
        a = "",
        p = "global/UNSOL/ota/software/download",
        sid = "N001",
        e = "",
        d = "",
        s = 1,
        uuid = jobID,
        l = Map("s" -> 2)
      ),
      done = true
    )

    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    Await.result(future, waiting)

    val results = DBHelper.getJobStatus(jobID)
    val status = results.head

    assert(status.jobid == jobID)
    assert(status.status == JOB_DONE)
  }

  test("JobStatusDB negative") {
    val flowUnderTest = MQTTMessagesGraph(OTAFirmwareMock, testConnector).JobStatusDB

    val jobID: String = UUID.randomUUID().toString
    val dataFlow: DataFlow = DataFlow(
      res = MQTTRes(
        a = "",
        p = "global/UNSOL/ota/software/download",
        sid = "N001",
        e = "",
        d = "",
        s = 1,
        uuid = jobID,
        l = Map("s" -> 2)
      ),
      done = false
    )

    val future = Source(List(dataFlow)).via(flowUnderTest).runWith(Sink.seq)
    Await.result(future, waiting)

    val results = DBHelper.getJobStatus(jobID)

    assert(results.isEmpty)
  }

}
