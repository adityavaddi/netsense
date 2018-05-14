package com.verizon.netsense.graph

import java.util.UUID

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Sink, Source}
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import akka.testkit.TestKit
import com.datastax.driver.core.utils.UUIDs
import com.verizon.netsense.database.CassandraConnector.testConnector
import com.verizon.netsense.entity._
import com.verizon.netsense.graph.APIGetGraphData.GetJobFlowData
import com.verizon.netsense.helper.{DBHelper, OTAFirmwareMock}
import org.joda.time.{DateTime, DateTimeZone}
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FunSuiteLike, MustMatchers}

import scala.concurrent.Await
import scala.concurrent.duration._

class APIGetGraphSpec extends TestKit(ActorSystem("DSResGraphSpec-System"))
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

  test("GetJobsForSiteFlow positive") {
    val flowUnderTest = APIGetGraph(OTAFirmwareMock, testConnector).GetJobsForSiteFlow

    val jobID: String = UUID.randomUUID().toString
    val orgID: String = UUID.randomUUID().toString
    val siteID: String = UUID.randomUUID().toString

    DBHelper.insertJobRelation(OTAJobRelation(
      orgid = orgID,
      siteid = siteID,
      jobid = jobID,
      when = UUIDs.timeBased()
    ))

    DBHelper.insertJobInfo(jobID = jobID, firmwareID = "F001-merlin", count = 1)

    val apiDataFlow: APIDataFlow = createAPIDataFlowFromJobID(orgID, siteID, jobID)

    val future = Source(List(apiDataFlow)).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting).head

    val res = result.res.get

    assert(res.success)
    assert(res.data.isDefined)
    assert(res.data.get.isInstanceOf[List[OTAJobInfoConverted]])

    val info = res.data.get.asInstanceOf[List[OTAJobInfoConverted]].head

    assert(info.count == 1)
    assert(info.firmwareid == "F001-merlin")
    assert(info.jobid == jobID)
    assert(info.model == "merlin")
  }

  test("GetJobsForSiteFlow negative") {
    val flowUnderTest = APIGetGraph(OTAFirmwareMock, testConnector).GetJobsForSiteFlow

    val jobID: String = UUID.randomUUID().toString
    val orgID: String = UUID.randomUUID().toString
    val siteID: String = UUID.randomUUID().toString

    DBHelper.insertJobRelation(OTAJobRelation(
      orgid = orgID,
      siteid = siteID,
      jobid = jobID,
      when = UUIDs.timeBased()
    ))

    DBHelper.insertJobInfo(jobID = UUID.randomUUID().toString, firmwareID = "F001", count = 1)

    val apiDataFlow: APIDataFlow = createAPIDataFlowFromJobID(orgID, siteID, jobID)

    val future = Source(List(apiDataFlow)).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting).head

    val res = result.res.get

    assert(res.success)
    assert(res.data.isDefined)
    assert(res.data.get.isInstanceOf[List[OTAJobInfoConverted]])

    val info = res.data.get.asInstanceOf[List[OTAJobInfoConverted]]

    assert(info.isEmpty)
  }

  test("GetJobFlow positive") {
    val flowUnderTest = APIGetGraph(OTAFirmwareMock, testConnector).GetJobFlow

    val jobID: String = UUID.randomUUID().toString
    val orgID: String = UUID.randomUUID().toString
    val siteID: String = UUID.randomUUID().toString

    DBHelper.insertJobInfo(jobID = jobID, firmwareID = "F001-merlin", count = 1)

    val apiDataFlow: APIDataFlow = createAPIDataFlowFromJobID(orgID, siteID, jobID)

    val future = Source(List(apiDataFlow)).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting).head

    assert(result.apiData == apiDataFlow)
    assert(result.info.isDefined)

    val info = result.info.get

    assert(info.isInstanceOf[OTAJobInfoConverted])
    assert(info.jobid == jobID)
    assert(info.firmwareid == "F001-merlin")
    assert(info.count == 1)
  }

  test("GetJobFlow negative") {
    val flowUnderTest = APIGetGraph(OTAFirmwareMock, testConnector).GetJobFlow

    val jobID: String = UUID.randomUUID().toString
    val orgID: String = UUID.randomUUID().toString
    val siteID: String = UUID.randomUUID().toString
    val apiDataFlow: APIDataFlow = createAPIDataFlowFromJobID(orgID, siteID, jobID)

    val future = Source(List(apiDataFlow)).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting).head

    assert(result.apiData == apiDataFlow)
    assert(result.info.isEmpty)
  }

  test("GetJobErrorFlow positive") {
    val flowUnderTest = APIGetGraph(OTAFirmwareMock, testConnector).GetJobErrorFlow

    val jobID: String = UUID.randomUUID().toString
    val orgID: String = UUID.randomUUID().toString
    val siteID: String = UUID.randomUUID().toString

    val getJobFlowData: GetJobFlowData = GetJobFlowData(
      apiData = createAPIDataFlowFromJobID(orgID, siteID, jobID)
    )

    val future = Source(List(getJobFlowData)).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting).head

    assert(result.res.isDefined)

    val res: OTARes = result.res.get

    assert(!res.success)
    assert(res.error.isDefined)
    assert(res.error.get.status == 404)
    assert(res.error.get.message == s"Job ID ${jobID} not found")

  }

  test("GetJobErrorFlow negative") {
    val flowUnderTest = APIGetGraph(OTAFirmwareMock, testConnector).GetJobErrorFlow

    val jobID: String = UUID.randomUUID().toString
    val orgID: String = UUID.randomUUID().toString
    val siteID: String = UUID.randomUUID().toString

    DBHelper.insertJobInfo(jobID = jobID, firmwareID = "F001-merlin", count = 1)

    val info: OTAJobInfo = DBHelper.getJobInfo(jobID).head
    val infoConverted: OTAJobInfoConverted = OTAJobInfoConverted(
      jobid = info.jobid,
      firmwareid = info.firmwareid,
      targetid = info.targetid,
      targettype = info.targettype,
      model = "merlin",
      count = info.count,
      description = info.description,
      when = new DateTime(UUIDs.unixTimestamp(info.when)).toDateTime(DateTimeZone.UTC).toString()
    )

    val getJobFlowData: GetJobFlowData = GetJobFlowData(
      apiData = createAPIDataFlowFromJobID(orgID, siteID, jobID),
      info = Some(infoConverted)
    )

    val future = Source(List(getJobFlowData)).via(flowUnderTest).runWith(Sink.seq)
    val results = Await.result(future, waiting)

    assert(results.isEmpty)
  }

  test("GetJobInfoFlow positive") {
    val flowUnderTest = APIGetGraph(OTAFirmwareMock, testConnector).GetJobInfoFlow

    val jobID: String = UUID.randomUUID().toString
    val orgID: String = UUID.randomUUID().toString
    val siteID: String = UUID.randomUUID().toString

    DBHelper.insertJobInfo(jobID = jobID, firmwareID = "F001-merlin", count = 1)
    DBHelper.insertJobStatus(jobID = jobID, status = JOB_STARTED)
    DBHelper.insertNodeStatus(OTANodeStatus(
      jobid = jobID,
      nodeid = "N001",
      status = DOWNLOAD_STARTED,
      progress = -1,
      when = UUIDs.timeBased()
    ))

    val info: OTAJobInfo = DBHelper.getJobInfo(jobID).head
    val infoConverted: OTAJobInfoConverted = OTAJobInfoConverted(
      jobid = info.jobid,
      firmwareid = info.firmwareid,
      targetid = info.targetid,
      targettype = info.targettype,
      model = "merlin",
      count = info.count,
      description = info.description,
      when = new DateTime(UUIDs.unixTimestamp(info.when)).toDateTime(DateTimeZone.UTC).toString()
    )

    val getJobFlowData: GetJobFlowData = GetJobFlowData(
      apiData = createAPIDataFlowFromJobID(orgID, siteID, jobID),
      info = Some(infoConverted)
    )

    val future = Source(List(getJobFlowData)).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting).head

    assert(result.res.isDefined)

    val res: OTARes = result.res.get

    assert(res.success)
    assert(res.data.isDefined)
    assert(res.error.isEmpty)

    val data: Map[String, Any] = res.data.get.asInstanceOf[Map[String, Any]]

    assert(data.isDefinedAt("job_info"))
    assert(data.isDefinedAt("job_status"))
    assert(data.isDefinedAt("nodes_status"))
    assert(data.get("job_info").get.isInstanceOf[OTAJobInfoConverted])
    assert(data.get("job_status").get.isInstanceOf[List[OTAJobStatusConverted]])
    assert(data.get("nodes_status").get.isInstanceOf[Map[String, OTANodeStatusConverted]])
  }

  test("GetJobInfoFlow negative") {
    val flowUnderTest = APIGetGraph(OTAFirmwareMock, testConnector).GetJobInfoFlow

    val jobID: String = UUID.randomUUID().toString
    val orgID: String = UUID.randomUUID().toString
    val siteID: String = UUID.randomUUID().toString

    val getJobFlowData: GetJobFlowData = GetJobFlowData(
      apiData = createAPIDataFlowFromJobID(orgID, siteID, jobID)
    )

    val future = Source(List(getJobFlowData)).via(flowUnderTest).runWith(Sink.seq)
    val results = Await.result(future, waiting)

    assert(results.isEmpty)
  }

  test("GetFirmwares positive") {
    val flowUnderTest = APIGetGraph(OTAFirmwareMock, testConnector).GetFirmwares

    val jobID: String = UUID.randomUUID().toString
    val orgID: String = UUID.randomUUID().toString
    val siteID: String = UUID.randomUUID().toString

    val apiDataFlow: APIDataFlow = createAPIDataFlowFromJobID(orgID, siteID, jobID)

    val future = Source(List(apiDataFlow)).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting).head
    val res = result.res.get

    assert(res.success)

    val data: List[Map[String, Any]] = res.data.get.asInstanceOf[List[Map[String, Any]]]
    val firmware = OTAFirmwareMock.getFirmwares().head

    assert(data.head.getOrElse("firmwareid", "") == firmware.firmwareid)
    assert(data.head.getOrElse("release", "") == firmware.version)
    assert(data.head.getOrElse("type", "") == firmware.`type`)
    assert(data.head.getOrElse("image_size", "") == firmware.size)
    assert(data.head.get("name").isDefined)
  }

  test("GetFirmware positive") {
    val flowUnderTest = APIGetGraph(OTAFirmwareMock, testConnector).GetFirmware

    val jobID: String = UUID.randomUUID().toString
    val orgID: String = UUID.randomUUID().toString
    val siteID: String = UUID.randomUUID().toString

    val apiDataFlow: APIDataFlow = createAPIDataFlowFromJobID(orgID, siteID, jobID)

    val future = Source(List(apiDataFlow)).via(flowUnderTest).runWith(Sink.seq)
    val result = Await.result(future, waiting).head
    val res = result.res.get

    assert(res.success)

    val data: Map[String, Any] = res.data.get.asInstanceOf[Map[String, Any]]
    val firmware = OTAFirmwareMock.getFirmwares().head

    assert(data.getOrElse("firmwareid", "") == firmware.firmwareid)
    assert(data.getOrElse("release", "") == firmware.version)
    assert(data.getOrElse("type", "") == firmware.`type`)
    assert(data.getOrElse("image_size", "") == firmware.size)
    assert(data.get("name").isDefined)
  }

  def createAPIDataFlowFromJobID(orgID: String, siteID: String, jobID: String): APIDataFlow = {
    val req: OTAReq = OTAReq(
      `type` = "t",
      model = "",
      action = "",
      user = UUID.randomUUID().toString,
      firmwareprops = FirmwareProps(firmwareid = "F007-merlin"),
      otaprops = OTAProps(
        jobid = jobID,
        description = "desc",
        action = "stop"
      ),
      nodeprops = NodeProps(nodeid = "N001"),
      groupprops = GroupProps(groupid = UUID.randomUUID().toString),
      orgprops = OrgProps(orgid = orgID),
      siteprops = SiteProps(siteid = siteID)
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
