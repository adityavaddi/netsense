package com.verizon.netsense.sse.specs.service

import akka.actor.ActorSystem
import akka.stream.Supervision
import akka.testkit.TestKit
import com.outworkers.phantom.connectors.CassandraConnection
import com.outworkers.phantom.dsl._
import com.verizon.netsense.sse.service.{BaseSSEService, SubscribeProcess}
import com.verizon.netsense.sse.specs.data.TestData
import com.verizon.netsense.sse.specs.db.CassandraSpec
import com.verizon.netsense.sse.specs.phantom.PhantomServiceSpec
import com.verizon.netsense.utils.Logging
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, MustMatchers}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.control.NonFatal

/**
 * Created by muppasw on 8/18/17.
 */
class SSESubscribeSpec
    extends TestKit(ActorSystem("SSESubscribeSpec"))
    with FlatSpecLike
    with PhantomServiceSpec
    with MustMatchers
    with CassandraSpec
    with BaseSSEService
    with TestData
    with ScalaFutures
    with BeforeAndAfterAll
    with Logging {

  def graphiteHost: String = "test"
  def graphitePort: Int    = 1

  implicit val conn: CassandraConnection = startDB()
  implicit val session: Session          = startDB().session

  override def kafkaBootstrapServers: String = ???
  override def kafkaClientId: String         = ???
  override def kafkaGroupId: String          = ???
  override def kafkaOffsetReset: String      = ???

  override def beforeAll() = {
    createSchemaAndTable(session)
    insertIntoDevices(session)
    insertIntoFilter(session)
    insertIntoBusinessAlertFilter(session)
    super.beforeAll()
  }

  override def afterAll() = {
    deleteFilter(session)
    stopDB()
    super.afterAll()
  }

  val decider: Supervision.Decider = {
    case NonFatal(ex) =>
      log.error("Got non fatal exception in SSEActor flow", ex)
      Supervision.Resume
    case ex =>
      log.error("Got fatal exception in SSEActor flow, stream will be stopped", ex)
      Supervision.Stop
  }
  /*
   *  Validate process with
   *
   */

  "Validate" should "process orgid and siteid" in {
    //For DeviceEvents
    val sseSubRequest  = generateValidSubscribeRequest(stype)
    val actualResponse = SubscribeProcess.validateAndProcess(sseSubRequest)

    // For BusinessAlerts
    val sseBusinessAlertSubRequest  = generateValidBusinessAlertSubscribeRequest(stype)
    val businessAlertactualResponse = SubscribeProcess.validateAndProcessBusinessAlert(sseBusinessAlertSubRequest)

    log.info("ActualResponse:::::" + Await.result(actualResponse, 1 second))
    Await.result(actualResponse, 1 second)

    log.info("BusinessAlertActualResponse:::::" + Await.result(businessAlertactualResponse, 1 second))
    Await.result(businessAlertactualResponse, 1 second)
  }

  "Subscribe" should "validate with valid eventtype `subscribe`" in {
    //For DeviceEvents
    val sseSubRequest      = generateValidSubscribeRequest(stype)
    val sseSuccessResponse = generateSubscribeResponse(stype, scode, smsg)

    val actualResponse = SubscribeProcess.buildProcess(sseSubRequest)
    log.info("ActualResponse -->" + Await.result(actualResponse, 1 second))

    val expectedResponse = Future(sseSuccessResponse)
    log.info("ExpectedResponse -->" + Await.result(expectedResponse, 1 second))

    // For BusinessAlerts
    val sseBusinessAlertSubRequest      = generateValidBusinessAlertSubscribeRequest(stype)
    val sseBusinessAlertSuccessResponse = generateValidBusinessAlertSubscribeResponse(stype, scode, smsg)

    val businessActualResponse = SubscribeProcess.buildBusinessAlertProcess(sseBusinessAlertSubRequest)
    log.info("BusinessAlertActualResponse -->" + Await.result(actualResponse, 1 second))

    val businessExpectedResponse = Future(sseBusinessAlertSuccessResponse)
    log.info("BusinessAlertExpectedResponse -->" + Await.result(expectedResponse, 1 second))

    Await.result(actualResponse, 1 second) mustEqual Await.result(expectedResponse, 1 second)
    Await.result(businessActualResponse, 1 second) mustEqual Await.result(businessExpectedResponse, 1 second)
  }

  "Heartbeat" should "validate with valid eventtype `heartbeat`" in {
    //For DeviceEvents
    val sseSubRequest      = generateValidSubscribeRequest(htype)
    val sseSuccessResponse = generateSubscribeResponse(htype, scode, hmsg)

    val actualResponse = SubscribeProcess.buildProcess(sseSubRequest)
    log.info("ActualResponse -->" + Await.result(actualResponse, 1 second))

    val expectedResponse = Future(sseSuccessResponse)
    log.info("ExpectedResponse -->" + Await.result(expectedResponse, 1 second))

    // For BusinessAlerts
    val sseBusinessAlertSubRequest      = generateValidBusinessAlertSubscribeRequest(htype)
    val sseBusinessAlertSuccessResponse = generateValidBusinessAlertSubscribeResponse(htype, scode, hmsg)

    val businessActualResponse = SubscribeProcess.buildBusinessAlertProcess(sseBusinessAlertSubRequest)
    log.info("BusinessAlertActualResponse -->" + Await.result(actualResponse, 1 second))

    val businessExpectedResponse = Future(sseBusinessAlertSuccessResponse)
    log.info("BusinessAlertExpectedResponse -->" + Await.result(expectedResponse, 1 second))

    Await.result(actualResponse, 1 second) mustEqual Await.result(expectedResponse, 1 second)
    Await.result(businessActualResponse, 1 second) mustEqual Await.result(businessExpectedResponse, 1 second)
  }

  "Disconnect" should "validate with valid eventtype `disconnect`" in {
    //For DeviceEvents
    val sseSubRequest      = generateValidSubscribeRequest(dtype)
    val sseSuccessResponse = generateSubscribeResponse(dtype, scode, dmsg)

    val actualResponse = SubscribeProcess.buildProcess(sseSubRequest)
    log.info("ActualResponse -->" + Await.result(actualResponse, 1 second))

    val expectedResponse = Future(sseSuccessResponse)
    log.info("ExpectedResponse -->" + Await.result(expectedResponse, 1 second))

    // For BusinessAlerts
    val sseBusinessAlertSubRequest      = generateValidBusinessAlertSubscribeRequest(dtype)
    val sseBusinessAlertSuccessResponse = generateValidBusinessAlertSubscribeResponse(dtype, scode, dmsg)

    val businessActualResponse = SubscribeProcess.buildBusinessAlertProcess(sseBusinessAlertSubRequest)
    log.info("BusinessAlertActualResponse -->" + Await.result(actualResponse, 1 second))

    val businessExpectedResponse = Future(sseBusinessAlertSuccessResponse)
    log.info("BusinessAlertExpectedResponse -->" + Await.result(expectedResponse, 1 second))

    Await.result(actualResponse, 1 second) mustEqual Await.result(expectedResponse, 1 second)
    Await.result(businessActualResponse, 1 second) mustEqual Await.result(businessExpectedResponse, 1 second)
  }

  "Process" should "validate with invalid eventtype `InvalidType` " in {
    //For DeviceEvents
    val sseSubRequest      = generateValidSubscribeRequest(ftype)
    val sseFailureResponse = generateSubscribeResponse(ftype, fcode, fmsg)

    val actualResponse = SubscribeProcess.buildProcess(sseSubRequest)
    log.info("ActualResponse -->" + Await.result(actualResponse, 1 second))

    val expectedResponse = Future(sseFailureResponse)
    log.info("ExpectedResponse -->" + Await.result(expectedResponse, 1 second))

    //For BusinessAlerts
    val sseBusinessAlertSubRequest      = generateValidBusinessAlertSubscribeRequest(ftype)
    val sseBusinessAlertSuccessResponse = generateValidBusinessAlertSubscribeResponse(ftype, fcode, fmsg)

    val businessActualResponse = SubscribeProcess.buildBusinessAlertProcess(sseBusinessAlertSubRequest)
    log.info("BusinessAlertActualResponse -->" + Await.result(actualResponse, 1 second))

    val businessExpectedResponse = Future(sseBusinessAlertSuccessResponse)
    log.info("BusinessAlertExpectedResponse -->" + Await.result(expectedResponse, 1 second))

    Await.result(actualResponse, 1 second) mustEqual Await.result(expectedResponse, 1 second)
    Await.result(businessActualResponse, 1 second) mustEqual Await.result(businessExpectedResponse, 1 second)
  }
}
