package com.verizon.netsense.sse.specs.service

import akka.stream.Supervision
import com.outworkers.phantom.connectors.CassandraConnection
import com.outworkers.phantom.dsl._
import com.verizon.netsense.sse.service.{BaseSSEService, SSEStreamHelper}
import com.verizon.netsense.sse.specs.data.TestData
import com.verizon.netsense.sse.specs.db.CassandraSpec
import com.verizon.netsense.sse.specs.phantom.PhantomServiceSpec
import com.verizon.netsense.utils.Logging
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, MustMatchers}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.control.NonFatal

/**
 * Created by muppasw on 9/19/17.
 */
class SSEStreamHelperSpec
    extends FlatSpecLike
    with PhantomServiceSpec
    with CassandraSpec
    with MustMatchers
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

  override def kafkaClientId: String = ???

  override def kafkaGroupId: String = ???

  override def kafkaOffsetReset: String = ???

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

  "Data" should "get data based on nodeid to enrich the data" in {
    val chain = for {
      get <- databaseTest.orgtabletest.insertOrgEvent(orgEvent)
      get <- databaseTest.orgtabletest.getbyNodeID(nodeId)
    } yield get
    whenReady(chain) { res =>
      res mustBe Option(s"$nodeId", s"$orgId", s"$siteId")
      log.info("Result:::" + res)
    }
  }

  "InsertSSEFilter" should "insert the data in ssefilter table" in {
    val chain = for {
      get <- databaseTest.ssefiltertest.insertSSEFilter(generateValidSubscribeRequest(stype),
                                                        sensorTopicname,
                                                        StreamTtl,
                                                        count)
    } yield get
    whenReady(chain) { res =>
      log.info("Data from org table -->" + res)
      res isExhausted () mustBe true
    }
  }

  "UpdateSSEFilter" should "update the ttl in ssefilter table" in {
    val chain = for {
      get <- databaseTest.ssefiltertest.updateSSEFilterHeartBeat(generateValidSubscribeRequest(htype),
                                                                 sensorTopicname,
                                                                 count,
                                                                 StreamTtl)
      get <- databaseTest.sseBusinessAlertFilterEventtest.updateBussinessAlertSSEFilterHeartBeat(
        generateValidBusinessAlertSubscribeRequest(htype),
        businessTopicname,
        StreamTtl,
        count
      )
    } yield get
    whenReady(chain) { res =>
      log.info("Data from org table -->" + res)
      res isExhausted () mustBe true
    }
  }

  "DeleteSSEFilter" should "delete the data in ssefilter table" in {
    val chain = for {
      get <- databaseTest.ssefiltertest.deleteSSEFilter(generateValidSubscribeRequest(dtype))
      get <- databaseTest.sseBusinessAlertFilterEventtest.deleteBussinessAlertSSEFilter(
        generateValidBusinessAlertSubscribeRequest(dtype)
      )
    } yield get
    whenReady(chain) { res =>
      log.info("Data from org table -->" + res)

      res isExhausted () mustBe true
    }
  }

  "Subscribe" should "store data in ssefilter table with valid eventtype `subscribe`" in {

    val sseSubRequest  = generateValidSubscribeRequest(stype)
    val actualResponse = SSEStreamHelper.sseSubscribe(sseSubRequest)
    log.info("ActualResponse -->" + Await.result(actualResponse, 1 second))

    val sseBusinessSubRequest       = generateValidBusinessAlertSubscribeRequest(stype)
    val businessAlertactualResponse = SSEStreamHelper.sseBusinessAlertSubscribe(sseBusinessSubRequest)
    log.info("BusinessAlert ActualResponse -->" + Await.result(businessAlertactualResponse, 1 second))

    Await.result(actualResponse, 1 second) isExhausted () mustBe true
    Await.result(businessAlertactualResponse, 1 second) isExhausted () mustBe true
  }

  "Heartbeat" should "update data in ssefilter table with valid eventtype `heartbeat`" in {

    val sseSubRequest  = generateValidSubscribeRequest(htype)
    val actualResponse = SSEStreamHelper.sseHeartBeat(sseSubRequest)
    log.info("ActualResponse -->" + Await.result(actualResponse, 1 second))

    val sseBusinessSubRequest       = generateValidBusinessAlertSubscribeRequest(htype)
    val businessAlertactualResponse = SSEStreamHelper.sseBusinessAlertHeartBeat(sseBusinessSubRequest)
    log.info("BusinessAlert ActualResponse -->" + Await.result(businessAlertactualResponse, 1 second))

    Await.result(actualResponse, 1 second) isExhausted () mustBe true
    Await.result(businessAlertactualResponse, 1 second) isExhausted () mustBe true
  }

  "Disconnect" should "delete data in ssefilter table with valid eventtype `disconnect`" in {
    val sseSubRequest  = generateValidSubscribeRequest(dtype)
    val actualResponse = SSEStreamHelper.sseDisconnect(sseSubRequest)
    log.info("ActualResponse -->" + Await.result(actualResponse, 1 second))

    val sseBusinessSubRequest       = generateValidBusinessAlertSubscribeRequest(dtype)
    val businessAlertactualResponse = SSEStreamHelper.sseBusinessAlertDisconnect(sseBusinessSubRequest)
    log.info("BusinessAlert ActualResponse -->" + Await.result(businessAlertactualResponse, 1 second))

    Await.result(actualResponse, 1 second) isExhausted () mustBe true
    Await.result(businessAlertactualResponse, 1 second) isExhausted () mustBe true
  }
}
