package com.verizon.netsense.service

/**
  * Created by nalamte on 2/18/18.
  */
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestKit
import com.outworkers.phantom.dsl.ResultSet
import com.verizon.netsense.config.{Neo4jConnection, TestSuiteConfig}
import com.verizon.netsense.connector.CassandraConnector
import com.verizon.netsense.data.TestData
import com.verizon.netsense.model.SensorSampleQueryResponse
import com.verizon.netsense.utils.{DeviceModels, Logging, ObjectMapperUtil}
import com.vz.nsp.datasample.service.db.EmbeddedDatabase
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FlatSpecLike, MustMatchers}

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._


class SensorSampleQueryFlowTest  extends TestKit(ActorSystem("QueryFlowTest-System"))
  with FlatSpecLike
  with MustMatchers
  with BeforeAndAfterAll
  with BeforeAndAfterEach
  with TestSuiteConfig
  with EmbeddedDatabase
  with Logging
  with CassandraConnector.testConnector.Connector
  with TestData {

  implicit val ec = ExecutionContext.Implicits.global

  implicit val mat = ActorMaterializer()(system)

  object QueryServiceDB extends {

    implicit val session = CassandraConnector.testConnector.session
    implicit val executor = system.dispatcher

    def init(): Future[List[ResultSet]] = {
      val create =
        Future.sequence(List(database.deviceSensorSampleTableTest.create.ifNotExists().future()(session, executor)))
      Await.ready(create, 5.seconds)
    }

    def cleanup(): Future[List[ResultSet]] = {
      val truncate = Future.sequence(
        List(
          database.deviceSensorSampleTableTest.truncate.future()(session, executor))
      )
      Await.ready(truncate, 5.seconds)
    }
  }

  val sitename = "Sample Site"
  val nodename = "Sample Node"
  val street = "900 Chelmsford street"
  val city = "Lowell"
  val state = "MA"
  val zipcode = "01867"
  val country = "USA"
  val latitude = "42.614685"
  val longitude = "-71.324845"
  val timeZone = "America/New_York"
  val nodeModel = "unode-v7"
  val sensorid = "a"
  val sensorid2 = "b"
  val sensorid3 = "c"


  def neo4jInit = {
    val neo4jSession = Neo4jConnection.neo4jDriver.session()

    def createNode(nodeModel: String = DeviceModels.UNODEV7.toString) =
      s"MERGE (site:Site:Active {siteid: '$siteId'," +
        s" name: '$sitename'," +
        s" street1: '$street'," +
        s" city: '$city', state: '$state', postal_code: '$zipcode', country: '$country'," +
        s" latitude: '$latitude', longitude: '$longitude', time_zone: '$timeZone'})" +
        s"-[:HAS]->(node:Node:Active {nodeid: '$nodeId', name: '$nodename'," +
        s" building: '2', longitude: $longitude, latitude: $latitude, level: 3," +
        s" time_zone: '$timeZone', model: '$nodeModel'})-[:BELONGS_TO]->(site)"

    val nodeCreationQuery = createNode(DeviceModels.UNODEV7.toString)
    log.info("Creating node in graph db " + nodeCreationQuery)

    try {
      neo4jSession.run(nodeCreationQuery)
      log.info("***** Initializing Neo4j DB")
    } catch {
      case ex: Exception => log.error("***** Failed in Initializing Neo4j DB " + ex.getMessage)
        neo4jSession.close()
    }
  }

  def neo4jCleanup(): Unit = {
    lazy val neo4JSession = Neo4jConnection.neo4jDriver.session()

    val deleteSite = s"MATCH (a:Site {siteid: '$siteId'})" +
      s" DETACH DELETE a RETURN" +
      s" { success: true }"
    val deleteNode = s"MATCH (a:Node {nodeid: '$nodeId'})" +
      s" DETACH DELETE a RETURN" +
      s" { success: true }"
    try {
      neo4JSession.run(deleteSite)
      neo4JSession.run(deleteNode)
    } catch {
      case ex: Exception => log.error("Unable to cleanup the graph db " + ex)
    } finally {
      neo4JSession.close()
    }
  }

  override def beforeAll(): Unit = {
    neo4jInit
    super.beforeAll()
    session.init()
    QueryServiceDB.init()
  }

  override def afterAll(): Unit = {
    neo4jCleanup()
    super.afterAll()
    system.terminate()
    QueryServiceDB.cleanup()
  }

  "QueryFlowTest" should "return two datapoints for multiple sensor query" in {
    Thread.sleep(1000) //To load neo4j data. To be solved later
    val sensorid = "a"
    val sensorid2 = "b"
    val sampleStore1 = database.deviceSensorSampleTableTest.store(buildSensorPayload(sensorid))
    val sampleStore2 = database.deviceSensorSampleTableTest.store(buildSensorPayload(sensorid2))
    Await.result(sampleStore1, remainingOrDefault)
    Await.result(sampleStore2, remainingOrDefault)
    val query = generateQueryWithHeaders(nodeId, sensorid + "," + sensorid2)
    val queryBuildCombinedflowUnderTest = SensorHistoryQueryService.sensorSampleQueryFlow
    val sourceMock = Source.single(query).map(x => x)
    val probe1 = sourceMock.via(queryBuildCombinedflowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val result: SensorSampleQueryResponse = ObjectMapperUtil.fromJson[SensorSampleQueryResponse](consumedElement._2)
    result.response.sensorid mustEqual sensorid + "," + sensorid2
    result.response.items.size mustEqual 2
  }

  "QueryFlowTest" should "return single datapoint for single sensor query" in {
    val sensorid = "c"
    val sampleStore1 = database.deviceSensorSampleTableTest.store(buildSensorPayload(sensorid))
    Await.result(sampleStore1, remainingOrDefault)
    val query = generateQueryWithHeaders(nodeId, sensorid)
    val queryBuildCombinedflowUnderTest = SensorHistoryQueryService.sensorSampleQueryFlow
    val sourceMock = Source.single(query).map(x => x)
    val probe1 = sourceMock.via(queryBuildCombinedflowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val result: SensorSampleQueryResponse = ObjectMapperUtil.fromJson[SensorSampleQueryResponse](consumedElement._2)
    result.response.sensorid mustEqual sensorid
    result.response.items.size mustEqual 1
  }

  "QueryFlowTest" should "return zero datapoints for single sensor query(with no data in database)" in {
    val sensorid = "d"
    val query = generateQueryWithHeaders(nodeId, sensorid)
    val queryBuildCombinedflowUnderTest = SensorHistoryQueryService.sensorSampleQueryFlow
    val sourceMock = Source.single(query).map(x => x)
    val probe1 = sourceMock.via(queryBuildCombinedflowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val result: SensorSampleQueryResponse = ObjectMapperUtil.fromJson[SensorSampleQueryResponse](consumedElement._2)
    result.response.sensorid mustEqual sensorid
    result.response.items.size mustEqual 0
  }

  "QueryFlowTest" should "return two datapoints for three sensor query(only two datapoints in DB)" in {
    val sensorid = "e"
    val sensorid2 = "f"
    val sensorid3 = "g"
    val sampleStore1 = database.deviceSensorSampleTableTest.store(buildSensorPayload(sensorid))
    val sampleStore2 = database.deviceSensorSampleTableTest.store(buildSensorPayload(sensorid2))
    Await.result(sampleStore1, remainingOrDefault)
    Await.result(sampleStore2, remainingOrDefault)
    val query = generateQueryWithHeaders(nodeId, sensorid + "," + sensorid2 + "," + sensorid3)
    val queryBuildCombinedflowUnderTest = SensorHistoryQueryService.sensorSampleQueryFlow
    val sourceMock = Source.single(query).map(x => x)
    val probe1 = sourceMock.via(queryBuildCombinedflowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val result: SensorSampleQueryResponse = ObjectMapperUtil.fromJson[SensorSampleQueryResponse](consumedElement._2)
    result.response.sensorid mustEqual sensorid + "," + sensorid2 + "," +sensorid3
    result.response.items.size mustEqual 2
  }

  "QueryFlowTest" should "return zero datapoints for two sensor query(with no data in database)" in {
    val sensorid = "h"
    val sensorid2 = "i"
    val query = generateQueryWithHeaders(nodeId, sensorid + "," + sensorid2)
    val queryBuildCombinedflowUnderTest = SensorHistoryQueryService.sensorSampleQueryFlow
    val sourceMock = Source.single(query).map(x => x)
    val probe1 = sourceMock.via(queryBuildCombinedflowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val result: SensorSampleQueryResponse = ObjectMapperUtil.fromJson[SensorSampleQueryResponse](consumedElement._2)
    result.response.sensorid mustEqual sensorid + "," + sensorid2
    result.response.items.size mustEqual 0
  }
}