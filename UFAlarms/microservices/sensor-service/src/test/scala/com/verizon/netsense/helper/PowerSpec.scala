/*
package com.verizon.netsense.helper

import java.util.UUID

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import akka.stream.testkit.javadsl.TestSink
import com.verizon.netsense.model._
import com.verizon.netsense.service.{FixtureService, PowerCalculationService}
import akka.testkit.TestKit
import com.outworkers.phantom.dsl.ResultSet
import com.verizon.netsense.config.{Neo4jConnection, TestSuiteConfig}
import com.verizon.netsense.connector.CassandraConnector
import com.verizon.netsense.data.TestData
import com.verizon.netsense.db.DbLayer
import com.verizon.netsense.utils.Logging
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FlatSpecLike, MustMatchers}
import com.vz.nsp.datasample.service.db.{DatabaseSpec, EmbeddedDatabase}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
  * Created by nalamte on 3/25/18.
**/

class PowerSpec extends TestKit(ActorSystem("FixtureTest-System"))
                                with FlatSpecLike
                                with MustMatchers
                                with BeforeAndAfterAll
                                with BeforeAndAfterEach
                                with TestSuiteConfig
                                with EmbeddedDatabase
                                with Logging
                                with CassandraConnector.testConnector.Connector
                                with DatabaseSpec
                                with TestData{

  implicit val mat = ActorMaterializer()(system)
  implicit val ec = system.dispatcher

  override def beforeAll(): Unit = {
    super.beforeAll()
    println("initializing neo4j")
    neo4jInit
    DatabaseService.init()
    session.init()
  }

  override def afterAll(): Unit = {
    super.afterAll()
    println("Cleaning up neo4j")
    neo4jCleanup()
    DatabaseService.cleanup()
  }

  def neo4jInit ={

    lazy val neo4JSession = Neo4jConnection.neo4jDriver.session()
    val initSchema = "CREATE CONSTRAINT ON (n:Node{nodeid:{props}.nodeid}) ASSERT node.nodeid IS UNIQUE;"
    try {
      neo4JSession.run(createFixture)
    } catch {
      case ex: Exception => log.error("***** Failed in Initializing Neo4j DB " + ex)
    } finally {
      neo4JSession.close()
    }
  }

  def neo4jCleanup(): Unit = {
    lazy val neo4JSession = Neo4jConnection.neo4jDriver.session()

    val deleteNode = s"MATCH (a:Node {nodeid: '$fixturenodeid'})" +
      s" DETACH DELETE a RETURN" +
      s" { success: true }"
    try {
      neo4JSession.run(deleteNode)
    } catch {
      case ex: Exception => log.error("Unable to cleanup the graph db " + ex)
    } finally {
      neo4JSession.close()
    }
  }
  object DatabaseService {
    def init(): Future[List[ResultSet]] = {
      val create = Future.sequence(List(database.deviceSensorSampleTableTest.create.ifNotExists().future()))
      Await.ready(create, 5.seconds)
    }

    def cleanup(): Future[List[ResultSet]] = {
      val truncate = Future.sequence(List(database.deviceSensorSampleTableTest.truncate.future()))
      Await.ready(truncate, 5.seconds)
    }
  }

   "PowerSpec" should "write false if there are no p and mP sensors" in {
     // if there are no sensors of type p and mP and units mW in sensor sample event
    val actualResult =  FixtureService.filterPowerSensorValues(sensorSampleEvent)
    val expectedResult = false
    actualResult mustBe expectedResult
   }

  it should "Filter the sensors of type p and mP and units " in {
    // if there are sensors of type p and mP and units mW in sensor sample event(FixturesensorSampleEvent)
   val actualResult =  FixtureService.filterPowerSensorValues(FixturesensorSampleEvent)
   val expectedResult = true
   actualResult mustBe expectedResult
   }

  it should "Validate the power level for over power alarm" in {
    Await.result(database.deviceSensorSampleTableTest.store(FixturesensorSampleEventLT), remainingOrDefault)
    val frb = FixtureQueryRequestBinded(FixturesensorSampleEventOverPower, fixture, driverLevelValue = List((System.currentTimeMillis(), driverLevel)), 1)
    val actual = PowerCalculationService.calculatePower(frb)
    val expectedOutput = List(DeviceAlarm(deviceAlarm,fixturenodeid,overPower,"Critical",s"Consuming more power for Driver Level $driverLevel"),
                              DeviceAlarm(deviceAlarm,fixturenodeid,underPower,"Clear", s"Consuming less power for Driver Level $driverLevel"))
    actual mustBe expectedOutput
  }
  it should "Validate the power level for under power alarm" in {
    Await.result(database.deviceSensorSampleTableTest.store(FixturesensorSampleEventLT), remainingOrDefault)
    val frb = FixtureQueryRequestBinded(FixturesensorSampleEventUnderPower, fixture, driverLevelValue = List((System.currentTimeMillis(), driverLevel)), 1)
    val driverlevelListfromDb = List((starttimeInMillis, driverLevel))
    val actual = PowerCalculationService.calculatePower(frb)
    val expectedOutput = List(DeviceAlarm(deviceAlarm,fixturenodeid,underPower,"Critical", s"Consuming less power for Driver Level $driverLevel"),
                              DeviceAlarm(deviceAlarm,fixturenodeid,overPower,"Clear", s"Consuming more power for Driver Level $driverLevel"))
     actual mustBe expectedOutput

  }

  it should "Validate the power level for Clear power alarm" in {
     Await.result(database.deviceSensorSampleTableTest.store(FixturesensorSampleEventLT), remainingOrDefault)
     val frb = FixtureQueryRequestBinded(FixturesensorSampleEventClear, fixture, driverLevelValue = List((System.currentTimeMillis(), driverLevel)), 1)
     val starttimeInMillis   = System.currentTimeMillis()
     val driverlevelListfromDb = List((starttimeInMillis, driverLevel))
     val actual = PowerCalculationService.calculatePower(frb)
     val expectedResult = List(DeviceAlarm(deviceAlarm,fixturenodeid,"UnderPower","Clear",s"Consuming less power for Driver Level $driverLevel"),
                               DeviceAlarm(deviceAlarm,fixturenodeid,"OverPower","Clear",s"Consuming more power for Driver Level $driverLevel"))
     actual mustBe expectedResult
  }

  it should "Caluculate the sensor power flow" in {

    val queryBuildCombinedflowUnderTest = FixtureService.fixtureQueryFlow
    val sourceMock = Source.single(FixturesensorSampleEvent).map(x => x)
    val probe1 = sourceMock.via(queryBuildCombinedflowUnderTest).runWith(TestSink.probe(system:ActorSystem))
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualResult = consumedElement
    val expectedResult = FixtureQueryRequestBinded(FixturesensorSampleEvent, fixture)
    actualResult.fixture.maxPower0 mustBe   expectedResult.fixture.maxPower0
    actualResult.fixture.maxPower10 mustBe  expectedResult.fixture.maxPower10
    actualResult.fixture.maxPower50 mustBe  expectedResult.fixture.maxPower50
    actualResult.fixture.maxPower100 mustBe expectedResult.fixture.maxPower100
    actualResult.fixture.minPower0 mustBe   expectedResult.fixture.minPower0
    actualResult.fixture.minPower10 mustBe  expectedResult.fixture.minPower10
    actualResult.fixture.minPower50 mustBe  expectedResult.fixture.minPower50
    actualResult.fixture.minPower100 mustBe expectedResult.fixture.minPower100
    actualResult.fixture.nodeType mustBe    expectedResult.fixture.nodeType

  }

  it should "check for recent lt flow" in {
    val currentTime = System.currentTimeMillis()*1000
    Await.result(database.deviceSensorSampleTableTest.store(FixturesensorSampleEventLT), remainingOrDefault)
    val fixtureQueryRequestBinded = FixtureQueryRequestBinded(FixturesensorSampleEventLT, fixture)
    val queryBuildCombinedflowUnderTest = FixtureService.checkForRecentLtValueFlow
    val sourceMock = Source.single(fixtureQueryRequestBinded).map(x => x)
    val probe1 = sourceMock.via(queryBuildCombinedflowUnderTest).runWith(TestSink.probe(system:ActorSystem))
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val result = List((FixturesensorSamplePayload.t,fixtureSensorValue))
    consumedElement.driverLevelValue mustBe result

  }

  it should "get the fixture data from DB" in {
    import scala.concurrent.duration._
    val actualGetFixtureData = DbLayer().getFixtureForNode(fixturenodeid)
    val actualData = Await.result(actualGetFixtureData, 5.seconds)
    val expectedOutPut = Future{fixture}
    val exp = Await.result(expectedOutPut, 5.seconds)
    actualData mustBe exp
  }

  it should "Check if the value is with in the range" in {
    val currentTime = System.currentTimeMillis()*1000
    val input = FixtureQueryRequestBinded(FixturesensorSampleEvent, fixture, driverLevelValue = List((System.currentTimeMillis(), driverLevel)), 1)
    val sourceMock = Source.single(input).map(x => x)
    val rangeCheck = FixtureService.calculatePowerAndCheckValueWithInRange
    val probe1 = sourceMock.via(rangeCheck).runWith(TestSink.probe(system:ActorSystem))
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val expectedOutput = List(DeviceAlarm(deviceAlarm,fixturenodeid,overPower,"Critical", s"Consuming more power for Driver Level $driverLevel"),
                              DeviceAlarm(deviceAlarm,fixturenodeid,underPower,"Clear", s"Consuming less power for Driver Level $driverLevel"))
    consumedElement mustBe expectedOutput
  }
}
*/
