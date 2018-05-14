package com.verizon.netsense.helper

import java.time.{Instant, ZoneId}

import akka.stream.ActorMaterializer
import com.verizon.netsense.config.{Neo4jConnection, TestSuiteConfig}
import com.verizon.netsense.data.TestData
import com.verizon.netsense.model._
import com.verizon.netsense.util.BaseSpec
import com.verizon.netsense.utils.{DeviceModels, Logging}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
 * Created by maidapr on 6/8/17.
 */
class EventWrapperSpec extends BaseSpec with TestSuiteConfig with TestData with Logging {

  private implicit val ec = scala.concurrent.ExecutionContext.Implicits.global


  override def beforeAll(): Unit = {
    super.beforeAll()
    println("initializing neo4j")
    neo4jInit
  }

  override def afterAll(): Unit = {
    super.afterAll()
    println("Cleaning up neo4j")
    neo4jCleanup()
  }

  def neo4jCleanup(): Unit = {
    val siteid = siteId
    val nodeid = nodeId
    lazy val neo4JSession = Neo4jConnection.neo4jDriver.session()

    val deleteSite = s"MATCH (a:Site {siteid: '$siteid'})" +
      s" DETACH DELETE a RETURN" +
      s" { success: true }"
    val deleteNode = s"MATCH (a:Node {nodeid: '$nodeid'})" +
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

  def neo4jInit = {
    lazy val neo4JSession = Neo4jConnection.neo4jDriver.session()

    val sitename = "Sample Site"
    val nodename = "Sample Node"
    val siteid = "ubserSite"
    val nodeid = "uberNode"
    val street = "900 Chelmsford street"
    val city = "Lowell"
    val state = "MA"
    val zipcode = "01867"
    val country = "USA"
    val latitude = "42.614685"
    val longitude = "-71.324845"
    val timezone = "America/New_York"
    val nodetype = "unode-v7"
    val initSiteSchema = "CREATE CONSTRAINT ON (site:Site) ASSERT site.siteid IS UNIQUE;"
    val createSite = s"MERGE (site:Site:Active {siteid: '$siteid', name: '$sitename', street1: '$street'," +
      s" city: '$city', state: '$state', postal_code: '$zipcode', country: '$country'," +
      s" latitude: '$latitude', longitude: '$longitude', time_zone: '$timezone'})"

    val createNode = s"MERGE (site)-[:HAS]->(node:Node:Active {nodeid: '$nodeid', name: '$nodename'," +
      s" building: '2', longitude: $longitude, latitude: $latitude, level: 3," +
      s" time_zone: '$timezone', model: '$nodetype'})-[:BELONGS_TO]->(site)"
    try {
      neo4JSession.run(createSite)
      neo4JSession.run(createNode)
    } catch {
      case ex: Exception => log.error("***** Failed in Initializing Neo4j DB " + ex)
    }
    finally {
      neo4JSession.close()
    }
  }

  "EventWrapper" should "Convert sensor value with the timestamp" in {
    val timeZoneName = "GMT-4"
    val convertSensorValueWithTimeStampValue = EventWrapper.convertSensorValueWithTimeStamp(sensorSamplePayloadTuple,
                                                                                            requestQueryWithHeaders,
                                                                                            ZoneId.of(timeZoneName), sensorId = sensorId)
    val expectedOutput =
      SensorSamplePayload(sensorId,Instant
                            .ofEpochMilli(sensorSamplePayloadTuple._1.toLong / 1000)
                            .atZone(ZoneId.of(timeZoneName))
                            .format(ISODateFormat),
                          sensorValue)
    convertSensorValueWithTimeStampValue mustBe expectedOutput
  }

  it should "build the Sensor Sample response payload " in {
    val listOfFutures = Future[List[List[(Long, Double)]]] {
      List(List(sensorSamplePayloadTuple))
    }
    val expectedOutput = Future[SensorSampleQueryResponse] {
      SensorSampleQueryResponse(messageId, SampleQueryResponseSingleSensor)
    }
    val nodeModelTimeZone    = NodeModelTimeZone(Some(DeviceModels.UNODEV4.toString), timeZone = Some("UTC"))
    val buildResponsePayload = EventWrapper.buildResponsePayload((requestQueryWithHeaders, nodeModelTimeZone), listOfFutures)
    val obtainedResult       = Await.result(buildResponsePayload, 10.seconds)
    val expectedResult       = Await.result(expectedOutput, 10.seconds)
    println("Obtained" +(obtainedResult._2))
    println("expected" +(expectedResult))
    obtainedResult._2 mustBe expectedResult

  }

  it should "build the Node response payload " in {

    val listOfFutures = Future[List[(String, String, String, Double, Double, Double, Double, Double)]] {
      List(EnergySavingsNodePayloadItem.unapply(sEnergySavingNodePayloadItem).get)
    }

    val sENResponse: SensorEnergyNodePayload = energyNodeResponsePayload

    val expectedOutput = (requestQueryWithHeaders, SensorEnergyNodeResponse(messageId, sENResponse))

    val buildNodeResponsePayload: Future[(SensorQueryEnvelope, SensorEnergyNodeResponse)] =
      EventWrapper.buildNodeResponsePayload(requestQueryWithHeaders, listOfFutures)
    val obtainedResult = Await.result(buildNodeResponsePayload, 10.seconds)

    obtainedResult mustBe expectedOutput
  }

  it should "build the Site response payload " in {
    val listOfFutures = Future[List[(String, String, Double, Double, Double, Double, Double)]] {
      List(EnergySavingsSitePayloadItem.unapply(sEnergySavingSitePayloadItem).get)
    }
    val sESResponse: SensorEnergySitePayload = EnergySiteResponsePayload
    val expectedOutput                       = (requestQueryWithHeaders, SensorEnergySiteResponse(messageId, sESResponse))
    val buildSiteResponsePayload: Future[(SensorQueryEnvelope, SensorEnergySiteResponse)] =
      EventWrapper.buildSiteResponsePayload(requestQueryWithHeaders, listOfFutures)
    val obtainedResult = Await.result(buildSiteResponsePayload, 10.seconds)
    obtainedResult mustBe expectedOutput
  }
}
