package com.verizon.netsense.service

import java.util.UUID

import akka.actor.ActorSystem
import akka.kafka.ProducerMessage.Message
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.kafka.{ConsumerSettings, ProducerSettings, Subscriptions}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.stream.testkit.TestSubscriber
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestKit
import akka.{Done, NotUsed}
import com.outworkers.phantom.dsl._
import com.verizon.netsense.config._
import com.verizon.netsense.connector.CassandraConnector
import com.verizon.netsense.data.TestData
import com.verizon.netsense.helper.EventWrapper
import com.verizon.netsense.model._
import com.verizon.netsense.test.kafka.Utils.StageStoppingTimeout
import com.verizon.netsense.util.TestKafkaConfig
import com.verizon.netsense.utils.Logging
import com.verizon.netsense.utils.{DeviceModels, ObjectMapperUtil}
import com.vz.nsp.datasample.service.db.{DatabaseSpec, EmbeddedDatabase}
import net.manub.embeddedkafka.{EmbeddedKafka, EmbeddedKafkaConfig}
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer, StringDeserializer}
import org.velvia.MsgPackUtils

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * Created by maidapr on 9/18/17.
  */
class DeviceRequestSpec extends TestKit(ActorSystem("SensorServiceGraphSpec-System"))
  with TestData
  with DatabaseSpec
  with TestSuiteConfig
  with EmbeddedDatabase
  with Logging
  with CassandraConnector.testConnector.Connector {
  implicit val ec = ExecutionContext.Implicits.global

  implicit val embeddedKafkaConfig = EmbeddedKafkaConfig(kafkaPort = 8822, zooKeeperPort = 8833)
  val bootstrapServers = s"localhost:${embeddedKafkaConfig.kafkaPort}"

  implicit val mat = ActorMaterializer()(system)
  implicit val stageStoppingTimeout = StageStoppingTimeout(20.seconds)


  override val siteId = UUID.randomUUID().toString

  val sitename = "Sample Site"
  val nodename = "Sample Node"
  val siteid = siteId
  val nodeid = nodeId
  val street = "900 Chelmsford street"
  val city = "Lowell"
  val state = "MA"
  val zipcode = "01867"
  val country = "USA"
  val latitude = "42.614685"
  val longitude = "-71.324845"
  val timeZone = "America/New_York"
  val nodeModel = "unode-v7"

  def createEventProbe(consumerSettings: ConsumerSettings[Array[Byte], Array[Byte]],
                       topic: String): TestSubscriber.Probe[Array[Byte]] =
    Consumer
      .plainSource(consumerSettings, Subscriptions.topics(topic))
      .map(_.value)
      .runWith(TestSink.probe)

  def createEventProbeString(consumerSettings: ConsumerSettings[String, String],
                             topic: String): TestSubscriber.Probe[String] =
    Consumer
      .plainSource(consumerSettings, Subscriptions.topics(topic))
      .map(_.value)
      .runWith(TestSink.probe)

  object QueryServiceDB extends {

    def init(): Future[List[ResultSet]] = {
      val create = Future.sequence(
        List(
          database.energySavingsNodeTableTest.create.ifNotExists().future(),
          database.energySavingSiteTableTest.create.ifNotExists().future(),
          database.deviceSensorSampleTableTest.create.ifNotExists().future()
        )
      )
      Await.ready(create, 5.seconds)
    }

    def cleanup(): Future[List[ResultSet]] = {
      val truncate = Future.sequence(
        List(
          database.energySavingsNodeTableTest.truncate.future(),
          database.energySavingSiteTableTest.truncate.future(),
          database.deviceSensorSampleTableTest.create.ifNotExists().future()
        )
      )
      Await.ready(truncate, 5.seconds)
    }
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    neo4jInit
    session.init()
    QueryServiceDB.init()
    EmbeddedKafka.start()
  }

  override def afterAll(): Unit = {
    EmbeddedKafka.stop()
    super.afterAll()
    neo4jCleanup()
    system.terminate()
    QueryServiceDB.cleanup()
  }


  def neo4jCleanup(): Unit ={
    val neo4jSession = Neo4jConnection.neo4jDriver.session()

    val siteid = siteId
    val nodeid = nodeId
    val deleteSite = s"MATCH (a:Site {siteid: '$siteid'})" + s" DETACH DELETE a RETURN" + s" { success: true }"
    val deleteNode = s"MATCH (a:Node {nodeid: '$nodeid'})" + s" DETACH DELETE a RETURN" + s" { success: true }"
    try {
      neo4jSession.run(deleteSite)
      neo4jSession.run(deleteNode)
    } catch {
      case ex: Exception => log.error("***** Failed in cleaning up Neo4j DB " + ex)
    } finally {
      neo4jSession.close()
    }
  }

  def neo4jInit = {
    val neo4jSession = Neo4jConnection.neo4jDriver.session()

    def createNode(nodeModel: String = DeviceModels.UNODEV7.toString) =
      s"MERGE (site:Site:Active {siteid: '$siteid'," +
        s" name: '$sitename'," +
        s" street1: '$street'," +
        s" city: '$city', state: '$state', postal_code: '$zipcode', country: '$country'," +
        s" latitude: '$latitude', longitude: '$longitude', time_zone: '$timeZone'})" +
        s"-[:HAS]->(node:Node:Active {nodeid: '$nodeid', name: '$nodename'," +
        s" building: '2', longitude: $longitude, latitude: $latitude, level: 3," +
        s" time_zone: '$timeZone', model: '$nodeModel'})-[:BELONGS_TO]->(site)"

    val nodeCreationQuery = createNode(DeviceModels.UNODEV7.toString)
    log.info("Creating node in graph db " + nodeCreationQuery)

    try {
      neo4jSession.run(nodeCreationQuery)
      log.info("***** Initializing Neo4j DB")
    } catch {
      case ex: Exception => log.error("***** Failed in Initializing Neo4j DB " + ex.getMessage)
    } finally {
      neo4jSession.close()
    }
  }


  trait KafkaConfiguration{
    val partition0 = 0
    val group1 = createGroup

    def createTopic: String = "topic-" + UUID.randomUUID().toString

    def createGroup: String = "group-" + UUID.randomUUID().toString

    def createEventConsumerSettingsString(group: String): ConsumerSettings[String, String] =
      SensorKafkaConnector.configKafkaConsumerSettings(bootstrapServers,
        group,
        new StringDeserializer,
        new StringDeserializer)

    def createEventConsumerSettings(group: String): ConsumerSettings[Array[Byte], Array[Byte]] =
      SensorKafkaConnector.configKafkaConsumerSettings(bootstrapServers,
        group,
        new ByteArrayDeserializer,
        new ByteArrayDeserializer)

    def produceEvent(topic: String, sensorQueryEnvelope: SensorQueryEnvelope): Future[Done] = {
      val source = Source
        .single(sensorQueryEnvelope)
        .map(e => ObjectMapperUtil.toJson(e))
        .map(m => {
          Message(new ProducerRecord(topic, sensorQueryEnvelope.request.requestid.getBytes, m.getBytes()), NotUsed)
        })
        .viaMat(Producer.flow(createEventProducerSettings))(Keep.right)
      source.runWith(Sink.ignore)
    }

    def produceSensorSampleEvent(topic: String, sensorSampleEvent: SensorSampleEvent): Future[Done] = {
      val source = Source
        .single(sensorSampleEvent)
        .map(e => ObjectMapperUtil.toJson(e))
        .map(m => {
          Message(new ProducerRecord(topic, sensorSampleEvent.uuid.getBytes, m.getBytes()), NotUsed)
        })
        .viaMat(Producer.flow(createEventProducerSettings))(Keep.right)
      source.runWith(Sink.ignore)
    }

    def createEventProducerSettings: ProducerSettings[Array[Byte], Array[Byte]] =
      SensorKafkaConnector.configKafkaProducerSettings(bootstrapServers, new ByteArraySerializer, new ByteArraySerializer)
  }

  trait KafkaSettingsQuery extends KafkaConfiguration{
    lazy val coreNodeEventTopic: String = createTopic
    lazy val videoNodeEventTopic: String = createTopic
    lazy val coreNodeCommandTopic: String = createTopic
    lazy val videoNodeCommandTopic: String = createTopic
    lazy val testGroup: String = createGroup
    lazy val requestTopic: String = createTopic
    lazy val responseTopic: String = createTopic
    lazy val responseTopic1: String = responsetopicInRequest

    implicit private val setupKafkaConfig = TestKafkaConfig.setupEmbeddedKafkaConfig(bootstrapServers,
      testGroup,
      coreNodeEventTopic,
      videoNodeEventTopic,
      requestTopic,
      responseTopic,
      coreNodeCommandTopic,
      videoNodeCommandTopic
    )
  }


  "EventWrapper" should "get the node belonging to Site and to fetch the node Model" in {
    val sensorQueryEnvelope = requestQueryWithHeaders
    val futureResult = EventWrapper.fetchNodeTypeSiteTimeZone(sensorQueryEnvelope)
    val modeTimeZone = Await.result(futureResult, remainingOrDefault)
    val expectedTimezoneModel = NodeModelTimeZone(Some(nodeModel), Some(timeZone))
    modeTimeZone.nodeTypeTimeZone mustEqual expectedTimezoneModel

  }


  // FIXME Failing when running with all tests
  ignore should "get events and send request to device to get latest event" in new KafkaSettingsQuery{
    //  "SensorHistoryServiceCommand" should "get events and send request to device to get latest event" in new KafkaSettingsQuery {
    val query = requestQueryWithHeaders
    Await.result(produceEvent(requestTopic, query), remainingOrDefault)
    val queryGraphUnderTest = SensorHistoryQueryService.queryServiceGraph.run()
    val consumerProbe =
      Consumer.plainSource(createEventConsumerSettings(createGroup), Subscriptions.topics(videoNodeCommandTopic)).runWith(TestSink.probe)
    consumerProbe.request(1)
    val consumedCommand = consumerProbe.requestNext(30.seconds)
    val unpackedMap     = MsgPackUtils.unpackMap(consumedCommand.value())
    unpackedMap("uuid") mustEqual requestQueryWithHeaders.request.requestid
    consumerProbe.cancel()

  }


}
