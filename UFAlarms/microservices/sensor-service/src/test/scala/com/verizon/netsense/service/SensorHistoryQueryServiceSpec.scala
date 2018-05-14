package com.verizon.netsense.service

import java.time.ZoneId
import java.util.UUID

import akka.actor.ActorSystem
import akka.kafka.ProducerMessage.Message
import akka.kafka.scaladsl.Consumer.Control
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.kafka.{ConsumerSettings, ProducerSettings, Subscriptions}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.stream.testkit.TestSubscriber
import akka.stream.testkit.TestSubscriber.Probe
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestKit
import akka.{Done, NotUsed}
import com.outworkers.phantom.dsl.ResultSet
import com.verizon.netsense.config._
import com.verizon.netsense.connector.CassandraConnector
import com.verizon.netsense.data.TestData
import com.verizon.netsense.helper.{Common, SensorSampleMsgPackConverter}
import com.verizon.netsense.model._
import com.verizon.netsense.util.TestKafkaConfig
import com.verizon.netsense.utils.{DeviceModels, Logging, ObjectMapperUtil, SensorValueConverter}
import com.vz.nsp.datasample.service.db.EmbeddedDatabase
import net.manub.embeddedkafka.{EmbeddedKafka, EmbeddedKafkaConfig}
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer, StringDeserializer}
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FlatSpecLike, MustMatchers}
import org.velvia.MsgPackUtils

import scala.concurrent.duration.{FiniteDuration, _}
import scala.concurrent.{Await, ExecutionContext, Future}

/**
 * Created by nalamte on 6/27/17.
  **/
class SensorHistoryQueryServiceSpec
    extends TestKit(ActorSystem("QueryServiceIntegrationSpec-System"))
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

  implicit val embeddedKafkaConfig = EmbeddedKafkaConfig(kafkaPort = 8767, zooKeeperPort = 8768)
  val bootstrapServers             = s"localhost:${embeddedKafkaConfig.kafkaPort}"

  implicit val mat                  = ActorMaterializer()(system)
  implicit val stageStoppingTimeout = StageStoppingTimeout(20.seconds)

  def createEventProbe(consumerSettings: ConsumerSettings[Array[Byte], Array[Byte]],
                       topic: String): TestSubscriber.Probe[Array[Byte]] =
    Consumer.plainSource(consumerSettings, Subscriptions.topics(topic)).map(_.value).runWith(TestSink.probe)

  def createEventConsumerSettings(group: String): ConsumerSettings[Array[Byte], Array[Byte]] =
    SensorKafkaConnector.configKafkaConsumerSettings(bootstrapServers,
                                               group,
                                               new ByteArrayDeserializer,
                                               new ByteArrayDeserializer)

  object QueryServiceDB extends {

    implicit val ec       = scala.concurrent.ExecutionContext
    implicit val session  = CassandraConnector.testConnector.session
    implicit val executor = system.dispatcher

    def init(): Future[List[ResultSet]] = {
      val create =
        Future.sequence(List(database.deviceSensorSampleTableTest.create.ifNotExists().future()(session, executor)))
      Await.ready(create, 5.seconds)
    }

    def cleanup(): Future[List[ResultSet]] = {
      val truncate = Future.sequence(
        List(database.deviceSensorSampleTableTest.truncate.future()(session, executor),
             database.deviceSensorSampleTableTest.truncate.future()(session, executor))
      )
      Await.ready(truncate, 5.seconds)
    }
  }

  override def beforeAll(): Unit = {
    neo4jInit
    super.beforeAll()
    session.init()
    QueryServiceDB.init()
    if (EmbeddedKafka.isRunning) {
      println("Kafka still running");
    } else {
      println("Spinning Embedded kafka");
      EmbeddedKafka.start()
    }
  }

  override def afterAll(): Unit = {
    EmbeddedKafka.stop()
    neo4jCleanup()
    super.afterAll()
    system.terminate()
     QueryServiceDB.cleanup()
  }



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


  def neo4jCleanup(): Unit ={
    val neo4jSession = Neo4jConnection.neo4jDriver.session()

    val siteid = siteId
    val nodeid = nodeId
    val deleteSite = s"MATCH (a:Site {siteid: '$siteid'})" + s" DETACH DELETE a RETURN" + s" { success: true }"
    val deleteNode = s"MATCH (a:Node {nodeid: '$nodeid'})" + s" DETACH DELETE a RETURN" + s" { success: true }"
    neo4jSession.run(deleteSite)
    neo4jSession.run(deleteNode)
    neo4jSession.close()
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
        neo4jSession.close()
    }
  }

  trait KafkaConfiguration {
    val partition0 = 0
    val group1     = createGroup

    def createTopic: String = "topic-" + UUID.randomUUID().toString

    def createGroup: String = "group-" + UUID.randomUUID().toString

    def createEventConsumerSettingsString(group: String): ConsumerSettings[String, String] =
      SensorKafkaConnector.configKafkaConsumerSettings(bootstrapServers,
                                                 group,
                                                 new StringDeserializer,
                                                 new StringDeserializer)

    def createEventConsumerSettings(group: String): ConsumerSettings[Array[Byte], Array[Byte]] =
      SensorKafkaConnector
        .configKafkaConsumerSettings(bootstrapServers, group, new ByteArrayDeserializer, new ByteArrayDeserializer)

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

    def createEventProducerSettings: ProducerSettings[Array[Byte], Array[Byte]] =
      SensorKafkaConnector
        .configKafkaProducerSettings(bootstrapServers, new ByteArraySerializer, new ByteArraySerializer)

  }

  trait KafkaSettingsQuery extends KafkaConfiguration with Common {
    lazy val coreNodeEventTopic: String     = createTopic
    lazy val videoNodeEventTopic: String    = createTopic
    lazy val commandTopic: String           = createTopic
    lazy val testGroup: String              = createGroup
    lazy val requestTopic: String           = createTopic
    lazy val responseTopic: String          = createTopic

    private implicit val setupKafkaConfig = TestKafkaConfig.setupEmbeddedKafkaConfig(bootstrapServers,
                                                                                 testGroup,
                                                                                 coreNodeEventTopic,
                                                                                 videoNodeEventTopic,
                                                                                 requestTopic,
                                                                                 responseTopic,
                                                                                 commandTopic)
  }

  final case class StageStoppingTimeout(time: FiniteDuration)

  "Query Service" should "start the embedded kafka & close the probe" in new KafkaSettingsQuery {

    val probe = createEventProbe(createEventConsumerSettings(testGroup), videoNodeEventTopic)
    probe.ensureSubscription()
    probe.cancel()
  }

  it should "produce a Sensor sample messages onto embedded kafka & can consume it" in new KafkaSettingsQuery {

    val testEvent = generateQueryWithHeaders()
    Await.result(produceEvent(videoNodeEventTopic, testEvent), remainingOrDefault)
    val consumerSettings = createEventConsumerSettings(testGroup)
    val probe            = createEventProbe(consumerSettings, videoNodeEventTopic)
    probe.request(1)
    val consumedMsg1 = probe.expectNext()
    val unpackedMessage =
      Await.result(ObjectMapperUtil.fromJsonByteArrayAsync[SensorQueryEnvelope](consumedMsg1), remainingOrDefault)
    unpackedMessage.get must equal(testEvent)
    probe.cancel()
  }

  it should "consumes the messages from kafka consumer flow" in new KafkaSettingsQuery {

    val requestQuery = generateQueryWithHeaders()
    Await.result(produceEvent(videoNodeEventTopic, requestQuery), remainingOrDefault)
    val consumerUnderTest =
      SensorHistoryQueryService.configKafkaSource(createEventConsumerSettings(testGroup), Set(videoNodeEventTopic))

    def createEventProbe: TestSubscriber.Probe[Array[Byte]] =
      consumerUnderTest.map(record => record.value).runWith(TestSink.probe)

    val probe = createEventProbe
    probe.request(1)
    val consumedMsg = probe.requestNext(10.second)
    val convertedMsg =
      Await.result(ObjectMapperUtil.fromJsonByteArrayAsync[SensorQueryEnvelope](consumedMsg), remainingOrDefault)
    convertedMsg.get must equal(requestQuery)
    probe.cancel()
  }

  it should "make the message flow thru Json unwrap flowShape & handle the unexpected event" in new KafkaSettingsQuery {

    val testEvent = generateQueryWithHeaders()
    Await.result(produceEvent(videoNodeEventTopic, testEvent), remainingOrDefault)
    val consumerUnderTest: Source[ConsumerRecord[String, String], Control] =
      SensorHistoryQueryService.configKafkaSource(createEventConsumerSettingsString(testGroup), Set(videoNodeEventTopic))
    val flowUnderTest = SensorHistoryQueryService.msgUnmarshallingFlow
    val testSink      = TestSink.probe[SensorQueryEnvelope]

    def createEventProbe: Probe[SensorQueryEnvelope] = consumerUnderTest.via(flowUnderTest).runWith(testSink)

    val probe = createEventProbe
    probe.request(1)
    val consumedMsg = probe.requestNext(10.second)
    consumedMsg must equal(testEvent)
    probe.cancel()
  }

 ignore should "Consume Sensor sample messages and can produce the sensor history response" +
  " to response topic from request envelope" in new KafkaSettingsQuery {

    val sensorTypeForTest = "p"
    val sensorSamplePayloadForSensor: SensorPayload =
      SensorPayload(n = nodeId, u = sensorUnits, s = sensorTypeForTest, v = sensorValue, t = starttimeInMicros)

    val sensorSampleEventForSensorType: SensorSampleEvent = SensorSampleEvent(UUID.randomUUID().toString,
                                                                              nodeId,
                                                                              unsolEventType,
                                                                              sensorTypeForTest,
                                                                              eventPath,
                                                                              sensorSamplePayloadForSensor)

    // produce an event to the topic random topic
    val sampleStore = database.deviceSensorSampleTableTest.store(sensorSampleEventForSensorType)

    Await.result(sampleStore, remainingOrDefault)

    val requestQuery = generateQueryWithHeaders(nodeId, sensorTypeForTest)
    Await.result(produceEvent(videoNodeEventTopic, requestQuery), remainingOrDefault)

    // Consuming the event from the topic "SensorTestTopic"
    val consumerUnderTest = SensorHistoryQueryService
      .configKafkaSource(createEventConsumerSettingsString(testGroup), Set(videoNodeEventTopic))

    val unMarshallingFlowUnderTest = SensorHistoryQueryService.msgUnmarshallingFlow
    //    val deviceEvent: SensorPayload = SensorPayload(n = nodeId, s = sensorId, v = sensorValue, t = starttime)
    val queryBuilderFlowUnderTest = SensorHistoryQueryService.sensorQueryResponseBuilderFlow
    val queryBuildCombinedflowUnderTest =
      SensorHistoryQueryService.sensorSampleQueryFlow

    val producerRecordBuilderUnderTest = SensorHistoryQueryService.responseKafkaFlow
    val kafkaSinkUnderTest = SensorHistoryQueryService.responseKafkaSink
    consumerUnderTest
      .via(unMarshallingFlowUnderTest)
      .via(SensorHistoryQueryService.messageValidationFlow)
      .via(queryBuildCombinedflowUnderTest)
      .via(producerRecordBuilderUnderTest)
      .toMat(kafkaSinkUnderTest)(Keep.both)
      .run()

    val sensorConsumerUnderTest = SensorHistoryQueryService
      .configKafkaSource(createEventConsumerSettingsString(testGroup), Set(requestQuery.responsetopic))

    val sensorCommandConsumerUnderTest = SensorHistoryQueryService
      .configKafkaSource(createEventConsumerSettings(testGroup), Set(commandTopic))

    val probeForQueryResponse = sensorConsumerUnderTest.toMat(TestSink.probe)(Keep.right).run()
//    val probeForCommand       = sensorCommandConsumerUnderTest.toMat(TestSink.probe)(Keep.right).run()
    probeForQueryResponse.request(1)
    val consumedMsg   = probeForQueryResponse.requestNext(30.seconds)
    val receivedEvent: SensorSampleQueryResponse = ObjectMapperUtil.fromJson[SensorSampleQueryResponse](consumedMsg.value())
   /* probeForCommand.request(1)
    val consumedCommand = probeForCommand.requestNext(30.seconds)
    val unpackedMap     = MsgPackUtils.unpackMap(consumedCommand.value())
    val sensorSampleRequestMsgPacked = convertCASELtoRestPackForVideoNode(requestQuery,requestQuery.request.extprops.sensorid).asInstanceOf[SensorSampleRequest]
    val sensorRequestPayloadMsgPacked = SensorSampleProps(s = sensorTypeForTest,
      t = convertISOToMicros(requestQuery.request.timestamp)).toMsgPack

    unpackedMap("uuid") mustEqual requestQuery.request.requestid
    unpackedMap("sid") mustEqual sensorSampleRequestMsgPacked.sid
    unpackedMap("l") mustEqual sensorRequestPayloadMsgPacked
*/
//    println("Hello: " + convertMicrosToISO(sensorSamplePayloadForSensor.t) + " and " + convertMicrosToISO(sensorSamplePayloadForSensor.t) + ", " + receivedEvent.response.items.head.time)
    receivedEvent.response.items mustEqual List(
      SensorSamplePayload(sensorId, value = sensorValue, time = convertMicrosToISO(sensorSamplePayloadForSensor.t,ZoneId.of("America/New_York")))
    )
    probeForQueryResponse.cancel()
//    probeForCommand.cancel()
  }


  ignore should "consume the request for Sensor history, construct sensor request from query and " +
    " publish the sensor request to command topic" in new KafkaSettingsQuery {

    val sensorTypeForTest = "p"

    val requestQuery = generateQueryWithHeaders(nodeId, sensorTypeForTest)
    Await.result(produceEvent(videoNodeEventTopic, requestQuery), remainingOrDefault)

    // Consuming the event from the topic "SensorTestTopic"
    val consumerUnderTest = SensorHistoryQueryService
      .configKafkaSource(createEventConsumerSettingsString(testGroup), Set(videoNodeEventTopic))

    val unMarshallingFlowUnderTest = SensorHistoryQueryService.msgUnmarshallingFlow
    val queryBuilderFlowUnderTest = SensorHistoryQueryService.sensorQueryResponseBuilderFlow
    val queryBuildCombinedflowUnderTest =
      SensorHistoryQueryService.sensorSampleQueryFlow

    val producerRecordBuilderUnderTest = SensorHistoryQueryService.responseKafkaFlow
    val kafkaSinkUnderTest = SensorHistoryQueryService.responseKafkaSink
    consumerUnderTest
      .via(unMarshallingFlowUnderTest)
      .via(queryBuildCombinedflowUnderTest)
      .via(producerRecordBuilderUnderTest)
      .toMat(kafkaSinkUnderTest)(Keep.both)
      .run()

    val sensorCommandConsumerUnderTest = SensorHistoryQueryService
      .configKafkaSource(createEventConsumerSettings(testGroup), Set(commandTopic))

    val probeForCommand       = sensorCommandConsumerUnderTest.toMat(TestSink.probe)(Keep.right).run()

    probeForCommand.request(1)
    val consumedCommand = probeForCommand.requestNext(30.seconds)
    val unpackedMap     = MsgPackUtils.unpackMap(consumedCommand.value())

    val sensorRequestPayloadMsgPacked = SensorSampleProps(s = sensorTypeForTest,
      t = convertISOToMicros(requestQuery.request.timestamp)).toMsgPack

    val sensorSampleRequestMsgPacked = convertCASELtoRestPackForVideoNode(requestQuery.request.requestid,
                                                                          requestQuery.request.nodeprops.nodeid,
                                                                          requestQuery.request.timestamp,
                                                                          requestQuery.request.extprops.sensorid)
      .asInstanceOf[(SensorQueryEnvelope,SensorSampleRequest)]

    unpackedMap("uuid") mustEqual sensorSampleRequestMsgPacked._2.uuid
    unpackedMap("uuid") mustEqual requestQuery.request.requestid
    unpackedMap("sid") mustEqual sensorSampleRequestMsgPacked._2.sid
    unpackedMap("sid") mustEqual requestQuery.request.nodeprops.nodeid
    unpackedMap("p") mustEqual SensorValueConverter.constructRestPathForDeviceRequest(nodeId, sensorTypeForTest)
    unpackedMap("p") mustEqual sensorSampleRequestMsgPacked._2.p
    unpackedMap("l") mustEqual sensorRequestPayloadMsgPacked

    probeForCommand.cancel()
  }
}
