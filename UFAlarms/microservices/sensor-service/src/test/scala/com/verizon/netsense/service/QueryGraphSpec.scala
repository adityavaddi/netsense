package com.verizon.netsense.service

import java.util.UUID

import akka.actor.{ActorRef, ActorSystem}
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
import com.verizon.netsense.config.{KafkaConfig, SensorKafkaConnector, SensorKafkaConnector$, TestSuiteConfig}
import com.verizon.netsense.connector.CassandraConnector
import com.verizon.netsense.data.TestData
import com.verizon.netsense.model.{SensorQueryEnvelope, SensorSampleEvent}
import com.verizon.netsense.test.kafka.Utils.StageStoppingTimeout
import com.verizon.netsense.util.TestKafkaConfig
import com.verizon.netsense.utils.ObjectMapperUtil
import com.vz.nsp.datasample.service.db.{DatabaseSpec, EmbeddedDatabase}
import net.manub.embeddedkafka.{EmbeddedKafka, EmbeddedKafkaConfig}
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer, StringDeserializer}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.reflectiveCalls

/**
 * Created by maidapr on 7/3/17.
 */
class QueryGraphSpec
    extends TestKit(ActorSystem("SensorServiceGraphSpec-System"))
    with TestData
    with DatabaseSpec
    with TestSuiteConfig
    with EmbeddedDatabase
    with CassandraConnector.testConnector.Connector {
  implicit val ec = ExecutionContext.Implicits.global

  implicit val embeddedKafkaConfig = EmbeddedKafkaConfig(kafkaPort = 8676, zooKeeperPort = 8677)
  val bootstrapServers             = s"localhost:${embeddedKafkaConfig.kafkaPort}"

  implicit val mat                  = ActorMaterializer()(system)
  implicit val stageStoppingTimeout = StageStoppingTimeout(20.seconds)

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
    session.init()
    QueryServiceDB.init()
    EmbeddedKafka.start()
  }

  override def afterAll(): Unit = {
    EmbeddedKafka.stop()
    super.afterAll()
    system.terminate()
    QueryServiceDB.cleanup()
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

  trait KafkaSettingsQuery extends KafkaConfiguration {
    lazy val coreNodeEventTopic: String       = createTopic
    lazy val videoNodeEventTopic: String      = createTopic
    lazy val commandTopic: String             = createTopic
    lazy val testGroup: String                = createGroup
    lazy val requestTopic: String             = createTopic
    lazy val responseTopic: String            = createTopic
    lazy val responseTopic1: String           = responsetopicInRequest

    implicit private val setupKafkaConfig = TestKafkaConfig.setupEmbeddedKafkaConfig(bootstrapServers,
                                                                                 testGroup,
                                                                                 coreNodeEventTopic,
                                                                                 videoNodeEventTopic,
                                                                                 requestTopic,
                                                                                 responseTopic,
                                                                                 commandTopic)
  }

  ignore should
  //  "Sensor History Query Graph" should
  "store the sensor sample using persist and get the sensor event from the" +
  " db with in date range" in new KafkaSettingsQuery {

    Await.ready(database.deviceSensorSampleTableTest.store(sensorSampleEvent), remainingOrDefault)
    Await.result(produceEvent(requestTopic, requestQueryWithHeaders), remainingOrDefault)
    val queryGraphUnderTest      = SensorHistoryQueryService.queryServiceGraph.run()
    val responseConsumerSettings = createEventConsumerSettingsString(createGroup)
    val consumerProbe            = createEventProbeString(responseConsumerSettings, requestQueryWithHeaders.responsetopic)
    consumerProbe.request(1)
    val consumedEvent = consumerProbe.requestNext(remainingOrDefault)
    consumedEvent mustBe ObjectMapperUtil.toJson(sensorSampleQueryResponse)
    consumerProbe.cancel()
  }

  ignore should "get the energy node event from the db with in date range" in new KafkaSettingsQuery {

    Await.ready(database.energySavingsNodeTableTest.storeEnergySavingNode(energySavingsNodeModel), remainingOrDefault)
    val energySavingsNodeQuery = generateQueryWithHeaders(nodeId, energySensorId)
    Await.ready(produceEvent(requestTopic, energySavingsNodeQuery), remainingOrDefault)
    val graphUnderTest           = SensorHistoryQueryService.queryServiceGraph.run()
    val responseConsumerSettings = createEventConsumerSettingsString(createGroup)
    val consumerProbe            = createEventProbeString(responseConsumerSettings, energySavingsNodeQuery.responsetopic)
    consumerProbe.request(1)
    val consumedEvent = consumerProbe.requestNext(30.seconds)
    consumedEvent mustBe ObjectMapperUtil.toJson(sensorEnergyNodeResponse)
    consumerProbe.cancel()
  }

  ignore should "get the energy site event from the db with in date range" in new KafkaSettingsQuery {

    Await.ready(database.energySavingSiteTableTest.storeEnergySavingSite(energySavingsSiteModel), remainingOrDefault)
    val energySavingsSiteQuery = generateQueryWithHeaders("all", energySensorId)
    Await.ready(produceEvent(requestTopic, energySavingsSiteQuery), remainingOrDefault)
    val graphUnderTest           = SensorHistoryQueryService.queryServiceGraph.run()
    val responseConsumerSettings = createEventConsumerSettingsString(createGroup)
    val consumerProbe            = createEventProbeString(responseConsumerSettings, energySavingsSiteQuery.responsetopic)
    consumerProbe.request(1)
    val consumedEvent = consumerProbe.requestNext(remainingOrDefault)
    consumedEvent mustBe ObjectMapperUtil.toJson(sensorEnergySiteResponse)
    consumerProbe.cancel()
  }

}
