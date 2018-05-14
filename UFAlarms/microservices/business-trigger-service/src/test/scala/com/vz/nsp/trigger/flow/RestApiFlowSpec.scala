package com.vz.nsp.trigger.flow


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
import com.verizon.netsense.test.kafka.Utils.StageStoppingTimeout
import com.vz.nsp.trigger.config.KafkaConfig
import com.vz.nsp.trigger.config.KafkaConnector._
import com.vz.nsp.trigger.dblayer.DbLayer
import com.vz.nsp.trigger.model.app.TriggerResponse
import com.vz.nsp.trigger.model.casel.{AppRequest, AppSuccessResponse}
import com.vz.nsp.trigger.service.{StreamService, TriggerProcessor, TriggerService}
import com.vz.nsp.trigger.util.BaseSpec
import com.vz.nsp.triggerservice.helper.ReqResGenerator
import com.vz.nsp.util.ObjectMapperUtil
//import com.verizon.netsense.businessalertservice.data.TestDataFeed._
import com.vz.nsp.trigger.db._
import com.vz.nsp.trigger.model._
import com.outworkers.phantom.database.DatabaseProvider
import com.vz.nsp.trigger.config._
import com.vz.nsp.trigger.data.TestData


import net.manub.embeddedkafka.EmbeddedKafkaConfig
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer, StringDeserializer}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

class RestApiFlowSpec extends TestKit(ActorSystem("Business-Test-System")) with BaseSpec with ProductionDatabase with TestData {

  object DatabaseService {
    def init() = {
      val create = Future.sequence(
        List(
          database.TriggerTable.create.ifNotExists().future()
        )
      )
      Await.ready(create, 5.seconds)
    }

    def cleanup(): Future[List[ResultSet]] = {
      val truncate = Future.sequence(
        List(
          database.TriggerTable.truncate.future()
        )
      )
      Await.ready(truncate, 5.seconds)
    }
  }

  override def beforeAll(): Unit = {
    DatabaseService.init()
    session.init()
  }

  override def afterAll(): Unit =
    DatabaseService.cleanup()

  implicit val ec = ExecutionContext.Implicits.global

  val kafkaConnector = new KafkaConnector()
  val reqRes         = new ReqResGenerator

  val businessTriggersDbLayer         = new DbLayer(new PhantomService with ProductionDatabase {})
  val triggerHelper = TriggerProcessor(businessTriggersDbLayer, reqRes)
  val streamHelper = StreamService(triggerHelper, reqRes)
  val triggerService = TriggerService(streamHelper)

  implicit val embeddedKafkaConfig  = EmbeddedKafkaConfig(kafkaPort = 8767, zooKeeperPort = 8768)
  val bootstrapServers              = s"localhost:${embeddedKafkaConfig.kafkaPort}"
  implicit val mat                  = ActorMaterializer()(system)
  implicit val stageStoppingTimeout = StageStoppingTimeout(20.seconds)

  def createEventProbe(consumerSettings: ConsumerSettings[Array[Byte], Array[Byte]],
                       topic: String): TestSubscriber.Probe[Array[Byte]] =
    Consumer
      .plainSource(consumerSettings, Subscriptions.topics(topic))
      .map(_.value)
      .runWith(TestSink.probe)

  def createEventConsumerSettings(group: String): ConsumerSettings[Array[Byte], Array[Byte]] =
    kafkaConnector.configKafkaConsumerSettings(bootstrapServers,
      group,
      new ByteArrayDeserializer,
      new ByteArrayDeserializer)

  trait KafkaConfiguration {
    val partition0 = 0
    val group1     = createGroup

    def createTopic: String = "topic-" + UUID.randomUUID().toString

    def createGroup: String = "group-" + UUID.randomUUID().toString

    def createEventConsumerSettingsString(group: String): ConsumerSettings[String, String] =
      kafkaConnector.configKafkaConsumerSettings(bootstrapServers,
        group,
        new StringDeserializer,
        new StringDeserializer)

    def createEventConsumerSettings(group: String): ConsumerSettings[Array[Byte], Array[Byte]] =
      kafkaConnector.configKafkaConsumerSettings(bootstrapServers,
        group,
        new ByteArrayDeserializer,
        new ByteArrayDeserializer)

    def produceEvent(topic: String, appRequest: AppRequest): Future[Done] = {
      val source = Source
        .single(appRequest)
        .map(e => ObjectMapperUtil.toJson(e))
        .map(m => {
          Message(new ProducerRecord(topic, appRequest.messageid.getBytes, m.getBytes()), NotUsed)
        })
        .viaMat(Producer.flow(createEventProducerSettings))(Keep.right)
      source.runWith(Sink.ignore)
    }

    def createEventProducerSettings: ProducerSettings[Array[Byte], Array[Byte]] =
      kafkaConnector.configKafkaProducerSettings(bootstrapServers, new ByteArraySerializer, new ByteArraySerializer)
  }

  final case class StageStoppingTimeout(time: FiniteDuration)
  trait KafkaSettingsQuery extends KafkaConfiguration {
    lazy val testTopic: String = createTopic
    lazy val testGroup: String = createGroup
    lazy val requestTopic: String = createTopic
    lazy val responseTopic: String = createTopic
    private implicit val setupKafkaConfig =
      KafkaConfig.setupEmbeddedKafkaConfig(bootstrapServers, testGroup, testTopic, requestTopic, responseTopic)
  }

  "RestApiFlow" should "start the embedded kafka & close the probe" in new KafkaSettingsQuery {
    val probe = createEventProbe(createEventConsumerSettings(testGroup), testTopic)
    probe.ensureSubscription()
    probe.cancel()
  }

  it should "Get Trigger by Id from Database" in new KafkaSettingsQuery {
    DatabaseService.cleanup()
    Await.result(database.TriggerTable.store(dbTrigger), remainingOrDefault)
    val flowUnderTest    = triggerService.triggerQueryFlow
    val sourceMock       = Source.single(appRequestTrigger1).map(x => x)
    val probe1           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement  = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[List[TriggerResponse]]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.size mustBe 1
    probe1.cancel()
  }

  it should "Get Empty BusinessAlert from Database" in new KafkaSettingsQuery {
    DatabaseService.cleanup()

    val flowUnderTest    = triggerService.triggerQueryFlow
    val sourceMock       = Source.single(appRequestTrigger1).map(x => x)
    val probe1           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement  = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[List[TriggerResponse]]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.size mustBe 0
    probe1.cancel()
  }

  it should "Get All Triggers from Database" in new KafkaSettingsQuery {
    DatabaseService.cleanup()
    Await.result(database.TriggerTable.store(dbTrigger), remainingOrDefault)
    Await.result(database.TriggerTable.store(dbTrigger2), remainingOrDefault)

    val flowUnderTest    = triggerService.triggerQueryFlow
    val sourceMock       = Source.single(appRequestAllTriggers).map(x => x)
    val probe1           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement  = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[List[TriggerResponse]]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.size mustBe 2
    probe1.cancel()
  }

  it should "Post Trigger to Database" in new KafkaSettingsQuery {
    DatabaseService.cleanup()
    Await.result(database.TriggerTable.store(dbTrigger), remainingOrDefault)
    Await.result(database.TriggerTable.store(dbTrigger2), remainingOrDefault)

    val flowUnderTest    = triggerService.triggerQueryFlow
    val sourceMock       = Source.single(appCreateTrigger).map(x => x)
    val probe1           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val request = probe1.request(1)
    val consumedElement = request.requestNext(FiniteDuration(5, java.util.concurrent.TimeUnit.SECONDS))

    consumedElement mustNot be(null)
    assert(consumedElement._1.messageid == messageId)
    probe1.cancel()
  }

 }