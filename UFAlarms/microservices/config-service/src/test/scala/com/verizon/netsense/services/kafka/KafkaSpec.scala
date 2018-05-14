package com.verizon.netsense.services.kafka

import java.util.UUID

import akka.{Done, NotUsed}
import akka.actor.{ActorRef, ActorSystem}
import akka.kafka.ProducerMessage.Message
import akka.kafka.{ConsumerSettings, ProducerSettings, Subscriptions}
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.stream.testkit.TestSubscriber
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestKit
import com.typesafe.config.ConfigFactory
import net.manub.embeddedkafka.{EmbeddedKafka, EmbeddedKafkaConfig}
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer, StringDeserializer, StringSerializer}
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FlatSpecLike, MustMatchers}

import scala.concurrent.duration._
import com.verizon.netsense.connector._
import com.verizon.netsense.entity._
import com.verizon.netsense.services.data.TestData
import com.verizon.netsense.utils.{Deserializer}
import org.apache.kafka.clients.producer.ProducerRecord

import scala.concurrent.{Await, ExecutionContext, Future}

/**
 * Created by davor on 6/9/17.
 */
class KafkaSpec
  extends TestKit(ActorSystem("kafka-system"))
  with FlatSpecLike
    with MustMatchers
    with BeforeAndAfterAll
    with BeforeAndAfterEach
    with TestData
{
  val kafkaConnection = new KafkaConnection(ActorSystem("kafka-system"))

  final case class StageStoppingTimeout(time: FiniteDuration)

  private val configLoader      = ConfigFactory.load()

  implicit val embeddedKafkaConfig = EmbeddedKafkaConfig(kafkaPort = 8777, zooKeeperPort = 8778)

  val bootstrapServers = s"localhost:${embeddedKafkaConfig.kafkaPort}"

  implicit val mat                  = ActorMaterializer()(system)
  implicit val ec                   = system.dispatcher
  implicit val stageStoppingTimeout = StageStoppingTimeout(20.seconds)

  implicit val deserializer = new ByteArrayDeserializer

  trait KafkaConfiguration {

    def createTopic: String = "topic-" + UUID.randomUUID().toString

    def createGroup: String = "group-" + UUID.randomUUID().toString

    val partition0 = 0
    val group1     = createGroup

    def createEventConsumerSettings(group: String): ConsumerSettings[Array[Byte], Array[Byte]] =
      kafkaConnection.configKafkaConsumerSettings(bootstrapServers,
        group,
        new ByteArrayDeserializer,
        new ByteArrayDeserializer)

    def createEventConsumerSettingsString(group: String): ConsumerSettings[String, String] =
      kafkaConnection.configKafkaConsumerSettings(bootstrapServers,
                                                  group,
                                                  new StringDeserializer,
                                                  new StringDeserializer)

    def createEventProducerSettings: ProducerSettings[Array[Byte], Array[Byte]] =
      kafkaConnection.configKafkaProducerSettings(bootstrapServers,
        new ByteArraySerializer,
        new ByteArraySerializer)

    def produceConfigEvent(topic: String, isResponseMessage: ISMessageConfig): Future[Done] = {
      val source = Source
        .single(isResponseMessage)
        .map(e => Deserializer.jsonMapper.writeValueAsString(e))
        .map(m => {
          Message(new ProducerRecord(topic, isResponseMessage.response.requestid.getBytes(), m.getBytes()), NotUsed)
        })
        .viaMat(Producer.flow(createEventProducerSettings))(Keep.right)
      source.runWith(Sink.ignore)
    }

    def produceIsRequest(topic: String, sensorQueryEnvelope: IsRequestQueryEnvelope): Future[Done] = {
      val source = Source
        .single(sensorQueryEnvelope)
        .map(e => Deserializer.jsonMapper.writeValueAsString(e))
        .map(m => {
          val request = sensorQueryEnvelope.request.asInstanceOf[IsQueryEnvelope]
          Message(new ProducerRecord(topic, request.requestid.getBytes, m.getBytes()), NotUsed)
        })
        .viaMat(Producer.flow(createEventProducerSettings))(Keep.right)
      source.runWith(Sink.ignore)
    }

    def produceIsConfigResponse(topic: String,isQueryEnvelope: ISMessageConfig): Future[Done] = {
      val source = Source
        .single(isQueryEnvelope)
        .map(e => Deserializer.jsonMapper.writeValueAsString(e))
        .map(m => {
          Message(new ProducerRecord(topic, isQueryEnvelope.response.requestid.getBytes, m.getBytes()), NotUsed)
        })
        .viaMat(Producer.flow(createEventProducerSettings))(Keep.right)
      source.runWith(Sink.ignore)
    }

    def produceIsErrorResponse(topic: String, isQueryEnvelope: ISMessageError): Future[Done] = {
      val source = Source
        .single(isQueryEnvelope)
        .map(e => Deserializer.jsonMapper.writeValueAsString(e))
        .map(m => {
          Message(new ProducerRecord(topic, isQueryEnvelope.response.requestid.getBytes, m.getBytes()), NotUsed)
        })
        .viaMat(Producer.flow(createEventProducerSettings))(Keep.right)
      source.runWith(Sink.ignore)
    }

    def produceIsSuccessResponse(topic: String, isQueryEnvelope: ISMessageSuccess): Future[Done] = {
      val source = Source
        .single(isQueryEnvelope)
        .map(e => Deserializer.jsonMapper.writeValueAsString(e))
        .map(m => {
          Message(new ProducerRecord(topic, isQueryEnvelope.response.requestid.getBytes, m.getBytes()), NotUsed)
        })
        .viaMat(Producer.flow(createEventProducerSettings))(Keep.right)
      source.runWith(Sink.ignore)
    }

    def produceIsTokenResponse(topic: String, isQueryEnvelope: ISMessageToken): Future[Done] = {
      val source = Source
        .single(isQueryEnvelope)
        .map(e => Deserializer.jsonMapper.writeValueAsString(e))
        .map(m => {
          Message(new ProducerRecord(topic, isQueryEnvelope.response.requestid.getBytes, m.getBytes()), NotUsed)
        })
        .viaMat(Producer.flow(createEventProducerSettings))(Keep.right)
      source.runWith(Sink.ignore)
    }

    def createEventProbe(consumerSettings: ConsumerSettings[Array[Byte], Array[Byte]],
                         topic: String): TestSubscriber.Probe[Array[Byte]] =
      Consumer
        .plainSource(consumerSettings, Subscriptions.topics(topic))
        .map(_.value)
        .runWith(TestSink.probe)

  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    EmbeddedKafka.start()
  }

  override def afterAll(): Unit = {
    super.afterAll()
    EmbeddedKafka.stop()
    system.terminate()
  }


  def responsetopic: String= UUID.randomUUID().toString

  trait KafkaSettingsQuery extends KafkaConfiguration {
    lazy val testTopic: String = createTopic
    lazy val testGroup: String = createGroup
    lazy val requestTopic: String = createTopic
    lazy val responseTopic: String = createTopic
    lazy val responseTopic1: String = responsetopic
  }

  it should "start the embedded kafka & close the probe" in new KafkaSettingsQuery {

    val probe = createEventProbe(createEventConsumerSettings(testGroup), testTopic)
    probe.ensureSubscription()
    probe.cancel()
  }

  it should "produce a IS request event onto embedded kafka & can consume it" in new KafkaSettingsQuery{
    val testEvent = generateIsRequestWithHeaders()
    EmbeddedKafka.createCustomTopic(topic = testTopic)

    Await.result(produceIsRequest(testTopic, testEvent), 10.seconds)
    Thread.sleep(1000)
    val consumerSettings = createEventConsumerSettings(testGroup)
    val probe = createEventProbe(consumerSettings, testTopic).request(1)

    val consumedMsg1 = probe.expectNext(10.seconds)

    //probe.request(1)
    //val consumedMsg1 = EmbeddedKafka.consumeFirstMessageFrom[Array[Byte]](testTopic)
    val unpackedMessage =
      Await.result(
          Future.successful(Option(Deserializer.jsonMapper.readValue[IsRequestQueryEnvelope](consumedMsg1, classOf[IsRequestQueryEnvelope]))),
          remainingOrDefault)
    unpackedMessage.get must equal(testEvent)
    probe.cancel()
  }

  it should "produce a IS config response event onto embedded kafka & can consume it" in new KafkaSettingsQuery{
    val testEvent = generateIsResponseConfigWithHeaders()
    Await.result(produceIsConfigResponse(testTopic, testEvent), remainingOrDefault)
    val consumerSettings = createEventConsumerSettings(testGroup)
    val probe = createEventProbe(consumerSettings, testTopic)
    probe.request(1)
    val consumedMsg1 = probe.expectNext(10.seconds)
    val unpackedMessage =
      Await.result(
        Future.successful(Option(Deserializer.jsonMapper.readValue[ISMessageConfig](consumedMsg1, classOf[ISMessageConfig]))),
        remainingOrDefault)
    unpackedMessage.get.toJSON must equal(testEvent.toJSON)
    probe.cancel()
  }

  it should "produce a IS error response event onto embedded kafka & can consume it" in new KafkaSettingsQuery{
    val testEvent = generateIsResponseErrorWithHeaders()
    Await.result(produceIsErrorResponse(testTopic, testEvent), remainingOrDefault)
    val consumerSettings = createEventConsumerSettings(testGroup)
    val probe = createEventProbe(consumerSettings, testTopic)
    probe.request(1)
    val consumedMsg1 = probe.expectNext(10.seconds)
    val unpackedMessage =
      Await.result(
        Future.successful(Option(Deserializer.jsonMapper.readValue[ISMessageError](consumedMsg1, classOf[ISMessageError]))),
        remainingOrDefault)
    unpackedMessage.get.toJSON must equal(testEvent.toJSON)
    probe.cancel()
  }

  it should "produce a IS success response event onto embedded kafka & can consume it" in new KafkaSettingsQuery{
    val testEvent = generateIsResponseSuccessWithHeaders()
    Await.result(produceIsSuccessResponse(testTopic, testEvent), remainingOrDefault)
    val consumerSettings = createEventConsumerSettings(testGroup)
    val probe = createEventProbe(consumerSettings, testTopic)
    probe.request(1)
    val consumedMsg1 = probe.expectNext(10.seconds)
    val unpackedMessage =
      Await.result(
        Future.successful(Option(Deserializer.jsonMapper.readValue[ISMessageSuccess](consumedMsg1, classOf[ISMessageSuccess]))),
        remainingOrDefault)
    unpackedMessage.get.toJSON must equal(testEvent.toJSON)
    probe.cancel()
  }

  it should "produce a IS token response event onto embedded kafka & can consume it" in new KafkaSettingsQuery{
    val testEvent = generateIsResponseTokenWithHeaders()
    Await.result(produceIsTokenResponse(testTopic, testEvent), remainingOrDefault)
    val consumerSettings = createEventConsumerSettings(testGroup)
    val probe = createEventProbe(consumerSettings, testTopic)
    probe.request(1)
    val consumedMsg1 = probe.expectNext(10.seconds)
    val unpackedMessage =
      Await.result(
        Future.successful(Option(Deserializer.jsonMapper.readValue[ISMessageToken](consumedMsg1, classOf[ISMessageToken]))),
        remainingOrDefault)
    unpackedMessage.get.toJSON must equal(testEvent.toJSON)
    probe.cancel()
  }
}
