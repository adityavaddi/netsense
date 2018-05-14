package com.verizon.netsense.service

import java.util.UUID

import akka.actor.ActorSystem
import akka.kafka.ProducerMessage.Message
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.kafka.{ConsumerSettings, ProducerSettings, Subscriptions}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.stream.testkit.TestSubscriber
import akka.stream.testkit.TestSubscriber.Probe
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestKit
import akka.{Done, NotUsed}
import com.verizon.netsense.config.{SensorKafkaConnector, TestSuiteConfig}
import com.verizon.netsense.data.TestData
import com.verizon.netsense.helper.{Common, CommonKafkaConsumer, SensorSampleMsgPackConverter}
import com.verizon.netsense.model.{CoreNodeRawSensorSample, SensorPayload, SensorSampleEvent}
import com.verizon.netsense.utils.ObjectMapperUtil
import net.manub.embeddedkafka.{EmbeddedKafka, EmbeddedKafkaConfig}
import org.apache.kafka.clients.consumer.ConsumerConfig
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer}
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FlatSpecLike, MustMatchers}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.reflectiveCalls
import scala.util.Random

/**
  * Created by maidapr on 4/27/17.
  */
class SensorSampleIngestServiceSpec
  extends TestKit(ActorSystem("KafkaIntegrationSpec-System"))
    with FlatSpecLike
    with MustMatchers
    with BeforeAndAfterAll
    with BeforeAndAfterEach
    with TestSuiteConfig
    with TestData with Common {

  final case class StageStoppingTimeout(time: FiniteDuration)

  implicit val embeddedKafkaConfig = EmbeddedKafkaConfig(kafkaPort = 8177, zooKeeperPort = 8178)

  val bootstrapServers = s"localhost:${embeddedKafkaConfig.kafkaPort}"

  implicit val mat                  = ActorMaterializer()(system)
  implicit val ec                   = system.dispatcher
  implicit val stageStoppingTimeout = StageStoppingTimeout(20.seconds)

  trait KafkaConfiguration {

    def createTopic: String = "topic-" + UUID.randomUUID().toString

    def createGroup: String = "group-" + UUID.randomUUID().toString

    val partition0 = 0
    val group1     = createGroup

    def createEventConsumerSettings1(group: String): ConsumerSettings[Array[Byte], Array[Byte]] =
      ConsumerSettings(system, new ByteArrayDeserializer, new ByteArrayDeserializer)
        .withBootstrapServers(bootstrapServers)
        .withGroupId(group)
        .withProperty(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest")
        .withWakeupTimeout(10.seconds)
        .withMaxWakeups(10)

    def createEventConsumerSettings(group: String): ConsumerSettings[Array[Byte], Array[Byte]] =
      SensorKafkaConnector.configKafkaConsumerSettings(bootstrapServers,
        group,
        new ByteArrayDeserializer,
        new ByteArrayDeserializer)

    def createEventProducerSettings: ProducerSettings[Array[Byte], Array[Byte]] =
      SensorKafkaConnector.configKafkaProducerSettings(bootstrapServers, new ByteArraySerializer, new ByteArraySerializer)

    def produceEvent(topic: String, sensorSampleEvent: SensorSampleEvent): Future[Done] = {
      val source = Source
        .single(sensorSampleEvent)
        .map(e => ObjectMapperUtil.toJson(e))
        .map(m => {
          Message(new ProducerRecord(topic, sensorSampleEvent.uuid.getBytes(), m.getBytes()), NotUsed)
        })
        .viaMat(Producer.flow(createEventProducerSettings))(Keep.right)
      source.runWith(Sink.ignore)
    }

    def produceEventForCoreNode(topic: String, sensorSampleEvent: CoreNodeRawSensorSample): Future[Done] = {
      val source = Source
        .single(sensorSampleEvent)
        .map(e => ObjectMapperUtil.toJson(e))
        .map(m => {
          Message(new ProducerRecord(topic, UUID.randomUUID().toString.getBytes(), m.getBytes()), NotUsed)
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

  trait KafkaSettings extends KafkaConfiguration with TestData with Common {
    val testEvent           = sensorSampleEvent
    val testEventMarshalled = ObjectMapperUtil.toJson(testEvent)
    val testTopic           = createTopic
    val testGroup           = createGroup
    Await.result(produceEvent(testTopic, testEvent), remainingOrDefault)
  }

  "Kafka Stream" should "start the embedded kafka & close the probe" in new KafkaSettings {

    val probe = createEventProbe(createEventConsumerSettings(testGroup), testTopic)
    probe.ensureSubscription()
    probe.cancel()
  }

  it should "produce a device sample message onto embedded kafka & can consume it" in new KafkaSettings {

    val consumerSettings = createEventConsumerSettings(testGroup)
    val probe            = createEventProbe(consumerSettings, testTopic)
    probe.request(1)
    val consumedMsg1 = probe.expectNext()
    val unpackedMessage =
      Await.result(ObjectMapperUtil.fromJsonByteArrayAsync[SensorSampleEvent](consumedMsg1), remainingOrDefault)
    unpackedMessage.get must equal(testEvent)
    probe.cancel()
  }

  it should "consume the messages from kafka consumer flow" in new KafkaSettings {

    val consumerUnderTest =
      SensorSampleIngestService.configKafkaSource(createEventConsumerSettings(testGroup), Set(testTopic))

    def createEventProbe: TestSubscriber.Probe[Array[Byte]] =
      consumerUnderTest
        .map(_.value)
        .runWith(TestSink.probe)

    val probe = createEventProbe
    probe.request(1)
    val consumedMsg = probe.requestNext(10.second)
    val convertedMsg =
      Await.result(ObjectMapperUtil.fromJsonByteArrayAsync[SensorSampleEvent](consumedMsg), remainingOrDefault)
    convertedMsg.get must equal(testEvent)
    probe.cancel()
  }

  it should "make the message flow thru Json unwrap flowShape & handle the unexpected event" in new KafkaSettings {

    val unexpectedEventProducer = Source
      .single("don't fail!#%^&*()")
      .map(m => {
        Message(new ProducerRecord(testTopic, m.getBytes(), m.getBytes()), NotUsed)
      })
      .viaMat(Producer.flow(createEventProducerSettings))(Keep.right)
      .runWith(Sink.ignore)
    val consumerUnderTest =
      SensorSampleIngestService.configKafkaSource(createEventConsumerSettings(testGroup), Set(testTopic))
    val flowUnderTest = CommonKafkaConsumer.restPackUnmarshallingFlow

    def createEventProbe: Probe[SensorSampleEvent] =
      consumerUnderTest.via(flowUnderTest).runWith(TestSink.probe)

    val probe = createEventProbe
    probe.request(2)
    val consumedMsg = probe.requestNext(remainingOrDefault)
    consumedMsg must equal(testEvent)
    probe.cancel()
  }

  it should "consume video device sample and can produce the sensor messages to sensor topic" in new KafkaSettings {

    val testTopicName = createTopic
    val sensorType    = "pc"
    val producedEventPayload =
      SensorPayload(n = nodeId, u = sensorUnits, s = sensorType, v = Random.nextInt((1000 - 0) + 1).toLong, t = System.currentTimeMillis())
    val producedEvent =
      SensorSampleEvent(UUID.randomUUID().toString,
        nodeId,
        unsolEventType,
        sensorType,
        eventPath,
        producedEventPayload)
    Await.result(produceEvent(testTopicName, producedEvent), remainingOrDefault)

    val consumerUnderTest =
      SensorSampleIngestService.configKafkaSource(createEventConsumerSettings(group1), Set(testTopicName))
    val flowUnderTest = CommonKafkaConsumer.restPackUnmarshallingFlow

    val kafkaSinkUnderTest = SensorSampleIngestService.configKafkaProducer(createEventProducerSettings)

    val (killSwitch, done) = consumerUnderTest
      .via(
        flowUnderTest.map(
          x => new ProducerRecord[Array[Byte], Array[Byte]](x.l.s, x.l.s.getBytes(), ObjectMapperUtil.toJsonBytes(x))
        )
      )
      .toMat(kafkaSinkUnderTest)(Keep.both)
      .run()

    val sensorConsumerUnderTest =
      SensorSampleIngestService.configKafkaSource(createEventConsumerSettings(group1), Set(sensorType))
    val probe = sensorConsumerUnderTest.via(flowUnderTest).toMat(TestSink.probe)(Keep.right).run()
    probe.request(1)
    val consumedMsg   = probe.requestNext(30.second)
    val receivedEvent = consumedMsg
    receivedEvent mustNot be(null)
    receivedEvent mustEqual producedEvent
    probe.cancel()
    killSwitch.shutdown()
  }

  it should "consume core device sample and can produce the sensor messages to sensor topic" in new KafkaSettings {

    val testTopicName = createTopic
    val sensorType    ="p"
    val producedEvent = generateCoreNodeSensorSample(coreNodeId, sensorType, starttimeInMicros, 0)
    val restPackConvertedMsg = SensorSampleMsgPackConverter.msgPackToRestPackConverter(producedEvent)

    Await.result(produceEventForCoreNode(testTopicName, producedEvent), remainingOrDefault)

    val consumerUnderTest =
      SensorSampleIngestService.configKafkaSource(createEventConsumerSettings(group1), Set(testTopicName))
    val flowUnderTest = CommonKafkaConsumer.msgPackUnmarshallingFlow
      .via(CommonKafkaConsumer.msgPackUnpackingFlow)

    val kafkaSinkUnderTest = SensorSampleIngestService.configKafkaProducer(createEventProducerSettings)

    val eventProbe = consumerUnderTest
      .via(flowUnderTest)
      .runWith(TestSink.probe)

    val probe = eventProbe
    probe.request(1)
    val consumedMsg   = probe.requestNext(30.second)
    val receivedEvent = consumedMsg
    receivedEvent mustNot be(null)
    receivedEvent.l.t mustEqual restPackConvertedMsg.l.t
    receivedEvent.sid mustEqual restPackConvertedMsg.sid
    probe.cancel()
  }

  it should "filter the sensor sample event from listed topics " in new KafkaSettings {
    val predicateUnderTest = SensorSampleIngestService.sensorTypeFilter
    val sensorType         = "l"
    val payload            = SensorPayload(n = nodeId, u = sensorUnits, s = sensorType, v = sensorValue, t = starttimeInMicros)
    val element            = SensorSampleEvent(messageId, nodeId, unsolEventType, sensorType, eventPath, payload)
    val test               = Source.single(element).filter(predicateUnderTest).runWith(TestSink.probe)
    val request            = test.requestNext()
    request mustBe element
  }

  ignore should "consume device sample without option and can produce the sensor messages to sensor topic" in new KafkaSettings {

    val testTopicName        = createTopic
    val sensorType           = "l"
    val producedEventPayload = SensorPayload(n = nodeId, u = sensorUnits, s = sensorType, v = sensorValue, t = starttimeInMicros)
    val producedEvent =
      SensorSampleEvent(messageId, nodeId, unsolEventType, sensorType, eventPath, producedEventPayload)
    Await.result(produceEvent(testTopicName, producedEvent), remainingOrDefault)
    val consumerUnderTest =
      SensorSampleIngestService.configKafkaSource(createEventConsumerSettings(group1), Set(testTopicName))
    val flowUnderTest                 = CommonKafkaConsumer.restPackUnmarshallingFlow
    val kafkaSinkUnderTest            = SensorSampleIngestService.configKafkaProducer(createEventProducerSettings)
    val kafkaSinkUnderTest1           = SensorSampleIngestService.kafkaSensorSink
    val sensorValueConvertorUnderTest = SensorSampleIngestService.sensorValueConverterFlow
    val (killSwitch, done) = consumerUnderTest
      .via(flowUnderTest)
      .via(sensorValueConvertorUnderTest)
      .toMat(kafkaSinkUnderTest1)(Keep.both)
      .run()
    lazy val sensorConsumerUnderTest =
      SensorSampleIngestService.configKafkaSource(createEventConsumerSettings("group2"),
        Set(sensorTopicMapping(sensorType)))
    val probe = sensorConsumerUnderTest
      .via(CommonKafkaConsumer.restPackUnmarshallingFlow)
      .toMat(TestSink.probe)(Keep.right)
      .run()
    probe.request(1)
    val consumedMsg   = probe.requestNext(30.second)
    val receivedEvent = consumedMsg
    receivedEvent mustNot be(null)
    receivedEvent mustEqual producedEvent
    Await.result(killSwitch.shutdown(), 10.second)
    probe.cancel()
  }
}
