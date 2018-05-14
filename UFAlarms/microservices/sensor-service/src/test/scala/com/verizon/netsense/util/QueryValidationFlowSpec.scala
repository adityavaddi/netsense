package com.verizon.netsense.util

import java.util.UUID

import akka.actor.ActorSystem
import akka.kafka.ProducerMessage.Message
import akka.kafka.scaladsl.Consumer.Control
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.kafka.{ConsumerSettings, ProducerSettings, Subscriptions}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.stream.testkit.TestSubscriber
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestKit
import akka.{Done, NotUsed}
import com.verizon.netsense.config.{KafkaConfig, SensorKafkaConnector, SensorKafkaConnector$, TestSuiteConfig}
import com.verizon.netsense.data.TestData
import com.verizon.netsense.exceptions.CustomExceptions.MessageUnMarshallingException
import com.verizon.netsense.helper.{Common, QueryServiceValidator}
import com.verizon.netsense.model.{SensorSampleQueryResponse, _}
import com.verizon.netsense.service.SensorHistoryQueryService
import com.verizon.netsense.test.kafka.Utils.StageStoppingTimeout
import com.verizon.netsense.utils.ObjectMapperUtil
import net.manub.embeddedkafka.{EmbeddedKafka, EmbeddedKafkaConfig}
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer, StringDeserializer}
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, MustMatchers}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * Created by nalamte on 9/1/17.
  **/
class QueryValidationFlowSpec extends TestKit(ActorSystem("Query-Validation-System"))
                         with FlatSpecLike
                         with MustMatchers
                         with TestSuiteConfig
                         with BeforeAndAfterAll
                         with TestData{
  implicit val ec = ExecutionContext.Implicits.global

  implicit val embeddedKafkaConfig = EmbeddedKafkaConfig(kafkaPort = 8767, zooKeeperPort = 8768)
  val bootstrapServers = s"localhost:${embeddedKafkaConfig.kafkaPort}"

  implicit val mat = ActorMaterializer()(system)
  implicit val stageStoppingTimeout = StageStoppingTimeout(20.seconds)

  def createEventProbe(consumerSettings: ConsumerSettings[Array[Byte], Array[Byte]], topic: String): TestSubscriber.Probe[Array[Byte]] = Consumer.plainSource(consumerSettings, Subscriptions.topics(topic)).map(_.value).runWith(TestSink.probe)

  def createEventConsumerSettings(group: String): ConsumerSettings[Array[Byte], Array[Byte]] = SensorKafkaConnector.configKafkaConsumerSettings(bootstrapServers, group, new ByteArrayDeserializer, new ByteArrayDeserializer)

  override protected def beforeAll(): Unit ={
    super.beforeAll()
    EmbeddedKafka.start()
  }

  override def afterAll(): Unit ={
    super.afterAll()
    EmbeddedKafka.stop()
    system.terminate()
  }


  trait KafkaConfiguration{
    val partition0 = 0
    val group1 = createGroup

    def createTopic: String = "topic-" + UUID.randomUUID().toString

    def createGroup: String = "group-" + UUID.randomUUID().toString

    def createEventConsumerSettingsString(group: String): ConsumerSettings[String, String] = SensorKafkaConnector
      .configKafkaConsumerSettings(bootstrapServers, group, new StringDeserializer, new StringDeserializer)

    def createEventProducerSettings: ProducerSettings[Array[Byte], Array[Byte]] = SensorKafkaConnector
      .configKafkaProducerSettings(bootstrapServers, new ByteArraySerializer, new ByteArraySerializer)

    def produceEvent(topic: String, sensorQueryEnvelope: SensorQueryEnvelope): Future[Done] ={
      val source = Source.single(sensorQueryEnvelope).map(e => ObjectMapperUtil.toJson(e)).map(m => {
        Message(new ProducerRecord(topic, sensorQueryEnvelope.request.requestid.getBytes, m.getBytes()), NotUsed)
      }).viaMat(Producer.flow(createEventProducerSettings))(Keep.right)
      source.runWith(Sink.ignore)
    }

  }

  trait KafkaSettingsQuery extends KafkaConfiguration with Common{
    lazy val coreNodeEventTopic: String = createTopic
    lazy val videoNodeEventTopic: String = createTopic
    lazy val commandTopic: String = createTopic
    lazy val testGroup: String = createGroup
    lazy val requestTopic: String = createTopic
    lazy val responseTopic: String = createTopic

    private implicit val setupKafkaConfig = TestKafkaConfig
      .setupEmbeddedKafkaConfig(bootstrapServers,
                                testGroup,
                                coreNodeEventTopic,
                                videoNodeEventTopic,
                                requestTopic,
                                responseTopic,
                                commandTopic)
  }

  def buildSQE(sqe: SensorQueryEnvelope, error: String) = {
    val sensorSampleQueryPayload: SensorSampleQueryPayload = SensorSampleQueryPayload(sqe.request.requestid,
      success = false, sqe.request.timestamp, error, 400, items = null)
    (sqe, SensorSampleQueryResponse(sqe.messageid, response = sensorSampleQueryPayload))
  }

  ignore should "validate if nodeid or sensorid or siteid  are null in  validation flow" in
    new KafkaSettingsQuery {

    val testEvent = generateQueryWithHeaders(_sensorId = null)
    Await.result(produceEvent(videoNodeEventTopic, testEvent), remainingOrDefault)
    val consumerUnderTest: Source[ConsumerRecord[String, String], Control] =
      SensorHistoryQueryService.configKafkaSource(createEventConsumerSettingsString(testGroup), Set(videoNodeEventTopic))
    val flowUnderTest1 = SensorHistoryQueryService.msgUnmarshallingFlow
    val flowUnderTest2 = SensorHistoryQueryService.messageValidationFlow
    val testSink = TestSink.probe[SensorQueryEnvelope]
    def createEventProbe1 = consumerUnderTest.via(flowUnderTest1).via(flowUnderTest2).toMat(Sink.ignore)(Keep.right)
    createEventProbe1.run()
    private val expectedErrorResponse = buildSQE(testEvent, "One of the required fields in nodeId ,SiteId," +
      " sensorId or date fields are missing")
    val errorConsumerUnderTest: Source[ConsumerRecord[String, String], Control] = SensorHistoryQueryService
      .configKafkaSource(createEventConsumerSettingsString(testGroup), Set(testEvent.responsetopic))
    def createEventProbe: TestSubscriber.Probe[String] = errorConsumerUnderTest.map(_.value).runWith(TestSink.probe)
    private val consumedValue = createEventProbe.request(1)
    private val requestNext = consumedValue.requestNext(10.second)
    private val response = ObjectMapperUtil.fromJson[SensorSampleQueryResponse](requestNext)
    QueryServiceValidator.validationPredicate(generateQueryWithHeaders(null, null)) mustBe false
    response.messageid must equal(testEvent.messageid)
    response.response.success mustBe false
    response.response.error.startsWith("One of the required fields in")
    response.response.items mustBe null
    createEventProbe.cancel()
  }

  "QueryValidator" should "Validate the all the fields in validation flow" in {
    QueryServiceValidator.validationPredicate(generateQueryWithHeaders(nodeId, sensorId)) mustBe true
  }

  it should "Validate the date1 format" in new TestData {
    override val date1 = "jkagdui"
    QueryServiceValidator.validationPredicate(generateQueryWithHeaders(nodeId, sensorId)) mustBe false
  }

  it should "Validate  date2 format" in new TestData {
    override val date2 = "jkagdui"
    QueryServiceValidator.validationPredicate(generateQueryWithHeaders(nodeId, sensorId)) mustBe false
  }

  it should "Validate the all the fields in validation flow including " +
    "dates and event Type formats" in new TestData {
    override val date2 = "jkagdui"
    override val `type` = "fail_type"
    QueryServiceValidator.validationPredicate(generateQueryWithHeaders(nodeId, sensorId)) mustBe false
  }

  it should "Validate the all the fields in validation flow including date formats" in {
    QueryServiceValidator.validationPredicate(generateQueryWithHeaders(nodeId, sensorId)) mustBe true
  }

  it should "Validate if messageId and response topic are null in  validation flow" in new TestData {
    override val messageId = null
    override val responsetopicInRequest = null
    an[MessageUnMarshallingException] must be thrownBy QueryServiceValidator
      .validationPredicate(generateQueryWithHeaders(nodeId, sensorId))
  }

}
