package com.verizon.netsense.services.alert.services.service

import java.util.UUID

import akka.actor.ActorSystem
import akka.kafka.ProducerMessage.Message
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.kafka.{ConsumerSettings, ProducerSettings, Subscriptions}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source, _}
import akka.stream.testkit.TestSubscriber
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestKit
import akka.{Done, NotUsed}
import com.verizon.netsense.services.alert.customexceptions.InvalidRequestPayloadException
import com.verizon.netsense.services.alert.helper.KafkaConfig.kafkaConfig
import com.verizon.netsense.services.alert.helper.{KafkaConnector, RequestValidator}
import com.verizon.netsense.services.alert.model.CaselType._
import com.verizon.netsense.services.alert.model.{Status, _}
import com.verizon.netsense.services.alert.services.RestService
import com.verizon.netsense.services.alert.services.config.TestSuiteConfig
import com.verizon.netsense.services.alert.services.data.TestData
import com.verizon.netsense.services.alert.services.database.CassandraDBTest
import com.verizon.netsense.services.alert.services.utils.ufAlarmsRequestBuilder
import com.verizon.netsense.services.alert.util.ObjectMapperUtil._
import net.manub.embeddedkafka.{EmbeddedKafka, EmbeddedKafkaConfig}
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer}
import org.scalatest._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class UFAlarmsRestServiceSpec
    extends TestKit(ActorSystem("UFAlarmsRestSpec-System"))
    with FlatSpecLike
    with MustMatchers
    with BeforeAndAfterAll
    with BeforeAndAfterEach
    with TestSuiteConfig
    with TestData {

  implicit val embeddedKafkaConfig = EmbeddedKafkaConfig(kafkaPort = embeddedKafkaPort, zooKeeperPort = embeddedZkPort)

  val bootstrapServers = s"localhost:${embeddedKafkaConfig.kafkaPort}"

  implicit val mat = ActorMaterializer()(system)
  implicit val ec  = system.dispatcher

  trait KafkaConfiguration {

    def createEventConsumerSettingsString(group: String): ConsumerSettings[Array[Byte], Array[Byte]] =
      KafkaConnector.configKafkaConsumerSettings(bootstrapServers,
                                                 group,
                                                 new ByteArrayDeserializer,
                                                 new ByteArrayDeserializer)

    def createEventProducerSettings: ProducerSettings[Array[Byte], Array[Byte]] =
      KafkaConnector.configKafkaProducerSettings(bootstrapServers, new ByteArraySerializer, new ByteArraySerializer)

    def produceEvent(topic: String, appRequest: String): Future[Done] = {
      val source = Source
        .single(appRequest)
        .map(m => {
          Message(new ProducerRecord(topic, UUID.randomUUID().toString.getBytes, m.getBytes), NotUsed)
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
    Await.ready(CassandraDBTest.UFAlarms.createTable(), 1.seconds)
    Await.ready(CassandraDBTest.UFAlarmsById.createTable(), 1.seconds)
    loadUFAlarms()
  }

  override def afterAll(): Unit = {
    super.afterAll()
    EmbeddedKafka.stop()
    Await.ready(CassandraDBTest.UFAlarms.truncateTable, 1.seconds)
    Await.ready(CassandraDBTest.UFAlarmsById.truncateTable, 1.seconds)
    system.terminate()
  }

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    EmbeddedKafka.start()
  }

  override def afterEach(): Unit = {
    super.afterEach()
    EmbeddedKafka.stop()
  }

  trait KafkaSettings extends KafkaConfiguration with TestData {
    val testGroup     = kafkaConfig.getString("group-name") + UUID.randomUUID().toString
    val alarmTopic    = kafkaConfig.getString("alarm-request-topic")
    val alertTopic    = kafkaConfig.getString("alert-response-topic")
    val requestTopic  = kafkaConfig.getString("interface-service-request-topic")
    val responseTopic = kafkaConfig.getString("interface-service-response-topic")

    val consumerUnderTest: Source[ConsumerRecord[Array[Byte], Array[Byte]], Consumer.Control] = RestService
      .configKafkaSource(createEventConsumerSettingsString(testGroup), Set(requestTopic))
    val jsonStringToAppRequestFlowTest = RestService.jsonStringToAppRequestFlow
    val validateRequestFlowTest        = RestService.validateRequestFlow
    val processAppRequestFlowTest      = RestService.processAppRequestFlow
    val jsonToRecordFlowTest           = RestService.jsonToRecord
    val publishToKafkaTest             = RestService.kafkaSink

    val probe1 = consumerUnderTest
      .map(x => x.value)
      .via(jsonStringToAppRequestFlowTest)
      .via(validateRequestFlowTest)
      .via(processAppRequestFlowTest)
      .via(jsonToRecordFlowTest)
      .runWith(TestSink.probe)

  }

  "Kafka Stream" should "start the embedded kafka & close the probe" in new KafkaSettings {

    val probe = createEventProbe(createEventConsumerSettingsString(testGroup), responseTopic)
    probe.ensureSubscription()
    probe.cancel()
  }

  it should "get All UFAlarms" in new KafkaSettings {
    val request = ufAlarmsRequestBuilder()
      .withReqType(GET_ALL_UF_ALARMS.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result = Await
      .result(fromJsonAsync[AppSuccessResponse[List[UfAlarm]]](consumedElement.value), remainingOrDefault)
      .response
      .result
    assert(result.nonEmpty)
    result.foreach(item => {
      assert(!item.alarmType.isEmpty)
      assert(item.ufName.isDefined)
    })
    probe1.cancel()
  }

  it should "get UFAlarm for valid mappingID" in new KafkaSettings {
    val request = ufAlarmsRequestBuilder()
      .withUfAlarmProps(UfAlarmProps(Some(mappingId1), None))
      .withReqType(GET_UF_ALARM.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[AppSuccessResponse[UfAlarm]](consumedElement.value), remainingOrDefault)
        .response
        .result
    assert(result != null)
    assert(result.ufName.getOrElse("").equals(ufName1))
    assert(result.alarmType.equals(alarmType1))
    assert(result.description.getOrElse("").equals(description1))
    assert(result.displayToCustomer)
    assert(result.displayToPartner)
    probe1.cancel()
  }

  it should "give error when fetching UFAlarm that do not exist" in new KafkaSettings {
    val request = ufAlarmsRequestBuilder()
      .withUfAlarmProps(UfAlarmProps(Some(UUID.randomUUID.toString), None))
      .withReqType(GET_UF_ALARM.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault)
        .success
    assert(!result)
    probe1.cancel()
  }

  it should "create UFAlarm with valid UFAlarm payload" in new KafkaSettings {
    val request = ufAlarmsRequestBuilder()
      .withUfAlarmProps(getUFAlarmProps)
      .withReqType(CREATE_UF_ALARM.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[AppSuccessResponse[UfAlarm]](consumedElement.value), remainingOrDefault)
        .response
        .result
    assert(result != null)
    assert(result.ufName.equals(getUFAlarmProps.ufName))
    assert(result.alarmType.equals(getUFAlarmProps.alarmType.getOrElse("")))
    assert(result.description.equals(getUFAlarmProps.description))
    assert(result.displayToPartner)
    assert(result.displayToCustomer)
    probe1.cancel()
  }

  it should "create UFAlarm with eixsting UFAlarm name and different models payload" in new KafkaSettings {
    val request = ufAlarmsRequestBuilder()
      .withUfAlarmProps(getUFAlarmProps1)
      .withReqType(CREATE_UF_ALARM.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[AppSuccessResponse[UfAlarm]](consumedElement.value), remainingOrDefault)
        .response
        .result
    assert(result != null)
    assert(result.ufName.equals(getUFAlarmProps1.ufName))
    assert(result.alarmType.equals(getUFAlarmProps1.alarmType.getOrElse("")))
    assert(result.description.equals(getUFAlarmProps1.description))
    assert(!result.displayToPartner)
    assert(!result.displayToCustomer)
    probe1.cancel()
  }

  it should "give error when creating UFAlarm with Type that already exists" in new KafkaSettings {
    val request = ufAlarmsRequestBuilder()
      .withUfAlarmProps(getUFAlarmProps)
      .withReqType(CREATE_UF_ALARM.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)

    val result =
      Await
        .result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault)
        .success
    assert(!result)
    probe1.cancel()
  }

  it should "give error when creating UFAlarm with UF Name that already exists" in new KafkaSettings {
    val request = ufAlarmsRequestBuilder()
      .withUfAlarmProps(getUFAlarmPropsExistingName)
      .withReqType(CREATE_UF_ALARM.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault)
        .success
    assert(!result)
    probe1.cancel()
  }

  it should "give error when creating UFAlarm with invalid UFAlarm payload" in new KafkaSettings {
    val request = ufAlarmsRequestBuilder()
      .withUfAlarmProps(getUFAlarmProps_Invalid)
      .withReqType(CREATE_UF_ALARM.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault)
        .success
    assert(!result)
    probe1.cancel()
  }

  it should "update UFAlarm with valid UFAlarm payload" in new KafkaSettings {
    val request = ufAlarmsRequestBuilder()
      .withUfAlarmProps(getUFAlarmPropsForUpdate)
      .withReqType(UPDATE_UF_ALARM.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[AppSuccessResponse[UfAlarm]](consumedElement.value), remainingOrDefault)
        .response
        .result
    assert(result != null)
    assert(result.mappingId.equals(getUFAlarmPropsForUpdate.mappingId.getOrElse("")))
    assert(result.ufName.equals(getUFAlarmPropsForUpdate.ufName))
    assert(result.description.equals(getUFAlarmPropsForUpdate.description))
    assert(result.alarmType.equals(alarmType1))
    assert(result.displayToPartner)
    assert(result.displayToCustomer)
    probe1.cancel()
  }

  it should "update UFAlarm with existing UFName and different node models" in new KafkaSettings {
    val request = ufAlarmsRequestBuilder()
      .withUfAlarmProps(getUFAlarmPropsForUpdate1)
      .withReqType(UPDATE_UF_ALARM.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[AppSuccessResponse[UfAlarm]](consumedElement.value), remainingOrDefault)
        .response
        .result
    assert(result != null)
    assert(result.mappingId.equals(getUFAlarmPropsForUpdate1.mappingId.getOrElse("")))
    assert(result.ufName.equals(getUFAlarmPropsForUpdate1.ufName))
    assert(result.description.equals(getUFAlarmPropsForUpdate1.description))
    assert(result.alarmType.equals(alarmType2))
    assert(result.displayToPartner)
    assert(result.displayToCustomer)
    probe1.cancel()
  }

  it should "give error when updating UFAlarm with ufName that already exists" in new KafkaSettings {
    val request = ufAlarmsRequestBuilder()
      .withUfAlarmProps(getUFAlarmPropsForUpdate)
      .withReqType(UPDATE_UF_ALARM.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault)
        .success
    assert(!result)
    probe1.cancel()
  }

  it should "give error when updating UFAlarm that do not exist" in new KafkaSettings {
    val request = ufAlarmsRequestBuilder()
      .withUfAlarmProps(getUFAlarmProps)
      .withReqType(UPDATE_UF_ALARM.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault)
        .success
    assert(!result)
    probe1.cancel()
  }

  it should "reset existing UFAlarm" in new KafkaSettings {
    val request = ufAlarmsRequestBuilder()
      .withUfAlarmProps(UfAlarmProps(Some(mappingId3), None))
      .withReqType(RESET_UF_ALARM.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[AppSuccessResponse[UfAlarm]](consumedElement.value), remainingOrDefault)
        .response
        .result

    assert(result != null)
    assert(result.mappingId.equals(mappingId3))
    assert(result.nodeModels.isEmpty)
    assert(result.alarmType.equals(alarmType3))
    assert(result.ufName.contains(alarmType3))
    assert(!result.displayToCustomer)
    assert(!result.displayToPartner)
    probe1.cancel()
  }

  it should "give error when resetting UFAlarm that do not exist" in new KafkaSettings {
    val request = ufAlarmsRequestBuilder()
      .withUfAlarmProps(UfAlarmProps(Some(UUID.randomUUID().toString), None))
      .withReqType(RESET_UF_ALARM.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault)
        .success
    assert(!result)
    probe1.cancel()
  }

  it should "delete  UFAlarm" in new KafkaSettings {
    val request = ufAlarmsRequestBuilder()
      .withUfAlarmProps(UfAlarmProps(Some(mappingId1), None))
      .withReqType(DELETE_UF_ALARM.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[AppSuccessResponse[Status]](consumedElement.value), remainingOrDefault)
        .response
        .success
    assert(result)
    probe1.cancel()
  }

  it should "give error when deleting  UFAlarm that do not exist" in new KafkaSettings {
    val request = ufAlarmsRequestBuilder()
      .withUfAlarmProps(UfAlarmProps(Some(UUID.randomUUID().toString), Some(alarmType1)))
      .withReqType(DELETE_UF_ALARM.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[AppSuccessResponse[Status]](consumedElement.value), remainingOrDefault)
        .response
        .success
    assert(!result)
    probe1.cancel()
  }

  it should "create Bulk UFAlarm with valid UFAlarm payload" in new KafkaSettings {
    val request = ufAlarmsRequestBuilder()
      .withUfAlarmProps(getUFAlarmPropsForBulk)
      .withReqType(CREATE_BULK_UF_ALARMS.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[AppSuccessResponse[List[BulkSuccess]]](consumedElement.value), remainingOrDefault)
        .response
        .result
    assert(result.nonEmpty)
    assert(result.length == 4)
    result.foreach { item =>
      assert(item.success)
      assert(item.ufAlarm != null)
      assert(item.ufAlarm.mappingId != null)
      assert(item.ufAlarm.ufName != null)
      assert(item.ufAlarm.description != null)
    }
    probe1.cancel()
  }

  it should " fail to create Bulk UFAlarm with alarm type that already exists" in new KafkaSettings {
    val request = ufAlarmsRequestBuilder()
      .withUfAlarmProps(getInvalidUFAlarmPropsForBulk1)
      .withReqType(CREATE_BULK_UF_ALARMS.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault)
        .success
    assert(!result)
    probe1.cancel()
  }

  it should " fail to create Bulk UFAlarm with ufname that already exists" in new KafkaSettings {
    val request = ufAlarmsRequestBuilder()
      .withUfAlarmProps(getInvalidUFAlarmPropsForBulk2)
      .withReqType(CREATE_BULK_UF_ALARMS.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault)
        .success
    assert(!result)
    probe1.cancel()
  }

  it should " fail to create Bulk UFAlarm with invalid UFAlarm payload" in new KafkaSettings {
    val request = ufAlarmsRequestBuilder()
      .withUfAlarmProps(getInvalidUFAlarmPropsForBulk)
      .withReqType(CREATE_BULK_UF_ALARMS.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault)
        .success
    assert(!result)
    probe1.cancel()
  }

  it should "produce a sample Request without ufAlarm props and the exception should be thrown" in new KafkaSettings {

    an[InvalidRequestPayloadException] must be thrownBy RequestValidator.validationPredicate(appReq_NoUfAlarmProps)
  }

}
