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
import com.verizon.netsense.services.alert.services.utils.AlertRequestBuilder
import com.verizon.netsense.services.alert.util.ObjectMapperUtil._
import net.manub.embeddedkafka.{EmbeddedKafka, EmbeddedKafkaConfig}
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer}
import org.scalatest._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class AlertRestServiceSpec
    extends TestKit(ActorSystem("AlertRestIntegrationSpec-System"))
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
    Await.ready(CassandraDBTest.OrgHierarchyByNode.createTable(), 1.seconds)
    Await.ready(CassandraDBTest.DeviceAlerts.createTable(), 1.seconds)
    Await.ready(CassandraDBTest.AlertsByNode.createTable(), 1.seconds)
    Await.ready(CassandraDBTest.UFAlarms.createTable(), 1.seconds)
    Await.ready(CassandraDBTest.UFAlarmsById.createTable(), 1.seconds)
    loadOrgHierarchy()
    loadUFAlarms
  }

  override def afterAll(): Unit = {
    super.afterAll()
    EmbeddedKafka.stop()
    Await.ready(CassandraDBTest.DeviceAlerts.truncateTable, 1.seconds)
    Await.ready(CassandraDBTest.AlertsByNode.truncateTable, 1.seconds)
    Await.ready(CassandraDBTest.OrgHierarchyByNode.truncateTable, 1.seconds)
    Await.ready(CassandraDBTest.UFAlarms.truncateTable, 1.seconds)
    Await.ready(CassandraDBTest.UFAlarmsById.truncateTable, 1.seconds)
    system.terminate()
  }

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    EmbeddedKafka.start()
    loadAlerts()
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

  it should "get All Alerts for specified Org and Site" in new KafkaSettings {
    val request = AlertRequestBuilder()
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withUserType(Some("customer"))
      .withReqType(GET_ALERTS_FOR_ORG_ID_AND_SITE_ID.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[AppSuccessResponse[List[AlertResponse]]](consumedElement.value), remainingOrDefault)
        .response
        .result
    assert(result.nonEmpty)
    result.foreach(item => {
      assert(item.siteId == siteId1)
      assert(item.orgId == orgId1)
      assert(item.active)
      assert(item.ufName != null)
      assert(item.description != null)
      assert(item.displayToCustomer != null)
      assert(item.displayToPartner != null)
    })
    probe1.cancel()
  }

  it should "give empty set for get All Alerts Invalid Org and Site" in new KafkaSettings {
    val request = AlertRequestBuilder()
      .withOrgId(orgId_random)
      .withSiteId(siteId_random)
      .withUserType(Some("customer"))
      .withReqType(GET_ALERTS_FOR_ORG_ID_AND_SITE_ID.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[AppSuccessResponse[List[AlertResponse]]](consumedElement.value), remainingOrDefault)
        .response
        .result
    assert(result.isEmpty)
    probe1.cancel()
  }

  it should "get All Alerts for specified Node" in new KafkaSettings {
    val request = AlertRequestBuilder()
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withNodeId(nodeId1)
      .withUserType(Some("partner"))
      .withReqType(GET_ALERTS_FOR_NODE_ID.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[AppSuccessResponse[List[AlertResponse]]](consumedElement.value), remainingOrDefault)
        .response
        .result
    assert(result.nonEmpty)
    result.foreach(item => {
      assert(item.orgId == orgId1)
      assert(item.nodeId == nodeId1)
      assert(item.active)
      assert(item.ufName != null)
      assert(item.description != null)
      assert(item.displayToCustomer != null)
      assert(item.displayToPartner != null)
    })
    probe1.cancel()
  }

  it should "give empty set for get All Alerts Invalid Node" in new KafkaSettings {
    val request = AlertRequestBuilder()
      .withOrgId(orgId_random)
      .withNodeId(nodeId_random)
      .withUserType(Some("partner"))
      .withReqType(GET_ALERTS_FOR_NODE_ID.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[AppSuccessResponse[List[AlertResponse]]](consumedElement.value), remainingOrDefault)
        .response
        .result

    assert(result.isEmpty)
    probe1.cancel()
  }

  it should "return Alert for get Alert" in new KafkaSettings {
    val request = AlertRequestBuilder()
      .withAlertProps(AlertProps(Some(alertId1), None, None))
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withReqType(GET_ALERT_FOR_ALERT_ID.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result = Await
      .result(fromJsonAsync[AppSuccessResponse[AlertResponse]](consumedElement.value), remainingOrDefault)
      .response
      .result

    assert(result.alertId == alertId1)
    assert(result.orgId == orgId1)
    assert(result.ufName == ufName1)
    assert(result.description == description1)
    assert(result.displayToPartner)
    assert(result.displayToCustomer)
    probe1.cancel()
  }

  it should "give error for get Alert with Invalid Alert Id" in new KafkaSettings {
    val request = AlertRequestBuilder()
      .withAlertProps(AlertProps(Some(alertId_random), None, None))
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withReqType(GET_ALERT_FOR_ALERT_ID.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result = Await
      .result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault)
      .success
    assert(!result)

    probe1.cancel()
  }

  it should "return Alert for get Alert Sys" in new KafkaSettings {
    val request = AlertRequestBuilder()
      .withAlertProps(AlertProps(Some(alertId1), None, None))
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withReqType(GET_ALERT_SYS.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result = Await
      .result(fromJsonAsync[AppSuccessResponse[AlertResponse]](consumedElement.value), remainingOrDefault)
      .response
      .result
    assert(result.alertId == alertId1)
    assert(result.active)
    assert(result.ufName == ufName1)
    assert(result.description == description1)
    assert(result.displayToPartner)
    assert(result.displayToCustomer)
    probe1.cancel()
  }

  it should "not return Inactive Alert for get Alert Sys" in new KafkaSettings {
    val request = AlertRequestBuilder()
      .withAlertProps(AlertProps(Some(alertId_inactive), None, None))
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withReqType(GET_ALERT_SYS.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result = Await
      .result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault)
      .success
    assert(!result)
    probe1.cancel()
  }

  it should "give error for get Alert Sys with Invalid Alert Id" in new KafkaSettings {
    val request = AlertRequestBuilder()
      .withAlertProps(AlertProps(Some(alertId_random), None, None))
      .withReqType(GET_ALERT_SYS.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result = Await
      .result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault)
      .success
    assert(!result)

    probe1.cancel()
  }

  it should "dismiss Alert with Valid Alert Id" in new KafkaSettings {
    val request = AlertRequestBuilder()
      .withAlertProps(AlertProps(Some(alertId1), None, None))
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withReqType(DISMISS_ALERT.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result = Await
      .result(fromJsonAsync[AppSuccessResponse[Status]](consumedElement.value), remainingOrDefault)
      .response
      .success
    assert(result)

    probe1.cancel()
  }

  it should "give error when dismissing Alert with Invalid Alert Id" in new KafkaSettings {
    val request = AlertRequestBuilder()
      .withAlertProps(AlertProps(Some(alertId_random), None, None))
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withReqType(DISMISS_ALERT.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result = Await
      .result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault)
      .success
    assert(!result)
    assert(!result)

    probe1.cancel()
  }

  it should "delete Alert with Valid Alert Id" in new KafkaSettings {
    val request = AlertRequestBuilder()
      .withAlertProps(AlertProps(Some(alertId1), None, None))
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withReqType(DELETE_ALERT.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result = Await
      .result(fromJsonAsync[AppSuccessResponse[Status]](consumedElement.value), remainingOrDefault)
      .response
      .success
    assert(result)

    probe1.cancel()
  }

  it should "give error when deleting Alert with Invalid Alert Id" in new KafkaSettings {
    val request = AlertRequestBuilder()
      .withAlertProps(AlertProps(Some(alertId_random), None, None))
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withReqType(DELETE_ALERT.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result = Await
      .result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault)
      .success
    assert(!result)
    assert(!result)

    probe1.cancel()
  }

  it should "create Alert with valid Alert payload" in new KafkaSettings {
    val request = AlertRequestBuilder()
      .withAlertProps(getAlertProps)
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withReqType(ADD_ALERT.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result = Await
      .result(fromJsonAsync[AppSuccessResponse[AlertResponse]](consumedElement.value), remainingOrDefault)
      .response
      .result

    assert(result.orgId == orgId1)
    assert(result.siteId == siteId1)
    assert(result.severity == alertSeverity)
    assert(result.category == alertCategory)
    assert(result.nodeId == nodeId1)
    assert(result.ufName == ufName3)
    assert(result.description == description3)
    assert(!result.displayToPartner)
    assert(result.displayToCustomer)
    probe1.cancel()
  }

  it should "give error for creating Alert that already exists" in new KafkaSettings {
    val request = AlertRequestBuilder()
      .withAlertProps(getAlertProps)
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withReqType(ADD_ALERT.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result = Await
      .result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault)
      .success
    assert(!result)
    probe1.cancel()
  }

  it should "give error for creating Alert with Invalid Alert payload" in new KafkaSettings {
    val request = AlertRequestBuilder()
      .withAlertProps(getAlertPropsInvalid)
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withReqType(ADD_ALERT.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result = Await
      .result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault)
      .success
    assert(!result)
    probe1.cancel()
  }

  it should "update Alert with valid Alert payload" in new KafkaSettings {
    val request = AlertRequestBuilder()
      .withAlertProps(getAlertPropsForUpdate)
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withReqType(UPDATE_ALERT.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result = Await
      .result(fromJsonAsync[AppSuccessResponse[AlertResponse]](consumedElement.value), remainingOrDefault)
      .response
      .result

    assert(result.orgId == orgId1)
    assert(result.siteId == siteId1)
    assert(result.category == alertCategory)
    assert(result.severity == updatedSeverity)
    assert(result.`type` == updateType)
    assert(result.ufName == ufName3)
    assert(result.description == description3)
    assert(!result.displayToPartner)
    assert(result.displayToCustomer)
    probe1.cancel()
  }

  it should "give error for updating Alert with Invalid Alert Id" in new KafkaSettings {
    val request = AlertRequestBuilder()
      .withAlertProps(getAlertPropsInvalid)
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withReqType(UPDATE_ALERT.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result = Await
      .result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault)
      .success
    assert(!result)
    probe1.cancel()
  }

  it should "produce a sample Request without alert props and the exception should be thrown" in new KafkaSettings {

    an[InvalidRequestPayloadException] must be thrownBy RequestValidator.validationPredicate(appReq_NoAlertProps)
  }

  it should "produce a sample Request without node props and the exception should be thrown" in new KafkaSettings {

    an[InvalidRequestPayloadException] must be thrownBy RequestValidator.validationPredicate(appReq_NoNodeProps)
  }

}
