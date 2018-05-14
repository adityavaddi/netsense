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
import com.verizon.netsense.services.alert.services.utils.NotificationRequestBuilder
import com.verizon.netsense.services.alert.util.ObjectMapperUtil._
import net.manub.embeddedkafka.{EmbeddedKafka, EmbeddedKafkaConfig}
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer}
import org.scalatest._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class NotificationRestServiceSpec
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
    Await.ready(CassandraDBTest.Notifications.createTable(), 1.seconds)
    Await.ready(CassandraDBTest.UFAlarms.createTable(), 1.seconds)
    Await.ready(CassandraDBTest.UFAlarmsById.createTable(), 1.seconds)
    loadNotifications()
    loadUFAlarms()
  }

  override def afterAll(): Unit = {
    super.afterAll()
    EmbeddedKafka.stop()
    Await.ready(CassandraDBTest.Notifications.truncateTable(), 1.seconds)
    Await.ready(CassandraDBTest.UFAlarms.truncateTable(), 1.seconds)
    Await.ready(CassandraDBTest.UFAlarmsById.truncateTable(), 1.seconds)
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

  it should "get All Notifications for specified Org and Site" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withReqType(NOTIFICATIONS_FOR_SITE_ID.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[AppSuccessResponse[List[Notification]]](consumedElement.value), remainingOrDefault)
        .response
        .result
    assert(result.nonEmpty)
    result.foreach(item => {
      assert(item.siteId == siteId1)
      assert(item.orgId == orgId1)
    })
    probe1.cancel()
  }

  it should "give empty set for get All Notifications with Invalid Org and Site" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withOrgId(orgId_random)
      .withSiteId(siteId_random)
      .withReqType(NOTIFICATIONS_FOR_SITE_ID.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[AppSuccessResponse[List[Notification]]](consumedElement.value), remainingOrDefault)
        .response
        .result
    assert(result.isEmpty)
    probe1.cancel()
  }

  it should "return Notification for get Notification" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withNotificationProps(NotificationProps(Some(notificationId1)))
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withReqType(NOTIFICATION_FOR_ID.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[AppSuccessResponse[Notification]](consumedElement.value), remainingOrDefault)
        .response
        .result

    assert(result.notificationId == notificationId1)
    assert(result.orgId == orgId1)
    assert(result.siteId == siteId1)
    probe1.cancel()
  }

  it should "give error for get Notification with Invalid Notification Id" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withNotificationProps(NotificationProps(Some(notificationId_random)))
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withReqType(NOTIFICATION_FOR_ID.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result          = Await.result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault).success
    assert(!result)

    probe1.cancel()
  }

  it should "return Notification for get Notification Sys" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withNotificationProps(NotificationProps(Some(notificationId1)))
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withReqType(NOTIFICATION_SYS.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[AppSuccessResponse[Notification]](consumedElement.value), remainingOrDefault)
        .response
        .result

    assert(result.notificationId == notificationId1)
    assert(result.orgId == orgId1)
    assert(result.siteId == siteId1)
    probe1.cancel()
  }

  it should "give error for get Notification Sys with Invalid Notification Id" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withNotificationProps(NotificationProps(Some(notificationId1)))
      .withReqType(NOTIFICATION_SYS.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result          = Await.result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault).success
    assert(!result)

    probe1.cancel()
  }

  it should "return Notification for get Notification By Name" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withNotificationProps(NotificationProps(None, Some(notificationName)))
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withReqType(NOTIFICATION_FOR_NAME.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[AppSuccessResponse[List[Notification]]](consumedElement.value), remainingOrDefault)
        .response
        .result
    assert(result.nonEmpty)
    result.foreach(item => {
      assert(item.name.getOrElse("") == notificationName)
    })
    probe1.cancel()
  }

  it should "give error for get Notification By Name with Invalid Notification Name" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withNotificationProps(NotificationProps(None, Some("NoName")))
      .withReqType(NOTIFICATION_FOR_NAME.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result          = Await.result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault).success
    assert(!result)

    probe1.cancel()
  }

  it should "activate Notification with Valid Notification Id" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withNotificationProps(NotificationProps(Some(notificationId_inactive)))
      .withOrgId(orgId_inactive)
      .withSiteId(siteId_inactive)
      .withReqType(ACTIVATE_NOTIFICATION.value)
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

  it should "give error when activating Notification with Invalid Notification Id" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withNotificationProps(NotificationProps(Some(notificationId_random)))
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withReqType(ACTIVATE_NOTIFICATION.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result          = Await.result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault).success
    assert(!result)

    probe1.cancel()
  }

  it should "deactivate Notification with Valid Notification Id" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withNotificationProps(NotificationProps(Some(notificationId1)))
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withReqType(DEACTIVATE_NOTIFICATION.value)
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

  it should "give error when de-activating Notification with Invalid Notification Id" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withNotificationProps(NotificationProps(Some(notificationId_random)))
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withReqType(DEACTIVATE_NOTIFICATION.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result          = Await.result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault).success
    assert(!result)

    probe1.cancel()
  }

  it should "create Notification with valid Notification payload for sensity user" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withNotificationProps(getNotificationProps)
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withUserType(Some("sensity"))
      .withReqType(ADD_NOTIFICATION.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[AppSuccessResponse[Notification]](consumedElement.value), remainingOrDefault)
        .response
        .result

    assert(result.name.getOrElse("") == notificationName)
    assert(result.description.getOrElse("") == "NewDescription")
    assert(result.severity == notificationSeverity)
    assert(result.notificationType == notificationType)
    assert(result.orgId == orgId1)
    assert(result.siteId == siteId1)
    probe1.cancel()
  }

  it should "create Notification with valid Notification payload for partner" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withNotificationProps(getNotificationPropsForPartner)
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withUserType(Some("partner"))
      .withReqType(ADD_NOTIFICATION.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[AppSuccessResponse[Notification]](consumedElement.value), remainingOrDefault)
        .response
        .result

    assert(result.name.getOrElse("") == notificationName)
    assert(result.description.getOrElse("") == "NewDescription")
    assert(result.notificationType == notificationTypeForPartner)
    assert(result.severity == notificationSeverity)
    assert(result.orgId == orgId1)
    assert(result.siteId == siteId1)
    probe1.cancel()
  }

  it should "give error for creating Notification with Invalid Notification Type for sensity" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withNotificationProps(getNotificationPropsInvalidForSensity)
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withUserType(Some("sensity"))
      .withReqType(ADD_NOTIFICATION.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result          = Await.result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault).success
    assert(!result)
    probe1.cancel()
  }

  it should "give error for creating Notification with Invalid Notification Type for partner" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withNotificationProps(getNotificationPropsInvalidForPartner)
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withUserType(Some("partner"))
      .withReqType(ADD_NOTIFICATION.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result          = Await.result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault).success
    assert(!result)
    probe1.cancel()
  }

  it should "give error for creating Notification with Invalid Notification payload" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withNotificationProps(getNotificationPropsInvalid)
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withUserType(Some("sensity"))
      .withReqType(ADD_NOTIFICATION.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result          = Await.result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault).success
    assert(!result)
    probe1.cancel()
  }

  it should "give error for creating Notification with Invalid Notification type" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withNotificationProps(getNotificationPropsInvalid)
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withUserType(Some("sensity"))
      .withReqType(ADD_NOTIFICATION.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result          = Await.result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault).success
    assert(!result)
    probe1.cancel()
  }

  it should "give error for creating Notification with Invalid Notification severity" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withNotificationProps(getNotificationPropsInvalidSeverity)
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withUserType(Some("sensity"))
      .withReqType(ADD_NOTIFICATION.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result          = Await.result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault).success
    assert(!result)
    probe1.cancel()
  }

  it should "give error for creating Notification with Invalid Hold Off" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withNotificationProps(getNotificationPropsInvalidHoldOff)
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withUserType(Some("sensity"))
      .withReqType(ADD_NOTIFICATION.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result          = Await.result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault).success
    assert(!result)
    probe1.cancel()
  }

  it should "update Notification with valid Notification payload" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withNotificationProps(getNotificationPropsForUpdate)
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withUserType(Some("sensity"))
      .withReqType(UPDATE_NOTIFICATION.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[AppSuccessResponse[Notification]](consumedElement.value), remainingOrDefault)
        .response
        .result
    assert(result.name.getOrElse("") == notificationName)
    assert(result.description.getOrElse("") == "UpdatedDescription")
    assert(result.severity == notificationSeverity)
    assert(result.orgId == orgId1)
    assert(result.siteId == siteId1)
    probe1.cancel()
  }

  it should "give error when updating Notification with invalid Notification type for partner" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withNotificationProps(getNotificationPropsForUpdate)
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withUserType(Some("partner"))
      .withReqType(UPDATE_NOTIFICATION.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result          = Await.result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault).success
    assert(!result)
    probe1.cancel()
  }

  it should "give error for updating Notification with Invalid Notification Id" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withNotificationProps(getNotificationProps)
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withUserType(Some("sensity"))
      .withReqType(UPDATE_NOTIFICATION.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result          = Await.result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault).success
    assert(!result)
    probe1.cancel()
  }

  it should "delete Notification with Valid Notification Id" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withNotificationProps(NotificationProps(Some(notificationId1)))
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withReqType(DELETE_NOTIFICATION.value)
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

  it should "give error when deleting Notification with Invalid Notification Id" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withNotificationProps(NotificationProps(Some(notificationId_random)))
      .withOrgId(orgId1)
      .withSiteId(siteId1)
      .withReqType(DELETE_NOTIFICATION.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result          = Await.result(fromJsonAsync[FailureResponseBody](consumedElement.value), remainingOrDefault).success
    assert(!result)

    probe1.cancel()
  }

  it should "get Notification Types for Customer" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withUserType(Some("customer"))
      .withReqType(GET_NOTIFICATION_TYPES.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[AppSuccessResponse[List[NotificationType]]](consumedElement.value), remainingOrDefault)
        .response
        .result
    assert(result.nonEmpty)
    assert(result.length == 3)
    result.foreach(item => {
      assert(item.ufName != null)
      assert(item.alarmType != null)
      assert(item.nodeModels.nonEmpty)
    })
    probe1.cancel()
  }

  it should "get Notification Types for Partner" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withUserType(Some("partner"))
      .withReqType(GET_NOTIFICATION_TYPES.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[AppSuccessResponse[List[NotificationType]]](consumedElement.value), remainingOrDefault)
        .response
        .result
    assert(result.nonEmpty)
    assert(result.length == 2)
    result.foreach(item => {
      assert(item.ufName != null)
      assert(item.alarmType != null)
      assert(item.nodeModels.nonEmpty)
    })
    probe1.cancel()
  }

  it should "get Notification Types for Sensity" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withUserType(Some("sensity"))
      .withReqType(GET_NOTIFICATION_TYPES.value)
      .withResponseTopic(responseTopic)
      .build
    Await.result(produceEvent(requestTopic, request), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val result =
      Await
        .result(fromJsonAsync[AppSuccessResponse[List[NotificationType]]](consumedElement.value), remainingOrDefault)
        .response
        .result
    assert(result.nonEmpty)
    assert(result.length == 3)
    result.foreach(item => {
      assert(item.ufName != null)
      assert(item.alarmType != null)
      assert(item.nodeModels.nonEmpty)
    })
    probe1.cancel()
  }

  it should "give error for get Notification Types of user type is missing in the payload" in new KafkaSettings {
    val request = NotificationRequestBuilder()
      .withReqType(GET_NOTIFICATION_TYPES.value)
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

  it should "produce a sample Request without notification props and the exception should be thrown" in new KafkaSettings {

    an[InvalidRequestPayloadException] must be thrownBy RequestValidator.validationPredicate(
      appReq_NoNotificationProps
    )
  }

  it should "produce a sample Request without org props and the exception should be thrown" in new KafkaSettings {

    an[InvalidRequestPayloadException] must be thrownBy RequestValidator.validationPredicate(appReq_NoOrgProps)
  }

  it should "produce a sample Request without site props and the exception should be thrown" in new KafkaSettings {

    an[InvalidRequestPayloadException] must be thrownBy RequestValidator.validationPredicate(appReq_NoSiteProps)
  }

}
