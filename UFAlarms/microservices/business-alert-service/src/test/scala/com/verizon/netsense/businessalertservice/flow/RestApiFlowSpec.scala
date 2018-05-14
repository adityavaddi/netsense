package com.verizon.netsense.businessalertservice.flow

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
import com.verizon.netsense.businessalertservice.config.KafkaConfig
import com.verizon.netsense.businessalertservice.data.TestDataFeed._
import com.verizon.netsense.businessalertservice.db._
import com.verizon.netsense.businessalertservice.model.ResponseMessage.{apply => _, _}
import com.verizon.netsense.businessalertservice.model._
import com.verizon.netsense.businessalertservice.service.BusinessAlertService
import com.verizon.netsense.businessalertservice.util.{BARequestResponseGenerator, BaseSpec, ObjectMapperUtil}
import com.verizon.netsense.businessalertservice.validator.BusinessAlertRequestValidator
import net.manub.embeddedkafka.EmbeddedKafkaConfig
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer, StringDeserializer}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

class RestApiFlowSpec extends TestKit(ActorSystem("Business-Test-System")) with BaseSpec with ProductionDatabase {

  object DatabaseService {
    def init() = {
      val create = Future.sequence(
        List(
          database.BusinessAlertTable.create.ifNotExists().future(),
          database.BusinessAlertHistoryTable.create.ifNotExists().future(),
          database.BusinessTriggerTable.create.ifNotExists().future()
        )
      )
      Await.ready(create, 5.seconds)
    }

    def cleanup(): Future[List[ResultSet]] = {
      val truncate = Future.sequence(
        List(
          database.BusinessAlertTable.truncate.future(),
          database.BusinessAlertHistoryTable.truncate().future(),
          database.BusinessTriggerTable.truncate().future()
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
  val reqRes         = new BARequestResponseGenerator

  val businessAlertsDbLayer         = new BusinessAlertsDbLayer(new BAPhantomService with ProductionDatabase {})
  val bAService                     = new BusinessAlertService(businessAlertsDbLayer, reqRes)
  val businessAlertRequestValidator = new BusinessAlertRequestValidator(kafkaConnector)
  val restApiFlow                   = new RestApiFlow(bAService, reqRes)
  val businessAlertFlow             = new BusinessAlertFlow(restApiFlow, businessAlertRequestValidator)

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

  val errorMessage = NotFound_BusinessAlertId_In_db.value + businessAlertId + Does_Not_Exist_OrgSite.value + orgId + Siteid.value + siteId

  val sysErrorMessage = "Cannot access inactive/acknowledged business alert"

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
    lazy val testTopic: String     = createTopic
    lazy val testGroup: String     = createGroup
    lazy val requestTopic: String  = createTopic
    lazy val responseTopic: String = createTopic
    private implicit val setupKafkaConfig =
      KafkaConfig.setupEmbeddedKafkaConfig(bootstrapServers, testGroup, testTopic, requestTopic, responseTopic)
  }

  "RestApiFlow" should "start the embedded kafka & close the probe" in new KafkaSettingsQuery {
    val probe = createEventProbe(createEventConsumerSettings(testGroup), testTopic)
    probe.ensureSubscription()
    probe.cancel()

  }

  it should "Get BusinessAlerts from Database" in new KafkaSettingsQuery {
    DatabaseService.cleanup()
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlert(businessAlertId, "Minor")),
                 remainingOrDefault)
    Await.result(database.BusinessTriggerTable.storeTrigger(buildTrigger(triggerId, List(resourceId))),
                 remainingOrDefault)
    val getAllAppRequest = generateAppRequest(responseTopic, "getAllBusinessAlerts")
    val flowUnderTest    = businessAlertFlow.businessAlertProcessRequestFlow
    val sourceMock       = Source.single(getAllAppRequest).map(x => x)
    val probe1           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement  = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[List[BusinessAlertResponse]]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.size mustBe 1
    probe1.cancel()
  }

  it should "Get Empty BusinessAlert from Database" in new KafkaSettingsQuery {
    DatabaseService.cleanup()

    val getAllAppRequest = generateAppRequest(responseTopic, "getAllBusinessAlerts")
    val flowUnderTest    = businessAlertFlow.businessAlertProcessRequestFlow
    val sourceMock       = Source.single(getAllAppRequest).map(x => x)
    val probe1           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement  = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[List[BusinessAlertResponse]]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.size mustBe 0
    probe1.cancel()
  }

  it should "Get BusinessAlert by Id Database" in new KafkaSettingsQuery {
    DatabaseService.cleanup()
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlert(businessAlertId, "Minor")),
                 remainingOrDefault)

    val getAllAppRequest = generateAppRequest(responseTopic, "getBusinessAlert")
    val flowUnderTest    = businessAlertFlow.businessAlertProcessRequestFlow
    val sourceMock       = Source.single(getAllAppRequest).map(x => x)
    val probe1           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement  = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[BusinessAlertResponse]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.businessAlertId mustBe businessAlertId
    probe1.cancel()
  }

  it should "Get BusinessAlert by Id Database error case" in new KafkaSettingsQuery {
    DatabaseService.cleanup()
    val getAllAppRequest = generateAppRequest(responseTopic, "getBusinessAlert")
    val flowUnderTest    = businessAlertFlow.businessAlertProcessRequestFlow
    val sourceMock       = Source.single(getAllAppRequest).map(x => x)
    val probe1           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement  = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.status mustBe 404
    actualOutput.response.error mustBe errorMessage
    probe1.cancel()
  }

  it should "Get BusinessAlert Sys by Id Database" in new KafkaSettingsQuery {
    DatabaseService.cleanup()
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlert(businessAlertId, "Minor")),
                 remainingOrDefault)
    Await.result(database.BusinessTriggerTable.storeTrigger(buildTrigger(triggerId, List("res1"))), remainingOrDefault)
    val sysAppRequest   = generateAppRequestForSys(businessAlertId, responseTopic)
    val flowUnderTest   = businessAlertFlow.businessAlertProcessRequestFlow
    val sourceMock      = Source.single(sysAppRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[BusinessAlertResponse]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.businessAlertId mustBe businessAlertId
    probe1.cancel()
  }

  it should "Get BusinessAlert Sys by Id Database error case" in new KafkaSettingsQuery {
    DatabaseService.cleanup()
    Await.result(
      database.BusinessAlertTable
        .storeBusinessAlert(businessAlert(businessAlertId, "Minor").copy(active = Some(false))),
      remainingOrDefault
    )
    Await.result(database.BusinessTriggerTable.storeTrigger(buildTrigger(triggerId, List("res1"))), remainingOrDefault)
    val sysAppRequest   = generateAppRequestForSys(businessAlertId, responseTopic)
    val flowUnderTest   = businessAlertFlow.businessAlertProcessRequestFlow
    val sourceMock      = Source.single(sysAppRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.status mustBe 404
    actualOutput.response.error mustBe sysErrorMessage
    probe1.cancel()
  }

  it should "Dismiss BusinessAlert by Id Database Success Case" in new KafkaSettingsQuery {
    DatabaseService.cleanup()
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlert(businessAlertId, "Minor")),
                 remainingOrDefault)

    val getAllAppRequest = generateAppRequest(responseTopic, "dismissBusinessAlert")
    val flowUnderTest    = businessAlertFlow.businessAlertProcessRequestFlow
    val sourceMock       = Source.single(getAllAppRequest).map(x => x)
    val probe1           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement  = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[SuccessMessage]](consumedElement._2)
    actualOutput.response.success mustBe true
    probe1.cancel()
  }

  it should "Dismiss BusinessAlert by Id Database error case" in new KafkaSettingsQuery {
    DatabaseService.cleanup()
    val getAllAppRequest = generateAppRequest(responseTopic, "dismissBusinessAlert")
    val flowUnderTest    = businessAlertFlow.businessAlertProcessRequestFlow
    val sourceMock       = Source.single(getAllAppRequest).map(x => x)
    val probe1           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement  = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.status mustBe 404
    actualOutput.response.error mustBe errorMessage
    probe1.cancel()
  }

  it should "Dismiss BusinessAlert by Id already dismissed alert case" in new KafkaSettingsQuery {
    DatabaseService.cleanup()
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlert(businessAlertId, "Clear")),
                 remainingOrDefault)

    val getAllAppRequest = generateAppRequest(responseTopic, "dismissBusinessAlert")
    val flowUnderTest    = businessAlertFlow.businessAlertProcessRequestFlow
    val sourceMock       = Source.single(getAllAppRequest).map(x => x)
    val probe1           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement  = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[SuccessWithMessage]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.success mustBe false
    actualOutput.response.result.success.equals(s"Alert with businessAlertId $businessAlertId was dismissed earlier")
    probe1.cancel()
  }

  it should "Search BusinessAlerts from Database" in new KafkaSettingsQuery {
    DatabaseService.cleanup()
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlert(businessAlertId, "Minor")),
                 remainingOrDefault)
    Await.result(database.BusinessTriggerTable.storeTrigger(buildTrigger(triggerId, List(resourceId))),
                 remainingOrDefault)
    val getAllAppRequest = generateAppRequest(responseTopic, "filterBusinessAlert")
    val flowUnderTest    = businessAlertFlow.businessAlertProcessRequestFlow
    val sourceMock       = Source.single(getAllAppRequest).map(x => x)
    val probe1           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement  = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[List[BusinessAlertResponse]]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.size mustBe 1
    probe1.cancel()
  }

  it should "Get Empty BusinessAlert from Database for search criteria" in new KafkaSettingsQuery {
    DatabaseService.cleanup()

    val getAllAppRequest = generateAppRequestSearch(responseTopic, "filterBusinessAlert", "active eq false")
    val flowUnderTest    = businessAlertFlow.businessAlertProcessRequestFlow
    val sourceMock       = Source.single(getAllAppRequest).map(x => x)
    val probe1           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement  = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[List[BusinessAlertResponse]]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.size mustBe 0
    probe1.cancel()
  }

  it should "Search BusinessAlert by severity" in new KafkaSettingsQuery {
    DatabaseService.cleanup()

    val getAllAppRequest = generateAppRequestSearch(responseTopic, "filterBusinessAlert", "severity eq Minor")
    val flowUnderTest    = businessAlertFlow.businessAlertProcessRequestFlow
    val sourceMock       = Source.single(getAllAppRequest).map(x => x)
    val probe1           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement  = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[List[BusinessAlertResponse]]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.size mustBe 0
    probe1.cancel()
  }

  it should "Search BusinessAlert by wrong condition key" in new KafkaSettingsQuery {
    DatabaseService.cleanup()

    val getAllAppRequest = generateAppRequestSearch(responseTopic, "filterBusinessAlert", "invalidKey ecccq Minor")
    val flowUnderTest    = businessAlertFlow.businessAlertProcessRequestFlow
    val sourceMock       = Source.single(getAllAppRequest).map(x => x)
    val probe1           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement  = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.error mustBe condition_correct.value
    probe1.cancel()
  }

  it should "Search BusinessAlert by invalid key" in new KafkaSettingsQuery {
    DatabaseService.cleanup()

    val getAllAppRequest = generateAppRequestSearch(responseTopic, "filterBusinessAlert", "invalidKey eq Minor")
    val flowUnderTest    = businessAlertFlow.businessAlertProcessRequestFlow
    val sourceMock       = Source.single(getAllAppRequest).map(x => x)
    val probe1           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement  = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.error mustBe invalid_key.value
    probe1.cancel()
  }

  it should "Search BusinessAlert by invalid value" in new KafkaSettingsQuery {
    DatabaseService.cleanup()

    val getAllAppRequest = generateAppRequestSearch(responseTopic, "filterBusinessAlert", "active eq invalidValue")
    val flowUnderTest    = businessAlertFlow.businessAlertProcessRequestFlow
    val sourceMock       = Source.single(getAllAppRequest).map(x => x)
    val probe1           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement  = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.error mustBe Invalid_Active.value
    probe1.cancel()
  }
}
