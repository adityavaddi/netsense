package com.verizon.netsense.whatifservice.flow

import java.util.UUID

import akka.{Done, NotUsed}
import com.verizon.netsense.whatifservice.db._
import akka.testkit.TestKit
import akka.actor.ActorSystem
import akka.kafka.ProducerMessage.Message
import akka.kafka.{ConsumerSettings, ProducerSettings, Subscriptions}
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.stream.testkit.TestSubscriber
import akka.stream.testkit.scaladsl.TestSink
import com.outworkers.phantom.dsl._
import com.verizon.netsense.whatifservice.util.{BaseSpec, ObjectMapperUtil, RequestResponseGenerator}

import scala.concurrent.duration._
import com.verizon.netsense.whatifservice.model.ResponseMessage.{Does_Not_Exist_OrgSite, NotFound_WhatIfJobId_In_db, Siteid}
import com.verizon.netsense.whatifservice.service.{SparkJobRESTService, WhatIfService}
import com.verizon.netsense.whatifservice.validator.WhatIfRequestValidator
import net.manub.embeddedkafka.EmbeddedKafkaConfig
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer, StringDeserializer}

import scala.concurrent.{Await, ExecutionContext, Future}
import com.verizon.netsense.whatifservice.data.TestDataFeed._
import com.verizon.netsense.whatifservice.model.casel.{AppFailureResponse, AppRequest, AppSuccessResponse}
import org.apache.kafka.clients.producer.ProducerRecord
import com.verizon.netsense.whatifservice.model.{WhatIfJobResponse, WhatIfSparkRequest}
import org.apache.cassandra.service.{CassandraDaemon, EmbeddedCassandraService}

/**
 * Created by maleva on 4/21/18.
 */
class RestApiFlowSpec extends TestKit(ActorSystem("WhatIf-Test-System")) with BaseSpec with EmbeddedDatabase {

  implicit val ec = ExecutionContext.Implicits.global

  implicit val mat = ActorMaterializer()(system)

  lazy val embeddedCassandraServiceObj = new EmbeddedCassandraService
  lazy val cassandraDaemon             = new CassandraDaemon

  object DatabaseService {
    def init() = {
      val create = Future.sequence(
        List(
          database.WhatIfJobTable.create.ifNotExists().future()
        )
      )
      Await.ready(create, 5.seconds)
    }

    def cleanup(): Future[List[ResultSet]] = {
      val truncate = Future.sequence(
        List(
          database.WhatIfJobTable.truncate.future()
        )
      )
      Await.ready(truncate, 5.seconds)
    }
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    val start: Unit = embeddedCassandraServiceObj.start()
    DatabaseService.init()
    session.init()

  }

  override protected def afterAll() = {
    DatabaseService.cleanup()
    cassandraDaemon.stop()
    cassandraDaemon.stopNativeTransport()
    super.afterAll()
  }

  val kafkaConnector = new KafkaConnector()
  val reqRes         = new RequestResponseGenerator
  val whatIfDbLayer = new WhatIfDbLayer(new PhantomService {
    override def database: WhatIfServicesDb = EmbeddedDb
  })
  val sparkReqRes            = new SparkJobRESTService(reqRes, whatIfDbLayer)
  val whatIfService          = new WhatIfService(whatIfDbLayer, reqRes, sparkReqRes)
  val whatIfRequestValidator = new WhatIfRequestValidator(kafkaConnector)
  val restApiFlow            = new RestApiFlow(whatIfService, reqRes)
  val whatIfFlow             = new WhatIfFlow(restApiFlow, whatIfRequestValidator, sparkReqRes)

  implicit val embeddedKafkaConfig  = EmbeddedKafkaConfig(kafkaPort = 8767, zooKeeperPort = 8768)
  val bootstrapServers              = s"localhost:${embeddedKafkaConfig.kafkaPort}"
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

  val errorMessage = NotFound_WhatIfJobId_In_db.value + jobId + Does_Not_Exist_OrgSite.value + orgId + Siteid.value + siteId

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
  }

  "RestApiFlow" should "start the embedded kafka & close the probe" in new KafkaSettingsQuery {
    val probe = createEventProbe(createEventConsumerSettings(testGroup), testTopic)
    probe.ensureSubscription()
    probe.cancel()

  }

  it should "Get WhatIF jobs from Database" in new KafkaSettingsQuery {
    DatabaseService.cleanup()
    Await.result(database.WhatIfJobTable.storeJobStatus(whatIf), remainingOrDefault)

    val getAllAppRequest = appRequest("getAllWhatIfJobs")
    val flowUnderTest    = whatIfFlow.whatIfProcessRequestFlow
    val sourceMock       = Source.single(getAllAppRequest).map(x => x)
    val probe1           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement  = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[List[WhatIfJobResponse]]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.size mustBe 1
    probe1.cancel()
  }

  it should "Get Empty WhatIf job from Database" in new KafkaSettingsQuery {
    DatabaseService.cleanup()
    val getAllAppRequest = appRequest("getAllWhatIfJobs")
    val flowUnderTest    = whatIfFlow.whatIfProcessRequestFlow
    val sourceMock       = Source.single(getAllAppRequest).map(x => x)
    val probe1           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement: (AppRequest, String, WhatIfSparkRequest) = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[List[WhatIfJobResponse]]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.size mustBe 0
    probe1.cancel()
  }

  it should "Get WhatIf by Id Database" in new KafkaSettingsQuery {
    DatabaseService.cleanup()
    Await.result(database.WhatIfJobTable.storeJobStatus(whatIf), remainingOrDefault)

    val getAllAppRequest = appRequest("getWhatIfJob")
    val flowUnderTest    = whatIfFlow.whatIfProcessRequestFlow
    val sourceMock       = Source.single(getAllAppRequest).map(x => x)
    val probe1           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement  = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[WhatIfJobResponse]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.jobId mustBe jobId
    probe1.cancel()
  }

  it should "Get WhatIf by Id Database error case" in new KafkaSettingsQuery {
    DatabaseService.cleanup()
    val getAllAppRequest = appRequest("getWhatIfJob")
    val flowUnderTest    = whatIfFlow.whatIfProcessRequestFlow
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
}
