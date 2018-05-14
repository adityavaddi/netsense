
package com.vz.nsp.parking.userdataservice.service

import java.util.UUID

import akka.actor.{ActorRef, ActorSystem}
import akka.kafka.ProducerMessage.Message
import akka.kafka.scaladsl.Consumer.Control
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.kafka.{ConsumerSettings, ProducerSettings, Subscriptions}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.stream.testkit.TestSubscriber
import akka.stream.testkit.TestSubscriber.Probe
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestKit
import akka.{Done, NotUsed}
import com.outworkers.phantom.dsl.ResultSet
import com.verizon.netsense.connector.CassandraConnector
import com.vz.nsp.parking.db.{EmbeddedDatabase, PhantomService, ProductionDatabase}
import com.vz.nsp.parking.dblayer.DbLayer
import com.vz.nsp.parking.model._
import com.vz.nsp.parking.config.{KafkaConfig, KafkaConnector}
import com.vz.nsp.parking.userdataservice.constants.TestConstants.{updateUserData, _}
import com.vz.nsp.parking.userdataservice.constants.TestDataFeed._
import com.vz.nsp.parking.userdataservice.db.UserDataConnector
import com.vz.nsp.parking.userdataservice.helper.{ReqResGenerator, StreamHelper, UserDataHelper}
import com.vz.nsp.parking.util.{AkkaStreamUtil, ObjectMapperUtil}
import net.manub.embeddedkafka.{EmbeddedKafka, EmbeddedKafkaConfig}
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer, StringDeserializer}
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FlatSpecLike, MustMatchers}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

class UserDataServiceTest
  extends TestKit(ActorSystem("UserDataService-System"))
    with FlatSpecLike
    with MustMatchers
    with BeforeAndAfterAll
    with BeforeAndAfterEach
    with EmbeddedDatabase {
  implicit val ec = ExecutionContext.Implicits.global
  type ActorMaterializerImpl = {
    def system: ActorSystem
    def supervisor: ActorRef
  }
  val dbLayer = new DbLayer(new PhantomService with ProductionDatabase {})
  val reqRes = new ReqResGenerator()
  val userDataHelper = new UserDataHelper(dbLayer, reqRes)
  val streamHelper = new StreamHelper(userDataHelper, reqRes)
  val userDataService = new UserDataService(streamHelper)
  implicit val embeddedKafkaConfig = EmbeddedKafkaConfig(kafkaPort = 8767, zooKeeperPort = 8768)
  val bootstrapServers = s"localhost:${embeddedKafkaConfig.kafkaPort}"
  implicit val mat = ActorMaterializer()(system)
  implicit val stageStoppingTimeout = StageStoppingTimeout(20.seconds)

  def createEventProbe(consumerSettings: ConsumerSettings[Array[Byte], Array[Byte]],
                       topic: String): TestSubscriber.Probe[Array[Byte]] =
    Consumer
      .plainSource(consumerSettings, Subscriptions.topics(topic))
      .map(_.value)
      .runWith(TestSink.probe)

  def createEventConsumerSettings(group: String): ConsumerSettings[Array[Byte], Array[Byte]] =
    KafkaConnector.configKafkaConsumerSettings(bootstrapServers,
      group,
      new ByteArrayDeserializer,
      new ByteArrayDeserializer)

  override def beforeAll(): Unit = {
    EmbeddedKafka.start()
    DBServiceUserData.init()
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    EmbeddedKafka.stop()
    DBServiceUserData.cleanup()
    super.afterAll()
    system.terminate()
  }

  trait KafkaConfiguration {
    val partition0 = 0
    val group1 = createGroup

    def createTopic: String = "topic-" + UUID.randomUUID().toString

    def createGroup: String = "group-" + UUID.randomUUID().toString

    def createEventConsumerSettingsString(group: String): ConsumerSettings[String, String] =
      KafkaConnector.configKafkaConsumerSettings(bootstrapServers,
        group,
        new StringDeserializer,
        new StringDeserializer)

    def createEventConsumerSettings(group: String): ConsumerSettings[Array[Byte], Array[Byte]] =
      KafkaConnector.configKafkaConsumerSettings(bootstrapServers,
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
      KafkaConnector.configKafkaProducerSettings(bootstrapServers, new ByteArraySerializer, new ByteArraySerializer)
  }

  trait KafkaSettingsQuery extends KafkaConfiguration {
    lazy val testTopic: String = createTopic
    lazy val testGroup: String = createGroup
    lazy val requestTopic: String = createTopic
    lazy val responseTopic: String = createTopic
    private implicit val setupKafkaConfig =
      KafkaConfig.setupEmbeddedKafkaConfig(bootstrapServers, testGroup, testTopic, requestTopic, responseTopic)
  }

  final case class StageStoppingTimeout(time: FiniteDuration)

  object DBServiceUserData {
    implicit val ec = scala.concurrent.ExecutionContext
    implicit val session = CassandraConnector.testConnector.session
    implicit val executor = system.dispatcher

    def init() = {
      Await.ready(UserDataConnector.userdataTestTable.createTable(), 5.seconds)
    }

    def cleanup() = {
      Await.ready(UserDataConnector.userdataTestTable.truncateTable(), 5.seconds)
    }
  }

  it should "start the embedded kafka & close the probe" in new KafkaSettingsQuery {
    val probe = createEventProbe(createEventConsumerSettings(testGroup), testTopic)
    probe.ensureSubscription()
    probe.cancel()

  }

  it should "make the message flow thru Json unwrap flowShape" in new KafkaSettingsQuery {

    val testEvent = generateAppRequestWithHeaders(responseTopic)
    Await.result(produceEvent(testTopic, testEvent), remainingOrDefault)
    val consumerUnderTest: Source[ConsumerRecord[String, String], Control] =
      AkkaStreamUtil.configKafkaSource(createEventConsumerSettingsString(testGroup), Set(testTopic))
    val flowUnderTest = userDataService.msgUnmarshallingFlow
    val testSink = TestSink.probe[AppRequest]

    def createEventProbe: Probe[AppRequest] =
      consumerUnderTest.via(flowUnderTest).runWith(testSink)

    val probe = createEventProbe
    probe.request(1)
    val consumedMsg = probe.requestNext(10.second)
    consumedMsg must equal(testEvent)
    probe.cancel()
  }

  /* API: getUserDataProcess */
  it should "Get userdata from database" in new KafkaSettingsQuery {

    Await.result(database.UserDataTable.store(userData), remainingOrDefault)
    val appRequest = generateAppRequestWithHeader(responseTopic, getUserData, userDataId, userDataRequest)
    val flowUnderTest = userDataService.userDataQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe1 = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[UserData]](consumedElement._2)
    actualOutput.response.result mustBe userData
    actualOutput.response.success mustBe true
    probe1.cancel()
  }

  it should "Fail to Get a userdata from database" in new KafkaSettingsQuery {

    Await.result(database.UserDataTable.store(userData), remainingOrDefault)
    val userDataIdForFail = UUID.randomUUID().toString
    val appRequest = generateAppRequestWithHeader(responseTopic, getUserData, userDataIdForFail, userDataRequest)
    val flowUnderTest = userDataService.userDataQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe1 = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.status mustBe 404
    actualOutput.response.success mustBe false
    probe1.cancel()

  }


  /* method: getAllUserDataProcess */
  it should "Get all userdata from Database" in new KafkaSettingsQuery {

    DBServiceUserData.cleanup()
    Await.result(database.UserDataTable.store(userData), remainingOrDefault)
    Await.result(database.UserDataTable.store(userDataone), remainingOrDefault)
    val appRequest = generateAppRequestWithHeader(responseTopic, getAllUserData, userDataId, userDataRequest)
    val flowUnderTest = userDataService.userDataQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe1 = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[List[UserData]]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.size mustBe 2
    probe1.cancel()
  }

  it should "Return Empty UserData List: Get all UserData in a Database" in new KafkaSettingsQuery {

    DBServiceUserData.cleanup()
    Await.result(database.UserDataTable.store(userData), remainingOrDefault)
    Await.result(database.UserDataTable.store(userDataone), remainingOrDefault)
    val appid = UUID.randomUUID().toString
    val userid = UUID.randomUUID().toString
    val appRequest = generateAppRequestWithHeadersForGetAllUserDataFailure(responseTopic, getAllUserData, userDataId, appid, userid)
    val flowUnderTest = userDataService.userDataQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[List[UserData]]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.size mustBe 0
    probe.cancel()

  }

  /* method: postUserDataProcess */
  it should "Create userdata in database" in new KafkaSettingsQuery {

    val appRequest = generateAppRequestWithHeader(responseTopic, postUserData, userDataId, userDataRequest)
    val flowUnderTest = userDataService.userDataQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe1 = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[UserData]](consumedElement._2)
    actualOutput.response.success mustBe true
    probe1.cancel()
  }

  it should "Fail to Create a userdata in database" in new KafkaSettingsQuery {

    val appRequest = generateAppRequestWithHeadersForCreatePolicyFailure(responseTopic, postUserData, userDataId)
    val flowUnderTest = userDataService.userDataQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.error.eq("Required userId missing in request")
    actualOutput.response.status mustBe 404
    actualOutput.response.success mustBe false
    probe.cancel()

  }


  /* method: deleteUserDataProcess */
  it should "Delete userdata in Database" in new KafkaSettingsQuery {

    Await.result(database.UserDataTable.store(userData), remainingOrDefault)
    val appRequest = generateAppRequestWithHeader(responseTopic, deleteUserData, userDataId, userDataRequest)
    val flowUnderTest = userDataService.userDataQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe1 = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[SuccessMessage]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result mustBe SuccessMessage(true)
    probe1.cancel()
  }

  it should "Fail to Delete a userdata in a database" in new KafkaSettingsQuery {

    Await.result(database.UserDataTable.store(userData), remainingOrDefault)
    val userDataIdForFail = UUID.randomUUID().toString
    val appRequest = generateAppRequestWithHeader(responseTopic, deleteUserData, userDataIdForFail, userDataRequest)
    val flowUnderTest = userDataService.userDataQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.status mustBe 404
    probe.cancel()

  }

  /* method: updateUserDataProcess */
  it should "Update userdata in Database" in new KafkaSettingsQuery {

    Await.result(database.UserDataTable.store(userData), remainingOrDefault)
    val appRequest = generateAppRequestWithHeader(responseTopic, updateUserData, userDataId, updatedDataRequest)
    val flowUnderTest = userDataService.userDataQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe1 = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[UserData]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.datavalue mustBe updatedDataRequest.datavalue
    probe1.cancel()
  }

  it should "Fail to Update userdata" in new KafkaSettingsQuery {

    Await.result(database.UserDataTable.store(userData), remainingOrDefault)
    val userDataIdForFail = UUID.randomUUID().toString
    val appRequest = generateAppRequestWithHeader(responseTopic, updateUserData, userDataIdForFail, updatedDataRequest)
    val flowUnderTest = userDataService.userDataQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe1 = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.status mustBe 404
    probe1.cancel()

  }

  it should "Throw error for unknown casel request type" in new KafkaSettingsQuery {

    Await.result(database.UserDataTable.store(userData), remainingOrDefault)
    val appRequest = generateAppRequestWithHeader(responseTopic, "unknown type", userDataId, userDataRequest)
    val flowUnderTest = userDataService.userDataQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe1 = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[UserData]](consumedElement._2)
    actualOutput.response.success mustBe false
    probe1.cancel()
  }

  it should "Throw error for random data id" in new KafkaSettingsQuery {

    Await.result(database.UserDataTable.store(userData), remainingOrDefault)
    val appRequest = generateAppRequestWithHeader(responseTopic, getUserData, "randomDataId", userDataRequest)
    val flowUnderTest = userDataService.userDataQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe1 = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[UserData]](consumedElement._2)
    actualOutput.response.success mustBe false
    probe1.cancel()
  }

  ignore should "Consume app request from kafka and can produce the app response to kafka" in new KafkaSettingsQuery {

    Await.result(database.UserDataTable.store(userData), remainingOrDefault)
    val testEvent = generateAppRequestWithHeader(responseTopic, getUserData, userDataId, userDataRequest)
    Await.result(produceEvent(testTopic, testEvent), remainingOrDefault)
    val consumerUnderTest =
      AkkaStreamUtil.configKafkaSource(createEventConsumerSettingsString(group1), Set(testTopic))

    val msgUnmarshallFlowUnderTest = userDataService.msgUnmarshallingFlow

    val flowUnderTest = userDataService.userDataQueryFlow

    val kafkaSinkUnderTest = userDataService.responseKafkaSink

    val (killSwitch, done) = consumerUnderTest
      .via(msgUnmarshallFlowUnderTest)
      .via(flowUnderTest)
      .toMat(kafkaSinkUnderTest)(Keep.both)
      .run()

    val kafkaConsumerUnderTest =
      AkkaStreamUtil.configKafkaSource(createEventConsumerSettingsString(group1), Set(responseTopic))
    val probe = kafkaConsumerUnderTest.toMat(TestSink.probe)(Keep.right).run()
    probe.request(1)
    val consumedMsg = probe.requestNext(30.seconds)
    val appresponse = ObjectMapperUtil.fromJson[AppSuccessResponse[UserData]](consumedMsg.value())
    appresponse.response.result mustEqual UserData
    probe.cancel()
    killSwitch.shutdown()

  }

}

