
package com.vz.nsp.parking.tagservice.service

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
import com.verizon.netsense.connector.CassandraConnector
import com.vz.nsp.parking.config.{KafkaConfig, KafkaConnector}
import com.vz.nsp.parking.db.{EmbeddedDatabase, PhantomService, ProductionDatabase}
import com.vz.nsp.parking.dblayer.DbLayer
import com.vz.nsp.parking.model.ResponseMessage.{Does_Not_Exist_OrgSite, NotFound_TagId, Siteid}
import com.vz.nsp.parking.model._
import com.vz.nsp.parking.tagservice.constants.TestConstants._
import com.vz.nsp.parking.tagservice.constants.TestDataFeed._
import com.vz.nsp.parking.tagservice.db.TagConnector._
import com.vz.nsp.parking.tagservice.helper.{ReqResGenerator, StreamHelper, TagHelper}
import com.vz.nsp.parking.util.{AkkaStreamUtil, ObjectMapperUtil}
import net.manub.embeddedkafka.{EmbeddedKafka, EmbeddedKafkaConfig}
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer, StringDeserializer}
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FlatSpecLike, MustMatchers}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

class TagServiceTest
  extends TestKit(ActorSystem("TagService-System"))
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
  val tagHelper = new TagHelper(dbLayer, reqRes)
  val streamHelper = new StreamHelper(tagHelper, reqRes)
  val tagService = new TagService(streamHelper)
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

  object DBServiceTag {
    implicit val ec = scala.concurrent.ExecutionContext
    implicit val session = CassandraConnector.testConnector.session
    implicit val executor = system.dispatcher

    def init() = {
      Await.ready(policyMappingTable.policyCustomType, 5.seconds)
      Await.ready(policyMappingTable.createPolicy, 5.seconds)
      Await.ready(tagTable.createTable(), 5.seconds)
      Await.ready(WhatIfTagTable.createWhatIfTagTable(), 5.seconds)
    }

    def cleanup() = {
      Await.ready(policyMappingTable.truncatePolicy, 5.seconds)
      Await.ready(tagTable.truncateTable(), 5.seconds)
      Await.ready(WhatIfTagTable.truncateWhatIfTagTable(), 5.seconds)
    }
  }

  override def beforeAll(): Unit = {
    EmbeddedKafka.start()
    DBServiceTag.init()
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    EmbeddedKafka.stop()
    DBServiceTag.cleanup()
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

  "Tag Service" should "start the embedded kafka & close the probe" in new KafkaSettingsQuery {
    val probe = createEventProbe(createEventConsumerSettings(testGroup), testTopic)
    probe.ensureSubscription()
    probe.cancel()

  }

  it should "produce a Sensor sample messages onto embedded kafka & can consume it" in new KafkaSettingsQuery {
    val testEvent = generateAppRequestWithHeaders(responseTopic)
    Await.result(produceEvent(testTopic, testEvent), remainingOrDefault)
    val consumerSettings = createEventConsumerSettings(testGroup)
    val probe = createEventProbe(consumerSettings, testTopic)
    probe.request(1)
    val consumedMsg1 = probe.expectNext()
    val unpackedMessage =
      Await.result(ObjectMapperUtil.fromJsonByteArrayAsync[AppRequest](consumedMsg1), remainingOrDefault)
    unpackedMessage.get must equal(testEvent)
    probe.cancel()
  }

  it should "make the message flow through Json unwrap flowShape" in new KafkaSettingsQuery {

    val testEvent = generateAppRequestWithHeaders(responseTopic)
    Await.result(produceEvent(testTopic, testEvent), remainingOrDefault)
    val consumerUnderTest: Source[ConsumerRecord[String, String], Control] =
      AkkaStreamUtil.configKafkaSource(createEventConsumerSettingsString(testGroup), Set(testTopic))
    val flowUnderTest = tagService.msgUnmarshallingFlow
    val testSink = TestSink.probe[AppRequest]

    def createEventProbe: Probe[AppRequest] =
      consumerUnderTest.via(flowUnderTest).runWith(testSink)

    val probe = createEventProbe
    probe.request(1)
    val consumedMsg = probe.requestNext(10.second)
    consumedMsg must equal(testEvent)
    probe.cancel()
  }

  /* API: getParkingTagProcess */
  it should "Get policy tag from Database by id" in new KafkaSettingsQuery {
    Await.result(database.TagTable.store(tag), remainingOrDefault)
    val appRequest = generateAppRequestWithHeaders(responseTopic, getParkingTag, tagId, tagFromUser)
    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe1 = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[TagResponse]](consumedElement._2)
    actualOutput.response.result mustBe tagResponse
    actualOutput.response.success mustBe true
    probe1.cancel()
  }


  it should "Fail to Get policy tag from Database by id" in new KafkaSettingsQuery {

    Await.result(database.TagTable.store(tag), remainingOrDefault)

    val appRequest = generateAppRequestWithHeadersGetTagFailure(responseTopic, getParkingTag, tagFromUser)

    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.error.eq("Required tagid missing in request")
    actualOutput.response.status mustBe 404
    probe.cancel()
  }


  it should "Fail to Get policy tag from Database by id because of wrong orgid" in new KafkaSettingsQuery {

    Await.result(database.TagTable.store(tag), remainingOrDefault)

    val appRequest = generateAppRequestWithHeadersGetTagFailureWrongOrgIdSiteid(responseTopic, getParkingTag, tag.uid, "orgid", siteId)

    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.error.eq("Required tagid missing in request")
    actualOutput.response.status mustBe 404
    probe.cancel()
  }


  /* API: postParkingTagProcess */

  it should "Create policy tag in Database" in new KafkaSettingsQuery {
    val appRequest = generateAppRequestWithHeaders(responseTopic, postParkingTag, tagId, tagFromUser)
    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe1 = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[Tag]](consumedElement._2)
    actualOutput.response.success mustBe true
    probe1.cancel()
  }

  it should "Fail to Create policy tag in Database" in new KafkaSettingsQuery {

    val appRequest = generateAppRequestWithHeadersGetTagFailure(responseTopic, postParkingTag, tagFromUser)

    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.error.eq("Required tagid missing in request")
    actualOutput.response.status mustBe 404
    probe.cancel()
  }


  /* API: getAllParkingTagProcess */

  it should "Get all policy tag from Database" in new KafkaSettingsQuery {
    DBServiceTag.cleanup()
    Await.result(database.TagTable.store(tag), remainingOrDefault)
    Await.result(database.TagTable.store(tagTwo), remainingOrDefault)
    val appRequest = generateAppRequestWithHeaders(responseTopic, getAllParkingTag, tagId, tagFromUser)
    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe1 = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[List[Tag]]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.size mustBe 2
    probe1.cancel()
  }

  it should "Return Empty Tag List for :- Get all policy tag from Database" in new KafkaSettingsQuery {

    DBServiceTag.cleanup()

    val appRequest = generateAppRequestWithHeadersGetTagFailure(responseTopic, getAllParkingTag, tagFromUser)

    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[List[Tag]]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.size mustBe 0
    probe.cancel()
  }


  /* API: deleteParkingTagProcess */
  it should "Delete policy tag in Database" in new KafkaSettingsQuery {
    Await.result(database.TagTable.store(tag), remainingOrDefault)
    val appRequest = generateAppRequestWithHeaders(responseTopic, deleteParkingTag, tagId, tagFromUser)
    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe1 = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[SuccessMessage]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.success mustBe true
    probe1.cancel()
  }

  it should "Fail to Delete policy tag in Database" in new KafkaSettingsQuery {

    Await.result(database.TagTable.store(tag), remainingOrDefault)

    val policytagIdForFail = UUID.randomUUID().toString
    val appRequest = generateAppRequestWithHeadersForDeleteTagFailure(responseTopic, deleteParkingTag, policytagIdForFail, tagFromUser)

    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.error.eq("Tag not found with id : " + policytagIdForFail)
    actualOutput.response.status mustBe 404
    probe.cancel()
  }

  it should "Fail to Delete policy tag in Database with wrong siteid" in new KafkaSettingsQuery {

    Await.result(database.TagTable.store(tag), remainingOrDefault)

    val appRequest = generateAppRequestWithHeadersGetTagFailureWrongOrgIdSiteid(responseTopic, deleteParkingTag, tag.uid, orgId, "wrongsiteid")

    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.error.eq("Tag not found with id : " + tag.uid)
    actualOutput.response.status mustBe 404
    probe.cancel()
  }


  /* API: updateParkingTagProcess */
  it should "Update policy tag in Database" in new KafkaSettingsQuery {
    Await.result(database.TagTable.store(tag), remainingOrDefault)
    val appRequest = generateAppRequestWithHeaders(responseTopic, updateParkingTag, tagId, updatedTagFromUser)
    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe1 = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[TagResponse]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.description mustBe updatedTag.description
    probe1.cancel()
  }

  it should "Fail to Update what if tag in Database" in new KafkaSettingsQuery {

    val policytagIdForFail = UUID.randomUUID().toString
    val appRequest = generateAppRequestWithHeadersForDeleteTagFailure(responseTopic, updateParkingTag, policytagIdForFail, tagFromUser)

    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.error.eq("Tag not found with id : " + policytagIdForFail)
    actualOutput.response.status mustBe 404
    probe.cancel()
  }

  it should "Fail to Update policy tag in Database with wrong orgid" in new KafkaSettingsQuery {
    Await.result(database.TagTable.store(tag), remainingOrDefault)
    val appRequest = generateAppRequestWithHeadersGetTagFailureWrongOrgIdSiteid(responseTopic, updateParkingTag, tag.uid, "wrongorgid", siteId)

    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.error.eq("Tag not found with id : " + tag.uid)
    actualOutput.response.status mustBe 404
    probe.cancel()
  }


  it should "Throw error for unknown casel request type" in new KafkaSettingsQuery {
    val appRequest = generateAppRequestWithHeaders(responseTopic, "unknown type", tagId, updatedTagFromUser)
    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe1 = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    probe1.cancel()
  }

  it should "Throw error for unknown policy tag id" in new KafkaSettingsQuery {
    val appRequest =
      generateAppRequestWithHeaders(responseTopic, getParkingTag, "randomPolicyCatId", updatedTagFromUser)
    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe1 = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    probe1.cancel()
  }

  it should "Get What if tag from Database by id" in new KafkaSettingsQuery {
    Await.result(database.WhatIfParkingTagTable.store(tag), remainingOrDefault)
    val appRequest = generateAppRequestWithHeaders(responseTopic, getWhatIfTag, tagId, tagFromUser)
    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe1 = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[TagResponse]](consumedElement._2)
    actualOutput.response.result mustBe tagResponse
    actualOutput.response.success mustBe true
    probe1.cancel()
  }

  it should "Fail to Get What if tag from Database by id" in new KafkaSettingsQuery {

    Await.result(database.WhatIfParkingTagTable.store(tag), remainingOrDefault)

    val appRequest = generateAppRequestWithHeadersGetTagFailure(responseTopic, getWhatIfTag, tagFromUser)

    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.error.eq("Required tagid missing in request")
    actualOutput.response.status mustBe 404
    probe.cancel()
  }


  it should "Fail to Get What if tag from Database by id because of wrong orgid" in new KafkaSettingsQuery {

    Await.result(database.WhatIfParkingTagTable.store(tag), remainingOrDefault)

    val appRequest = generateAppRequestWithHeadersGetTagFailureWrongOrgIdSiteid(responseTopic, getWhatIfTag, tag.uid, "orgid", siteId)

    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.status mustBe 404
    probe.cancel()
  }


  /* API: postParkingTagProcess */

  it should "Create What if tag in Database" in new KafkaSettingsQuery {
    val appRequest = generateAppRequestWithHeaders(responseTopic, createWhatIfTag, tagId, tagFromUser)
    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe1 = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[Tag]](consumedElement._2)
    actualOutput.response.success mustBe true
    probe1.cancel()
  }

  it should "Fail to Create What if tag in Database" in new KafkaSettingsQuery {

    val appRequest = generateAppRequestWithHeadersGetTagFailure(responseTopic, createWhatIfTag, tagFromUser)

    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.status mustBe 404
    probe.cancel()
  }


  /* API: getAllParkingTagProcess */

  it should "Get all What if tag from Database" in new KafkaSettingsQuery {
    DBServiceTag.cleanup()
    Await.result(database.WhatIfParkingTagTable.store(tag), remainingOrDefault)
    Await.result(database.WhatIfParkingTagTable.store(tagTwo), remainingOrDefault)
    val appRequest = generateAppRequestWithHeaders(responseTopic, getAllWhatIfTags, tagId, tagFromUser)
    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe1 = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[List[Tag]]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.size mustBe 2
    probe1.cancel()
  }

  it should "Return Empty What if Tag List for :- Get all What if tag from Database" in new KafkaSettingsQuery {

    DBServiceTag.cleanup()

    val appRequest = generateAppRequestWithHeadersGetTagFailure(responseTopic, getAllWhatIfTags, tagFromUser)

    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[List[Tag]]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.size mustBe 0
    probe.cancel()
  }


  /* API: deleteParkingTagProcess */
  it should "Delete What if tag in Database" in new KafkaSettingsQuery {
    Await.result(database.WhatIfParkingTagTable.store(tag), remainingOrDefault)
    val appRequest = generateAppRequestWithHeaders(responseTopic, deleteWhatIfTag, tagId, tagFromUser)
    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe1 = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[SuccessMessage]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.success mustBe true
    probe1.cancel()
  }

  it should "Fail to Delete what if tag in Database" in new KafkaSettingsQuery {

    Await.result(database.WhatIfParkingTagTable.store(tag), remainingOrDefault)

    val policytagIdForFail = UUID.randomUUID().toString
    val appRequest = generateAppRequestWithHeadersForDeleteTagFailure(responseTopic, deleteWhatIfTag, policytagIdForFail, tagFromUser)

    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.status mustBe 404
    probe.cancel()
  }

  it should "Fail to Delete what if tag in Database with wrong siteid" in new KafkaSettingsQuery {

    Await.result(database.WhatIfParkingTagTable.store(tag), remainingOrDefault)

    val appRequest = generateAppRequestWithHeadersGetTagFailureWrongOrgIdSiteid(responseTopic, deleteWhatIfTag, tag.uid, orgId, "wrongsiteid")

    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.status mustBe 404
    actualOutput.response.error mustBe NotFound_TagId.value + tag.uid + Does_Not_Exist_OrgSite.value + orgId + Siteid.value + "wrongsiteid"
    probe.cancel()
  }


  /* API: updateParkingTagProcess */
  it should "Update what if tag in Database" in new KafkaSettingsQuery {
    Await.result(database.WhatIfParkingTagTable.store(tag), remainingOrDefault)
    val appRequest = generateAppRequestWithHeaders(responseTopic, updateWhatIfTag, tagId, updatedTagFromUser)
    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe1 = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[TagResponse]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.description mustBe updatedTag.description
    probe1.cancel()
  }

  it should "Fail to Update policy tag in Database" in new KafkaSettingsQuery {

    val policytagIdForFail = UUID.randomUUID().toString
    val appRequest = generateAppRequestWithHeadersForDeleteTagFailure(responseTopic, updateWhatIfTag, policytagIdForFail, tagFromUser)

    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.status mustBe 404
    actualOutput.response.error mustBe NotFound_TagId.value + policytagIdForFail + Does_Not_Exist_OrgSite.value + orgId + Siteid.value + siteId

    probe.cancel()
  }

  it should "Fail to Update what if tag in Database with wrong orgid" in new KafkaSettingsQuery {
    Await.result(database.WhatIfParkingTagTable.store(tag), remainingOrDefault)
    val appRequest = generateAppRequestWithHeadersGetTagFailureWrongOrgIdSiteid(responseTopic, updateWhatIfTag, tag.uid, "wrongorgid", siteId)

    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.status mustBe 404
    actualOutput.response.error mustBe NotFound_TagId.value + tag.uid + Does_Not_Exist_OrgSite.value + "wrongorgid" + Siteid.value + siteId
    probe.cancel()
  }

  it should "Throw error for unknown what if tag id" in new KafkaSettingsQuery {
    val appRequest =
      generateAppRequestWithHeaders(responseTopic, getWhatIfTag, "randomPolicyCatId", updatedTagFromUser)
    val flowUnderTest = tagService.tagQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe1 = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.error mustBe NotFound_TagId.value + "randomPolicyCatId" + Does_Not_Exist_OrgSite.value + orgId + Siteid.value + siteId
    probe1.cancel()
  }

  ignore should "Consume app request from kafka and can produce the app response to kafka" in new KafkaSettingsQuery {

    Await.result(database.TagTable.store(tag), remainingOrDefault)
    val testEvent = generateAppRequestWithHeaders(responseTopic, getParkingTag, tagId, tagFromUser)
    Await.result(produceEvent(testTopic, testEvent), remainingOrDefault)
    val consumerUnderTest =
      AkkaStreamUtil.configKafkaSource(createEventConsumerSettingsString(group1), Set(testTopic))

    val msgUnmarshallFlowUnderTest = tagService.msgUnmarshallingFlow

    val policytagFlowUnderTest = tagService.tagQueryFlow

    val kafkaSinkUnderTest = tagService.responseKafkaSink

    val (killSwitch, done) = consumerUnderTest
      .via(msgUnmarshallFlowUnderTest)
      .via(policytagFlowUnderTest)
      .toMat(kafkaSinkUnderTest)(Keep.both)
      .run()

    val kafkaConsumerUnderTest =
      AkkaStreamUtil.configKafkaSource(createEventConsumerSettingsString(group1), Set(responseTopic))
    val probe = kafkaConsumerUnderTest.toMat(TestSink.probe)(Keep.right).run()
    probe.request(1)
    val consumedMsg = probe.requestNext(30.seconds)
    val appresponse = ObjectMapperUtil.fromJson[AppSuccessResponse[Tag]](consumedMsg.value())
    appresponse.response.result mustEqual tag
    probe.cancel()
    killSwitch.shutdown()

  }

}

