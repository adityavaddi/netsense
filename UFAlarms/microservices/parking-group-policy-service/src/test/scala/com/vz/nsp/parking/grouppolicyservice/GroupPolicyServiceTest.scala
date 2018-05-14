package com.vz.nsp.parking.grouppolicyservice

import java.util.UUID

import akka.actor.{ActorRef, ActorSystem}
import akka.kafka.ProducerMessage.Message
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.kafka.{ConsumerSettings, ProducerSettings, Subscriptions}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.stream.testkit.TestSubscriber
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestKit
import akka.{Done, NotUsed}
import com.verizon.netsense.connector.CassandraConnector
import com.vz.nsp.parking.config.{KafkaConnector, _}
import com.vz.nsp.parking.db.EmbeddedDatabase
import com.vz.nsp.parking.dblayer.DbLayer
import com.vz.nsp.parking.grouppolicyservice.constants.TestConstants.{updateParkingSpot, _}
import com.vz.nsp.parking.grouppolicyservice.constants.TestDataFeed._
import com.vz.nsp.parking.grouppolicyservice.db.parkingServicesDbTest.{ParkingSpaceTable, parkingGroupPolicyLinkTable, policyMappingTable}
import com.vz.nsp.parking.grouppolicyservice.helper.StreamHelper
import com.vz.nsp.parking.grouppolicyservice.service.{GroupPolicyService, GroupPolicyServiceAndSpaceService, ParkingSpaceService}
import com.vz.nsp.parking.model._
import com.vz.nsp.parking.util.ObjectMapperUtil
import net.manub.embeddedkafka.{EmbeddedKafka, EmbeddedKafkaConfig}
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer, StringDeserializer}
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FlatSpecLike, MustMatchers}

import scala.concurrent.duration.{FiniteDuration, _}
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * Created by dasarst on 8/16/17.
  */
class GroupPolicyServiceTest extends TestKit(ActorSystem("groupspot-System"))
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
  implicit val embeddedKafkaConfig = EmbeddedKafkaConfig(kafkaPort = 8767, zooKeeperPort = 8768)
  val bootstrapServers = s"localhost:${embeddedKafkaConfig.kafkaPort}"
  implicit val mat = ActorMaterializer()(system)
  implicit val stageStoppingTimeout = StageStoppingTimeout(20.seconds)

  val dbLayer = DbLayer()
  val groupPolicyHelper = GroupPolicyService(dbLayer)
  val parkingSpotHelper = ParkingSpaceService(dbLayer)
  val streamHelper = StreamHelper(groupPolicyHelper, parkingSpotHelper)
  val groupPolicyService = GroupPolicyServiceAndSpaceService(streamHelper)

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

  object DBServiceSpot {
    implicit val ec = scala.concurrent.ExecutionContext
    implicit val session = CassandraConnector.testConnector.session
    implicit val executor = system.dispatcher


    def initParkingSpot() = {
      Await.ready(ParkingSpaceTable.customTypesSpace, 5.seconds)
      Await.ready(ParkingSpaceTable.createSpaceTable(), 5.seconds)

    }

    def initPolicy() = {
      Await.ready(policyMappingTable.customTypesPolicy(), 5.seconds)
      Await.ready(policyMappingTable.createPolicyTable, 5.seconds)
    }

    def initGroupPolicy() = {
      Await.ready(parkingGroupPolicyLinkTable.createGroupTable(), 5.seconds)
    }

    def cleanup() = {
      Await.ready(ParkingSpaceTable.truncateSpaceTable, 5.seconds)
      Await.ready(policyMappingTable.truncatePolicy, 5.seconds)
      Await.ready(parkingGroupPolicyLinkTable.truncateTable(), 5.seconds)
    }
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    EmbeddedKafka.start()
    DBServiceSpot.initParkingSpot()
    DBServiceSpot.initPolicy()
    DBServiceSpot.initGroupPolicy()

  }

  override def afterAll(): Unit = {
    EmbeddedKafka.stop()
    DBServiceSpot.cleanup()
    system.terminate()
    super.afterAll()
  }

  trait KafkaConfiguration {
    val partition0 = 0
    val group1 = createGroup

    def createTopic: String = "topic-" + UUID.randomUUID().toString

    def createGroup: String = "group-" + UUID.randomUUID().toString

    def createEventConsumerSettingsString(group: String): ConsumerSettings[String, String] = {
      KafkaConnector.configKafkaConsumerSettings(bootstrapServers,
        group,
        new StringDeserializer,
        new StringDeserializer)
    }

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
      KafkaConnector.configKafkaProducerSettings(bootstrapServers,
        new ByteArraySerializer,
        new ByteArraySerializer)
  }

  trait KafkaSettingsQuery extends KafkaConfiguration {
    lazy val testTopic: String = createTopic
    lazy val testGroup: String = createGroup
    lazy val requestTopic: String = createTopic
    lazy val responseTopic: String = createTopic
    private implicit val setupKafkaConfig = KafkaConfig.setupEmbeddedKafkaConfig(
      bootstrapServers,
      testGroup, testTopic, requestTopic, responseTopic)
  }

  final case class StageStoppingTimeout(time: FiniteDuration)

  it should "start the embedded kafka & close the probe" in new KafkaSettingsQuery {
    val probe = createEventProbe(createEventConsumerSettings(testGroup), testTopic)
    probe.ensureSubscription()
    probe.cancel()

  }

  /* API: groupPolicyAssociationProcess */
  it should "associate parking group with policy" in new KafkaSettingsQuery {
    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)
    val appRequest = generateAppRequestWithHeaders(responseTopic, policyassociation, policyId, parkingGroupPolicyLinkRequest)
    val flowUnderTest = groupPolicyService.groupPolicyQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe1 = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[SuccessMessage]](consumedElement._2)
    actualOutput.response.success mustBe true
    probe1.cancel()
  }

  it should "Fail to associate parking group with policy" in new KafkaSettingsQuery {

    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)

    val appRequest = generateAppRequestWithHeaders(responseTopic, policyassociation, policyId, parkingGroupPolicyLinkRequest)

    val flowUnderTest = groupPolicyService.groupPolicyQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.error.eq("Specified parking group is already associated with other policy")
    actualOutput.response.status mustBe 404
    probe.cancel()
  }


  /* API: groupPolicyDisassociationProcess */
  it should "disassociate parking group with policy" in new KafkaSettingsQuery {

    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)
    database.parkingGroupPolicyLinkTable.associateGroupPolicy(parkingGroupPolicyLink, 10)
    val appRequest1 = generateAppRequestWithHeaders(responseTopic, policydisassociation, policyId, parkingGroupPolicyLinkRequest)
    val flowUnderTest = groupPolicyService.groupPolicyQueryFlow
    val sourceMock = Source.single(appRequest1).map(x => x)
    val probe1 = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[SuccessMessage]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result mustBe SuccessMessage(true)
    probe1.cancel()
  }

  it should "Fail to disassociate parking group with policy" in new KafkaSettingsQuery {

    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)

    database.parkingGroupPolicyLinkTable.associateGroupPolicy(parkingGroupPolicyLink, 10)

    val policyIdForFail = UUID.randomUUID().toString
    val appRequest = generateAppRequestWithHeaders(responseTopic, policydisassociation, policyIdForFail, parkingGroupPolicyLinkRequest)

    val flowUnderTest = groupPolicyService.groupPolicyQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.error.eq("Specified parking group is not associated with this policy id: " + policyIdForFail)
    actualOutput.response.status mustBe 404
    probe.cancel()
  }

  /* API: updateParkingSpotProcess */
  it should "Update a parking spot metadata by parkingspotids" in new KafkaSettingsQuery {
    Await.result(database.ParkingSpaceTable.storeParkingSpace(parkingspot,125671L), 5.second)
    val appRequest = generateAppRequestWithHeaders(responseTopic, updateParkingSpot, parkingSpotId, parkingSpotRequest, Set())
    val flowUnderTest = groupPolicyService.groupPolicyQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe1 = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[SuccessMessageWithIds]](consumedElement._2)
    actualOutput.response.success mustBe true
    // actualOutput.response.result.name mustBe updatedParkingSpot.name
    probe1.cancel()
  }

  /* API: getAllParkingSpotsProcess */
  it should "Get all parking spot's metadata by parkingspotids" in new KafkaSettingsQuery {
    DBServiceSpot.cleanup()
    Await.result(database.ParkingSpaceTable.storeParkingSpace(parkingspot,125671L), 5.second)
    Await.result(database.ParkingSpaceTable.storeParkingSpace(parkingSpotTwo,125671L), 5.second)
    val appRequest = generateAppRequestWithHeaders(responseTopic, getAllParkingSpots, parkingSpotId, parkingSpotRequest, Set(parkingSpotId, parkingSpotIdTwo))
    val flowUnderTest = groupPolicyService.groupPolicyQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe1 = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[List[ParkingSpaceResponse]]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.size mustBe 2
    probe1.cancel()
  }

  it should "Return Empty ParkingSpot List for - Get all parking spot's metadata in a site" in new KafkaSettingsQuery {

    DBServiceSpot.cleanup()
    Await.result(database.ParkingSpaceTable.storeParkingSpace(parkingspot,125671L), 5.second)
    Await.result(database.ParkingSpaceTable.storeParkingSpace(parkingSpotTwo,125671L), 5.second)

    val appRequest = generateAppRequestWithHeadersForGetAllParkingSpotFailure(responseTopic, getAllParkingSpots, parkingSpotId, Set("randomNumber"))

    val flowUnderTest = groupPolicyService.groupPolicyQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[List[ParkingSpaceResponse]]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.size mustBe 0 //Empty List
    probe.cancel()
  }


  /* API: deleteParkingSpotProcess */
  it should "Delete a parking spot metadata by parkingspotids" in new KafkaSettingsQuery {
    Await.result(database.ParkingSpaceTable.storeParkingSpace(parkingspot,125671L), 5.second)
    val appRequest = generateAppRequestWithHeaders(responseTopic, deleteParkingSpot, parkingSpotId, parkingSpotRequest, Set(parkingSpotId, parkingSpotIdTwo))
    val flowUnderTest = groupPolicyService.groupPolicyQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe1 = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[SuccessMessage]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result mustBe SuccessMessage(true)
    probe1.cancel()
  }

  it should "Fail to Delete a parking spot metadata by parkingspotids" in new KafkaSettingsQuery {

    Await.result(database.ParkingSpaceTable.storeParkingSpace(parkingspot,125671L), 5.second)

    val parkingSpotIdForFail = UUID.randomUUID().toString
    val appRequest = generateAppRequestWithHeaders(responseTopic, deleteParkingSpot, parkingSpotIdForFail, parkingSpotRequest, Set("random11", "random22"))

    val flowUnderTest = groupPolicyService.groupPolicyQueryFlow
    val sourceMock = Source.single(appRequest).map(x => x)
    val probe = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.error.eq("Spot attribues not found for these parkingspotids :" + Set("random11", "random22") mkString ", ")
    actualOutput.response.status mustBe 404
    probe.cancel()
  }


}
