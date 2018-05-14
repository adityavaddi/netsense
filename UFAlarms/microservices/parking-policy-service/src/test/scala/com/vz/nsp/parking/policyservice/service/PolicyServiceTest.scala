package com.vz.nsp.parking.policyservice.service

import java.time.Instant
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
import com.outworkers.phantom.dsl.ResultSet
import com.verizon.netsense.connector.CassandraConnector
import com.vz.nsp.parking.config.{KafkaConfig, KafkaConnector}
import com.vz.nsp.parking.db.{EmbeddedDatabase, PhantomService, ProductionDatabase}
import com.vz.nsp.parking.dblayer.DbLayer
import com.vz.nsp.parking.model.ResponseMessage.{Does_Not_Exist_OrgSite, NotFound_PolicyId_Table, Siteid}
import com.vz.nsp.parking.model._
import com.vz.nsp.parking.policyservice.config.TestSuiteConfig
import com.vz.nsp.parking.policyservice.constants.TestConstants._
import com.vz.nsp.parking.policyservice.constants.TestDataFeed.{policyId, _}
import com.vz.nsp.parking.policyservice.db.PolicyConnector
import com.vz.nsp.parking.policyservice.db.PolicyConnector._
import com.vz.nsp.parking.policyservice.helper.{PolicyHelper, StreamHelper}
import com.vz.nsp.parking.util.{AkkaStreamUtil, ObjectMapperUtil}
import net.manub.embeddedkafka.{EmbeddedKafka, EmbeddedKafkaConfig}
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer, StringDeserializer}
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FlatSpecLike, MustMatchers}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

class PolicyServiceTest
    extends TestKit(ActorSystem("PolicyService-System"))
    with FlatSpecLike
    with MustMatchers
    with BeforeAndAfterAll
    with BeforeAndAfterEach
    with TestSuiteConfig
    with EmbeddedDatabase {
  implicit val ec = ExecutionContext.Implicits.global

  type ActorMaterializerImpl = {
    def system: ActorSystem
    def supervisor: ActorRef
  }
  val dbLayer      = new DbLayer(new PhantomService with ProductionDatabase {})
  val policyHelper = new PolicyHelper(dbLayer)
  val streamHelper = new StreamHelper(policyHelper)

  val policyService                 = new PolicyService(streamHelper)
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
    KafkaConnector.configKafkaConsumerSettings(bootstrapServers,
                                               group,
                                               new ByteArrayDeserializer,
                                               new ByteArrayDeserializer)

  object DBServicePolicy {
    implicit val ec       = scala.concurrent.ExecutionContext
    implicit val session  = CassandraConnector.testConnector.session
    implicit val executor = system.dispatcher

    def initGroup() =
      Await.ready(parkingGroupPolicyLinkTable.createTable(), 5.seconds)

    def truncategroup(): Future[ResultSet] =
      Await.ready(parkingGroupPolicyLinkTable.truncateTable(), 5.seconds)

    def initTag() =
      Await.ready(tagTable.createTable(), 5.seconds)

    def truncateTag(): Future[ResultSet] =
      Await.ready(tagTable.truncateTable(), 5.seconds)

    def init(): Future[Seq[ResultSet]] = {
      policyMappingTable.policyCustomTypes()
      policyMappingTable.createPolicyTable()
      whatIfPolicyTable.createWhatIfPolicyTable()
      initGroup()
      initTag()
    }

    def cleanup() = {
      Await.ready(policyMappingTable.truncatePolicy, 5.seconds)
      Await.ready(whatIfPolicyTable.truncateWhatIfPolicy, 5.seconds)
      truncateTag()
      truncategroup()

    }
  }

  override def beforeAll(): Unit = {
    EmbeddedKafka.start()
    session.init()
    DBServicePolicy.init()
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    EmbeddedKafka.stop()
    DBServicePolicy.cleanup()
    super.afterAll()
    system.terminate()
  }

  trait KafkaConfiguration {
    val partition0 = 0
    val group1     = createGroup

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
    lazy val testTopic: String     = createTopic
    lazy val testGroup: String     = createGroup
    lazy val requestTopic: String  = createTopic
    lazy val responseTopic: String = createTopic
    private implicit val setupKafkaConfig =
      KafkaConfig.setupEmbeddedKafkaConfig(bootstrapServers, testGroup, testTopic, requestTopic, responseTopic)
  }

  final case class StageStoppingTimeout(time: FiniteDuration)

  it should "start the embedded kafka & close the probe" in new KafkaSettingsQuery {

    val probe = createEventProbe(createEventConsumerSettings(testGroup), testTopic)
    probe.ensureSubscription()
    probe.cancel()

  }

  /* method: postParkingPolicyProcess */
  it should "Create a parking policy in a site" in new KafkaSettingsQuery {

    val appRequest      = generateAppRequestWithHeaders(responseTopic, postParkingPolicy, policyId, policyFromUser)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[PolicyResponse]](consumedElement._2)
    actualOutput.response.success mustBe true
    probe1.cancel()

  }

  it should "Fail to Create a parking policy in a site" in new KafkaSettingsQuery {

    val appRequest      = generateAppRequestWithHeadersForCreatePolicyFailure(responseTopic, postParkingPolicy, policyId)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.error.eq("Policy not found in the request")
    actualOutput.response.status mustBe 404
    actualOutput.response.success mustBe false
    probe.cancel()

  }

  /* API: getParkingProcess */
  it should "Get a specific parking policy in a site" in new KafkaSettingsQuery {

    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)
    val appRequest      = generateAppRequestWithHeaders(responseTopic, getParkingPolicy, policyId, policyFromUser)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[PolicyResponse]](consumedElement._2)
    actualOutput.response.result.policyId mustBe policy.uid
    actualOutput.response.success mustBe true
    probe1.cancel()

  }

  it should "Fail to Get a specific parking policy in a site" in new KafkaSettingsQuery {

    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)

    val policyIdForFail = UUID.randomUUID().toString
    val appRequest      = generateAppRequestWithHeaders(responseTopic, getParkingPolicy, policyIdForFail, policyFromUser)

    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.error.eq("Policy not found with id : " + policyIdForFail)
    actualOutput.response.status mustBe 404
    actualOutput.response.success mustBe false
    probe1.cancel()

  }

  it should "Fail to Get a specific parking policy in a site because of wrong orgid" in new KafkaSettingsQuery {

    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)

    val appRequest = generateAppRequestWithHeadersWrongOrgidOrSiteid(responseTopic,
                                                                     getParkingPolicy,
                                                                     policyId,
                                                                     policyFromUser,
                                                                     "wrongOrgId",
                                                                     siteId)

    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.error.eq("Policy not found with id : " + policyId)
    actualOutput.response.status mustBe 404
    actualOutput.response.success mustBe false
    probe1.cancel()

  }

  it should "Fail to Get a specific parking policy in a site because of wrong siteid" in new KafkaSettingsQuery {

    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)

    val appRequest = generateAppRequestWithHeadersWrongOrgidOrSiteid(responseTopic,
                                                                     getParkingPolicy,
                                                                     policyId,
                                                                     policyFromUser,
                                                                     orgId,
                                                                     "wrongSiteid")

    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.error.eq("Policy not found with id : " + policyId)
    actualOutput.response.status mustBe 404
    actualOutput.response.success mustBe false
    probe1.cancel()

  }

  /**
   * Test cases for GET parking policy with associated parkinggroups
   */
  it should "Get a specific parking policy with associated parkinggroups in a site - Positive" in new KafkaSettingsQuery {
    Await.ready(policyMappingTable.truncatePolicy, remainingOrDefault)
    Await.ready(parkingGroupPolicyLinkTable.truncateTable(), remainingOrDefault)
    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)
    Await.result(database.parkingGroupPolicyLinkTable.associateGroupPolicy(parkingGroupPolicyLink, 10),
                 remainingOrDefault)
    val appRequest      = generateAppRequestWithHeaders(responseTopic, getParkingPolicy, policyId, policyFromUser)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[PolicyResponse]](consumedElement._2)
    actualOutput.response.result.policyId mustBe policy.uid
    actualOutput.response.result.associatedParkingGroups mustBe Some(List(parkingGroupPolicyLink.parkinggroupid))
    actualOutput.response.success mustBe true
    probe1.cancel()
  }

  it should "Get a specific parking policy with associated parkinggroups(multiple) in a site - Positive" in new KafkaSettingsQuery {
    Await.ready(policyMappingTable.truncatePolicy, remainingOrDefault)
    Await.ready(parkingGroupPolicyLinkTable.truncateTable(), remainingOrDefault)
    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)
    Await.result(database.parkingGroupPolicyLinkTable
                   .associateGroupPolicy(parkingGroupPolicyLink.copy(parkinggroupid = "pg1", uid = "1"), 10),
                 remainingOrDefault)
    Await.result(database.parkingGroupPolicyLinkTable
                   .associateGroupPolicy(parkingGroupPolicyLink.copy(parkinggroupid = "pg2", uid = "2"), 10),
                 remainingOrDefault)
    Await.result(
      database.parkingGroupPolicyLinkTable
        .associateGroupPolicy(parkingGroupPolicyLink.copy(parkinggroupid = "pg3", endTime = 123l, uid = "3"), 10),
      remainingOrDefault
    )
    val appRequest      = generateAppRequestWithHeaders(responseTopic, getParkingPolicy, policyId, policyFromUser)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[PolicyResponse]](consumedElement._2)
    actualOutput.response.result.policyId mustBe policy.uid
    assert(actualOutput.response.result.associatedParkingGroups.get.contains("pg1"))
    assert(actualOutput.response.result.associatedParkingGroups.get.contains("pg2"))
    actualOutput.response.success mustBe true
    probe1.cancel()
  }

  it should "Get a specific parking policy with associated parkinggroups in a site - Negative 1" in new KafkaSettingsQuery {
    Await.ready(policyMappingTable.truncatePolicy, remainingOrDefault)
    Await.ready(parkingGroupPolicyLinkTable.truncateTable(), remainingOrDefault)
    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)
    Await.result(
      database.parkingGroupPolicyLinkTable.associateGroupPolicy(parkingGroupPolicyLink.copy(endTime = 123), 10),
      remainingOrDefault
    )
    val appRequest      = generateAppRequestWithHeaders(responseTopic, getParkingPolicy, policyId, policyFromUser)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[PolicyResponse]](consumedElement._2)
    actualOutput.response.result.policyId mustBe policy.uid
    actualOutput.response.result.associatedParkingGroups mustBe None
    actualOutput.response.success mustBe true
    probe1.cancel()
  }

  it should "Get a specific parking policy with associated parkinggroups in a site - Negative 2" in new KafkaSettingsQuery {
    Await.ready(policyMappingTable.truncatePolicy, remainingOrDefault)
    Await.ready(parkingGroupPolicyLinkTable.truncateTable(), remainingOrDefault)
    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)
    val appRequest      = generateAppRequestWithHeaders(responseTopic, getParkingPolicy, policyId, policyFromUser)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[PolicyResponse]](consumedElement._2)
    actualOutput.response.result.policyId mustBe policy.uid
    actualOutput.response.result.associatedParkingGroups mustBe None
    actualOutput.response.success mustBe true
    probe1.cancel()
  }

  /* API: getAllParkingProcess */
  it should "Get all parking policies in a site" in new KafkaSettingsQuery {

    DBServicePolicy.cleanup()
    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)
    Await.result(database.policyMappingTable.storePolicy(policyOne), remainingOrDefault)
    val appRequest      = generateAppRequestWithHeaders(responseTopic, getAllParkingPolicy, policyId, policyFromUser)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[List[PolicyResponse]]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.size mustBe 2
    probe1.cancel()

  }

  it should "Return Empty Policy List: Get all parking policies in a site" in new KafkaSettingsQuery {

    DBServicePolicy.cleanup()
    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)
    Await.result(database.policyMappingTable.storePolicy(policyOne), remainingOrDefault)

    val orgId  = UUID.randomUUID().toString
    val siteId = UUID.randomUUID().toString
    val appRequest = generateAppRequestWithHeadersForGetAllParkingProcessFailure(responseTopic,
                                                                                 getAllParkingPolicy,
                                                                                 policyId,
                                                                                 orgId,
                                                                                 siteId)

    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[List[Policy]]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.size mustBe 0
    probe.cancel()

  }

  /**
   * Test cases for GET ALL parking policy with associated parkinggroups
   */
  it should "Get all parking policies in a site with associated parkinggroups - Positive and Negative" in new KafkaSettingsQuery {
    DBServicePolicy.cleanup()
    Await.result(database.policyMappingTable.storePolicy(policy.copy(uid = "policy1")), remainingOrDefault)
    Await.result(database.policyMappingTable.storePolicy(policy.copy(uid = "policy2")), remainingOrDefault)
    Await.result(database.policyMappingTable.storePolicy(policy.copy(uid = "policy3")), remainingOrDefault)
    Await.result(
      database.parkingGroupPolicyLinkTable
        .associateGroupPolicy(parkingGroupPolicyLink.copy(uid = "1", policyid = "policy1", parkinggroupid = "pg1"), 1),
      remainingOrDefault
    )
    Await.result(
      database.parkingGroupPolicyLinkTable
        .associateGroupPolicy(parkingGroupPolicyLink.copy(uid = "2", policyid = "policy1", parkinggroupid = "pg2"), 1),
      remainingOrDefault
    )
    Await.result(
      database.parkingGroupPolicyLinkTable
        .associateGroupPolicy(parkingGroupPolicyLink.copy(uid = "3", policyid = "policy2", parkinggroupid = "pg3"), 1),
      remainingOrDefault
    )
    Await.result(
      database.parkingGroupPolicyLinkTable
        .associateGroupPolicy(
          parkingGroupPolicyLink.copy(uid = "4", policyid = "policy2", parkinggroupid = "pg4", endTime = 123l),
          1
        ),
      remainingOrDefault
    )
    val appRequest      = generateAppRequestWithHeaders(responseTopic, getAllParkingPolicy, policyId, policyFromUser)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[List[PolicyResponse]]](consumedElement._2)
    assert(actualOutput.response.success)
    assert(actualOutput.response.result.size == 3)
    assert(
      actualOutput.response.result.filter(p => p.policyId == "policy1").head.associatedParkingGroups.get.contains("pg1")
    )
    assert(
      actualOutput.response.result.filter(p => p.policyId == "policy1").head.associatedParkingGroups.get.contains("pg2")
    )
    assert(actualOutput.response.result.filter(p => p.policyId == "policy2").head.associatedParkingGroups.get.size == 1)
    assert(
      actualOutput.response.result.filter(p => p.policyId == "policy2").head.associatedParkingGroups.get.contains("pg3")
    )
    assert(
      actualOutput.response.result.filter(p => p.policyId == "policy3").head.associatedParkingGroups.isEmpty
    )
    probe1.cancel()

  }

  /* API: deletePolicyProcess */
  it should "Delete a parking policy in a site" in new KafkaSettingsQuery {

    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)
    val appRequest      = generateAppRequestWithHeaders(responseTopic, deleteParkingPolicy, policyId, policyFromUser)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[SuccessMessage]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result mustBe SuccessMessage(true)
    probe1.cancel()

  }

  it should "Fail to Delete a parking policy in a site" in new KafkaSettingsQuery {

    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)

    val policyIdForFail = UUID.randomUUID().toString
    val appRequest      = generateAppRequestWithHeaders(responseTopic, deleteParkingPolicy, policyIdForFail, policyFromUser)

    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.error.eq("Policy not found with id : " + policyIdForFail)
    actualOutput.response.status mustBe 404
    probe.cancel()

  }

  /* API: updateParkingProcess */
  it should "Update a parking policy in a site" in new KafkaSettingsQuery {

    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)
    val appRequest      = generateAppRequestWithHeaders(responseTopic, updateParkingPolicy, policyId, policyFromUser)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[PolicyResponse]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.description mustBe updatedPolicy.description
    probe1.cancel()

  }

  it should "Fail to Update a parking policy in a site" in new KafkaSettingsQuery {

    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)

    val policyIdForFail = UUID.randomUUID().toString
    val appRequest      = generateAppRequestWithHeaders(responseTopic, updateParkingPolicy, policyIdForFail, policyFromUser)

    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.error.eq("Policy not found with id : " + policyIdForFail)
    actualOutput.response.status mustBe 404
    probe1.cancel()

  }

  /* API: getPolicyByVersionNumberAndId */
  it should "Get a specific parking policy in a site by version" in new KafkaSettingsQuery {

    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)
    val appRequest      = generateAppRequestWithHeaders(responseTopic, getPolicyByVersionNumber, policyId, policyFromUser)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[PolicyResponse]](consumedElement._2)
    actualOutput.response.result mustBe policyResponse(policy)
    actualOutput.response.success mustBe true
    probe1.cancel()

  }

  it should "Fail to Get a specific parking policy in a site by version" in new KafkaSettingsQuery {

    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)

    val appRequest = generateAppRequestWithHeadersForPolicyVersionFailure(responseTopic,
                                                                          getPolicyByVersionNumber,
                                                                          policyId,
                                                                          policyFromUser)

    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.error.eq("Required version number missing in request")
    actualOutput.response.status mustBe 404
    probe1.cancel()

  }

  /* API: getVersionHistoryProcess */
  it should "Get policy version history by id" in new KafkaSettingsQuery {

    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)
    Await.result(database.policyMappingTable.storePolicy(policyOne), remainingOrDefault)
    val appRequest      = generateAppRequestWithHeaders(responseTopic, getPolicyVersionHistory, policyId, policyFromUser)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[List[PolicyResponse]]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.size mustBe 2
    probe1.cancel()

  }

  it should "Fail to Get policy version history by id" in new KafkaSettingsQuery {

    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)
    Await.result(database.policyMappingTable.storePolicy(policyOne), remainingOrDefault)

    val policyIdForFail = UUID.randomUUID().toString
    val appRequest =
      generateAppRequestWithHeaders(responseTopic, getPolicyVersionHistory, policyIdForFail, policyFromUser)

    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.error.eq("Policy not found with id : " + policyIdForFail)
    actualOutput.response.status mustBe 404
    probe.cancel()

  }

  /* API: getActivePolicyProcess */
  it should "Get a specific active parking policy" in new KafkaSettingsQuery {

    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)
    Await.result(database.parkingGroupPolicyLinkTable.associateGroupPolicy(parkingGroupPolicyLink, 10),
                 remainingOrDefault)

    val appRequest      = generateAppRequestWithHeader(responseTopic, getActivePolicy, parkingGroupId, policyFromUser)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[PolicyResponse]](consumedElement._2)
    actualOutput.response.result mustBe policyResponse(policy)
    actualOutput.response.success mustBe true
    probe1.cancel()

  }

  def convertEpochToDate(epoch: Long): String =
    if (epoch == 0) "" else Instant.ofEpochMilli(epoch).toString

  it should "Fail to Get a specific active parking policy" in new KafkaSettingsQuery {

    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)
    Await.result(database.parkingGroupPolicyLinkTable.associateGroupPolicy(parkingGroupPolicyLink, 10),
                 remainingOrDefault)

    val parkingGroupId = UUID.randomUUID().toString
    val appRequest =
      generateAppRequestWithHeadersForActivePolicyFailure(responseTopic, getActivePolicy, policyId, policyFromUser)

    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.error.eq("Specified parking group is not associated with any policy")
    actualOutput.response.status mustBe 404
    probe.cancel()

  }

  /* API: searchPolicyByNameOrTagProcess */
  it should "Post To Search parking policy" in new KafkaSettingsQuery {

    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)
    val appRequest      = generateAppRequestWithHeadersObject(responseTopic, postToSearchPolicy, policyId, policyFromUser)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[List[PolicyResponse]]](consumedElement._2)
    actualOutput.response.success mustBe true
    probe1.cancel()

  }

  it should "Return Empty Policy List: Post To Search parking policy" in new KafkaSettingsQuery {

    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)

    val orgId  = UUID.randomUUID().toString
    val siteId = UUID.randomUUID().toString
    val appRequest = generateAppRequestWithHeadersForGetAllParkingProcessFailure(responseTopic,
                                                                                 getAllParkingPolicy,
                                                                                 policyId,
                                                                                 orgId,
                                                                                 siteId)

    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[List[Policy]]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.size mustBe 0
    probe.cancel()

  }

  /* API: getActivePoliciesInTimeline */
  it should "Get a specific parking policy of a site within timeline" in new KafkaSettingsQuery {

    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)
    Await.result(database.parkingGroupPolicyLinkTable.associateGroupPolicy(parkingGroupPolicyLink, 10),
                 remainingOrDefault)
    val appRequest =
      generateAppRequestWithHeaders(responseTopic, getActivePoliciesWithInTimeline, policyId, policyFromUser)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val expectedOutput =
      ActivePolicyResponse("3807c281-d938-4320-91b8-77e91a297bee", 10, "2017-10-27T20:24:03.820Z", "")
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[List[ActivePolicyResponse]]](consumedElement._2)
    actualOutput.response.result equals actualOutput
    actualOutput.response.success mustBe true
    probe1.cancel()
  }

  it should "Fail to Get a specific parking policy of a site within timeline" in new KafkaSettingsQuery {

    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)
    Await.result(database.parkingGroupPolicyLinkTable.associateGroupPolicy(parkingGroupPolicyLink, 10),
                 remainingOrDefault)

    val parkingGroupId = UUID.randomUUID().toString
    val appRequest =
      generateAppRequestWithHeadersForActivePolicyFailure(responseTopic, getActivePolicy, policyId, policyFromUser)

    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.error.eq("Specified parking group is not associated with any policy")
    actualOutput.response.status mustBe 404
    probe.cancel()
  }

  it should "Associate tag to policy in a site" in new KafkaSettingsQuery {

    Await.result(database.policyMappingTable.storePolicy(policy), remainingOrDefault)
    Await.result(database.TagTable.store(tag), remainingOrDefault)
    val appRequest      = generateAppRequestWithHeaders(responseTopic, "policyTagsAssociation", policyId, policyFromUser)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[SuccessMessageWithInvalidTags]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.notAssociatedTagIds.size mustBe 1
    probe1.cancel()

  }

  it should "Disassociate what-if tag to what-if policy in a site" in new KafkaSettingsQuery {

    Await.result(database.WhatIfPolicyTable.storeWhatIfPolicy(policy), remainingOrDefault)
    Await.result(database.WhatIfParkingTagTable.store(tag), remainingOrDefault)
    val appRequest      = generateAppRequestWithHeaders(responseTopic, "policyTagsDisassociation", policyId, policyFromUser)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[SuccessMessageWithInvalidTags]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.notAssociatedTagIds.size mustBe 1
    probe1.cancel()

  }

  it should "Create a What if parking policy in a site" in new KafkaSettingsQuery {

    val appRequest      = generateAppRequestWithHeaders(responseTopic, createWhatIfPolicy, policyId, policyFromUser)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[PolicyResponse]](consumedElement._2)
    actualOutput.response.success mustBe true
    probe1.cancel()

  }

  it should "Fail to Create a what if parking policy in a site" in new KafkaSettingsQuery {

    val appRequest      = generateAppRequestWithHeadersForCreatePolicyFailure(responseTopic, createWhatIfPolicy, policyId)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.status mustBe 404
    actualOutput.response.success mustBe false
    probe.cancel()

  }

  /* API: getParkingProcess */
  it should "Get a specific What if parking policy in a site" in new KafkaSettingsQuery {

    Await.result(database.WhatIfPolicyTable.storeWhatIfPolicy(policy), remainingOrDefault)
    val appRequest      = generateAppRequestWithHeaders(responseTopic, getWhatIfPolicy, policyId, policyFromUser)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[PolicyResponse]](consumedElement._2)
    actualOutput.response.result.policyId mustBe policy.uid
    actualOutput.response.success mustBe true
    probe1.cancel()

  }

  it should "Fail to Get what if a specific parking policy in a site" in new KafkaSettingsQuery {

    Await.result(database.WhatIfPolicyTable.storeWhatIfPolicy(policy), remainingOrDefault)

    val policyIdForFail = UUID.randomUUID().toString
    val appRequest      = generateAppRequestWithHeaders(responseTopic, getWhatIfPolicy, policyIdForFail, policyFromUser)

    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.error.eq("Policy not found with id : " + policyIdForFail)
    actualOutput.response.status mustBe 404
    actualOutput.response.success mustBe false
    actualOutput.response.error mustBe NotFound_PolicyId_Table.value + policyIdForFail + Does_Not_Exist_OrgSite.value + orgId + Siteid.value + siteId
    probe1.cancel()

  }

  it should "Fail to Get what if a specific parking policy in a site because of wrong orgid" in new KafkaSettingsQuery {

    Await.result(database.WhatIfPolicyTable.storeWhatIfPolicy(policy), remainingOrDefault)

    val appRequest = generateAppRequestWithHeadersWrongOrgidOrSiteid(responseTopic,
                                                                     getWhatIfPolicy,
                                                                     policyId,
                                                                     policyFromUser,
                                                                     "wrongOrgId",
                                                                     siteId)

    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.status mustBe 404
    actualOutput.response.success mustBe false
    actualOutput.response.error mustBe NotFound_PolicyId_Table.value + policyId + Does_Not_Exist_OrgSite.value + "wrongOrgId" + Siteid.value + siteId
    probe1.cancel()

  }

  it should "Fail to Get a what if  specific parking policy in a site because of wrong siteid" in new KafkaSettingsQuery {

    Await.result(database.WhatIfPolicyTable.storeWhatIfPolicy(policy), remainingOrDefault)

    val appRequest = generateAppRequestWithHeadersWrongOrgidOrSiteid(responseTopic,
                                                                     getWhatIfPolicy,
                                                                     policyId,
                                                                     policyFromUser,
                                                                     orgId,
                                                                     "wrongSiteid")

    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.error.eq("Policy not found with id : " + policyId)
    actualOutput.response.status mustBe 404
    actualOutput.response.success mustBe false
    actualOutput.response.error mustBe NotFound_PolicyId_Table.value + policyId + Does_Not_Exist_OrgSite.value + orgId + Siteid.value + "wrongSiteid"
    probe1.cancel()

  }

  /* API: getAllParkingProcess */
  it should "Get all what if parking policies in a site" in new KafkaSettingsQuery {

    DBServicePolicy.cleanup()
    Await.result(database.WhatIfPolicyTable.storeWhatIfPolicy(policy), remainingOrDefault)
    Await.result(database.WhatIfPolicyTable.storeWhatIfPolicy(policyOne), remainingOrDefault)
    val appRequest      = generateAppRequestWithHeaders(responseTopic, getAllWhatIfPolicies, policyId, policyFromUser)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[List[PolicyResponse]]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.size mustBe 2
    probe1.cancel()

  }

  it should "Return Empty Policy List: Get all what if parking policies in a site" in new KafkaSettingsQuery {

    DBServicePolicy.cleanup()
    Await.result(database.WhatIfPolicyTable.storeWhatIfPolicy(policy), remainingOrDefault)
    Await.result(database.WhatIfPolicyTable.storeWhatIfPolicy(policyOne), remainingOrDefault)

    val orgId  = UUID.randomUUID().toString
    val siteId = UUID.randomUUID().toString
    val appRequest = generateAppRequestWithHeadersForGetAllParkingProcessFailure(responseTopic,
                                                                                 getAllWhatIfPolicies,
                                                                                 policyId,
                                                                                 orgId,
                                                                                 siteId)

    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[List[Policy]]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.size mustBe 0
    probe.cancel()

  }

  /* API: deletePolicyProcess */
  it should "Delete a what if parking policy in a site" in new KafkaSettingsQuery {

    Await.result(database.WhatIfPolicyTable.storeWhatIfPolicy(policy), remainingOrDefault)
    val appRequest      = generateAppRequestWithHeaders(responseTopic, deleteWhatIfPolicy, policyId, policyFromUser)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[SuccessMessage]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result mustBe SuccessMessage(true)
    probe1.cancel()

  }

  it should "Fail to Delete a what if parking policy in a site" in new KafkaSettingsQuery {

    Await.result(database.WhatIfPolicyTable.storeWhatIfPolicy(policy), remainingOrDefault)

    val policyIdForFail = UUID.randomUUID().toString
    val appRequest      = generateAppRequestWithHeaders(responseTopic, deleteWhatIfPolicy, policyIdForFail, policyFromUser)

    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe           = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.status mustBe 404
    actualOutput.response.error mustBe NotFound_PolicyId_Table.value + policyIdForFail + Does_Not_Exist_OrgSite.value + orgId + Siteid.value + siteId

    probe.cancel()

  }

  /* API: updateParkingProcess */
  it should "Update a what if parking policy in a site" in new KafkaSettingsQuery {

    Await.result(database.WhatIfPolicyTable.storeWhatIfPolicy(policy), remainingOrDefault)
    val appRequest      = generateAppRequestWithHeaders(responseTopic, updateWhatIfPolicy, policyId, policyFromUser)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[PolicyResponse]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.description mustBe updatedPolicy.description
    probe1.cancel()

  }

  it should "Fail to Update a what if parking policy in a site" in new KafkaSettingsQuery {

    Await.result(database.WhatIfPolicyTable.storeWhatIfPolicy(policy), remainingOrDefault)

    val policyIdForFail = UUID.randomUUID().toString
    val appRequest      = generateAppRequestWithHeaders(responseTopic, updateWhatIfPolicy, policyIdForFail, policyFromUser)

    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)

    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    actualOutput.response.error mustBe NotFound_PolicyId_Table.value + policyIdForFail + Does_Not_Exist_OrgSite.value + orgId + Siteid.value + siteId
    actualOutput.response.status mustBe 404
    probe1.cancel()

  }

  it should "Associate what-if tag to what-if policy in a site" in new KafkaSettingsQuery {

    Await.result(database.WhatIfPolicyTable.storeWhatIfPolicy(policy), remainingOrDefault)
    Await.result(database.WhatIfParkingTagTable.store(tag), remainingOrDefault)
    val appRequest      = generateAppRequestWithHeaders(responseTopic, whatIfPolicyTagsAssociation, policyId, policyFromUser)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[SuccessMessageWithInvalidTags]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.notAssociatedTagIds.size mustBe 1
    probe1.cancel()

  }

  it should "Dis associate what-if tag to what-if policy in a site" in new KafkaSettingsQuery {

    Await.result(database.WhatIfPolicyTable.storeWhatIfPolicy(policy), remainingOrDefault)
    Await.result(database.WhatIfParkingTagTable.store(tag), remainingOrDefault)
    val appRequest =
      generateAppRequestWithHeaders(responseTopic, whatIfPolicyTagsDisassociation, policyId, policyFromUser)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppSuccessResponse[SuccessMessageWithInvalidTags]](consumedElement._2)
    actualOutput.response.success mustBe true
    actualOutput.response.result.notAssociatedTagIds.size mustBe 1
    probe1.cancel()

  }

  it should "Throw error for unknown casel request type" in new KafkaSettingsQuery {
    val appRequest      = generateAppRequestWithHeaders(responseTopic, "unknown type", policyId, policyFromUser)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    probe1.cancel()
  }

  it should "Throw error for unknown policy id" in new KafkaSettingsQuery {
    val appRequest =
      generateAppRequestWithHeaders(responseTopic, getParkingPolicy, "randomPolicyCatId", updatedTagFromUser)
    val flowUnderTest   = policyService.policyQueryFlow
    val sourceMock      = Source.single(appRequest).map(x => x)
    val probe1          = sourceMock.via(flowUnderTest).runWith(TestSink.probe)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    consumedElement mustNot be(null)
    val actualOutput = ObjectMapperUtil.fromJson[AppFailureResponse](consumedElement._2)
    actualOutput.response.success mustBe false
    probe1.cancel()
  }

  ignore should "Consume app request from kafka and can produce the app response to kafka" in new KafkaSettingsQuery {

    Await.result(database.TagTable.store(tag), remainingOrDefault)
    val testEvent = generateAppRequestWithHeaders(responseTopic, getParkingTag, policytagId, tagFromUser)
    Await.result(produceEvent(testTopic, testEvent), remainingOrDefault)
    val consumerUnderTest =
      AkkaStreamUtil.configKafkaSource(createEventConsumerSettingsString(group1), Set(testTopic))

    val msgUnmarshallFlowUnderTest = policyService.msgUnmarshallingFlow

    val policytagFlowUnderTest = policyService.policyQueryFlow

    val kafkaSinkUnderTest = policyService.responseKafkaSink

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
