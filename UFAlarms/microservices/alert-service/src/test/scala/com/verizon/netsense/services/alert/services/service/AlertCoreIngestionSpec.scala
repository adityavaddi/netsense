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
import com.verizon.netsense.services.alert.customexceptions._
import com.verizon.netsense.services.alert.database.CassandraDB
import com.verizon.netsense.services.alert.helper.KafkaConfig.kafkaConfig
import com.verizon.netsense.services.alert.helper.{AlarmValidator, KafkaConnector}
import com.verizon.netsense.services.alert.model._
import com.verizon.netsense.services.alert.services.AlertIngestionService
import com.verizon.netsense.services.alert.services.config.TestSuiteConfig
import com.verizon.netsense.services.alert.services.data.TestData
import com.verizon.netsense.services.alert.services.database.CassandraDBTest
import com.verizon.netsense.services.alert.util.ObjectMapperUtil._
import net.manub.embeddedkafka.{EmbeddedKafka, EmbeddedKafkaConfig}
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer}
import org.scalatest._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class AlertCoreIngestionSpec
    extends TestKit(ActorSystem("AlertCoreNodeIngestionSpec-System"))
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

    def produceCoreEvent(topic: String, coreAlarm: CoreAlarmEvent): Future[Done] = {
      val source = Source
        .single(coreAlarm)
        .map(e => toJsonBytes(e))
        .map(m => {
          Message(new ProducerRecord(topic, UUID.randomUUID.toString.getBytes, m), NotUsed)
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
    Await.ready(CassandraDBTest.DeviceAlarms.createTable(), 1.seconds)
    Await.ready(CassandraDBTest.AlertsByNode.createTable(), 1.seconds)
    Await.ready(CassandraDBTest.UFAlarmsById.createTable(), 1.seconds)
    Await.ready(CassandraDBTest.UFAlarms.createTable(), 1.seconds)
    loadOrgHierarchy()
  }

  override def afterAll(): Unit = {
    super.afterAll()
    EmbeddedKafka.stop()
    Await.ready(CassandraDBTest.DeviceAlarms.truncateTable, 1.seconds)
    Await.ready(CassandraDBTest.OrgHierarchyByNode.truncateTable, 1.seconds)
    Await.ready(CassandraDBTest.DeviceAlerts.truncateTable, 1.seconds)
    Await.ready(CassandraDBTest.AlertsByNode.truncateTable(), 1.seconds)
    Await.ready(CassandraDBTest.UFAlarmsById.truncateTable(), 1.seconds)
    Await.ready(CassandraDBTest.UFAlarms.truncateTable(), 1.seconds)
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
    val testGroup      = kafkaConfig.getString("alert-group-name") + UUID.randomUUID().toString
    val coreAlarmTopic = kafkaConfig.getString("core-alarm-request-topic")
    val alertTopic     = kafkaConfig.getString("alert-response-topic")
    val requestTopic   = kafkaConfig.getString("alarm-request-topic")
    val responseTopic  = kafkaConfig.getString("alert-response-topic")

    val consumerUnderTest = AlertIngestionService
      .configKafkaSource(createEventConsumerSettingsString(testGroup), Set(coreAlarmTopic))

    val msgPackToRestPackTest     = AlertIngestionService.messagePackTransformFlow
    val alarmValidationFlowTest   = AlertIngestionService.validateAlarmFlow
    val lookupExistingAlertTest   = AlertIngestionService.checkExistingAlertInDbFlow
    val lookupOrgForNewAlertTest  = AlertIngestionService.lookupOrgFlowForNewAlert
    val persistToDbTest           = AlertIngestionService.persistFlow
    val ufAlarmFlowTest           = AlertIngestionService.UfAlarmFlow
    val kafkaFlowTest             = AlertIngestionService.kafkaFlow
    val persistToAppendOnlyDbTest = AlertIngestionService.persistToAppendOnlyTableSink
    val publishToKafkaTest        = AlertIngestionService.kafkaSink

    val probe2 = consumerUnderTest
      .via(msgPackToRestPackTest)
      .via(alarmValidationFlowTest)
      .via(lookupOrgForNewAlertTest)
      .via(lookupExistingAlertTest)
      .via(ufAlarmFlowTest)
      .via(kafkaFlowTest)
      .runWith(TestSink.probe)
  }

  "Kafka Stream" should "start the embedded kafka & close the probe" in new KafkaSettings {

    val probe = createEventProbe(createEventConsumerSettingsString(testGroup), alertTopic)
    probe.ensureSubscription()
    probe.cancel()
  }

  it should "produce a sample core node alarm to embedded kafka and convert to Alert" in new KafkaSettings {

    Await.result(produceCoreEvent(coreAlarmTopic, coreAlarmPayload1), remainingOrDefault)
    val consumedElement = probe2.requestNext(remainingOrDefault)
    val unpackedMessage =
      Await.result(fromJsonAsync[AlertResponse](consumedElement.value()), remainingOrDefault)
    unpackedMessage.nodeId must equal(coreAlarmPayload1.nodeid)
    unpackedMessage.`type` must equal(coreAlarmPayload1.alarmType)
    unpackedMessage.severity.getOrElse("") must equal(coreAlarmPayload1.alarmSeverity)
    unpackedMessage.category.getOrElse("") must equal("Network")
    unpackedMessage.ufName must equal(coreAlarmPayload1.alarmType)
    unpackedMessage.description must equal(coreAlarmPayload1.msg)
    unpackedMessage.displayToPartner must equal(false)
    unpackedMessage.displayToCustomer must equal(false)
    probe2.cancel()
  }

  it should "produce a sample core node alarm of Clear type to embedded kafka and convert to Alert" in new KafkaSettings {

    Await.result(produceCoreEvent(coreAlarmTopic, coreAlarmPayload2), remainingOrDefault)
    val consumedElement = probe2.requestNext(remainingOrDefault)
    val unpackedMessage =
      Await.result(fromJsonAsync[AlertResponse](consumedElement.value()), remainingOrDefault)
    unpackedMessage.nodeId must equal(coreAlarmPayload2.nodeid)
    unpackedMessage.`type` must equal(coreAlarmPayload2.alarmType)
    unpackedMessage.severity.getOrElse("") must equal(coreAlarmPayload2.alarmSeverity)
    unpackedMessage.category.getOrElse("") must equal("Network")
    unpackedMessage.ufName must equal(coreAlarmPayload2.alarmType)
    unpackedMessage.description must equal(coreAlarmPayload2.msg)
    unpackedMessage.displayToPartner must equal(false)
    unpackedMessage.displayToCustomer must equal(false)
    probe2.cancel()
  }

  it should "produce a sample core node alarm with invalid node id and valid exception should be thrown" in new KafkaSettings {

    Await.result(produceCoreEvent(coreAlarmTopic, coreAlarmPayload1_invalidNode), remainingOrDefault)

    try {
      val consumedElement =
        probe2.onError(new Throwable("OrgDataNotFoundException Caught", throw new OrgDataNotFoundException))
    } catch {
      case ex: OrgDataNotFoundException => assert(true)
      case _: Throwable                 => assert(false)
    }

  }

  it should "produce a sample core node alarm with same alarm id and the existing alert should be updated" in new KafkaSettings {

    Await.result(produceCoreEvent(coreAlarmTopic, coreAlarmPayload_updated), remainingOrDefault)

    val consumedElement = probe2.requestNext(remainingOrDefault)
    val unpackedMessage =
      Await.result(fromJsonAsync[AlertResponse](consumedElement.value()), remainingOrDefault)
    unpackedMessage.nodeId must equal(coreAlarmPayload_updated.nodeid)
    unpackedMessage.`type` must equal(coreAlarmPayload_updated.alarmType)
    unpackedMessage.severity.getOrElse("") must equal(coreAlarmPayload_updated.alarmSeverity)
    unpackedMessage.category.getOrElse("") must equal("Network")
    unpackedMessage.ufName must equal(coreAlarmPayload_updated.alarmType)
    unpackedMessage.description must equal(coreAlarmPayload_updated.msg)
    unpackedMessage.displayToPartner must equal(false)
    unpackedMessage.displayToCustomer must equal(false)
    probe2.cancel()
  }

  it should "produce a sample core node alarm with invalid Alarm severity and the exception should be thrown" in new KafkaSettings {

    an[AlarmNotCreatedException] must be thrownBy
    AlarmValidator.validationPredicate(AlarmEvent(coreAlarmPayload1.copy(alarmSeverity = "Unknown")))
  }

  it should "insert a sample core node alarm in Alarms append only table" in new KafkaSettings {
    Await.ready(CassandraDBTest.DeviceAlarms.truncateTable, 1.seconds)
    Source(0 until 1)
      .map { x =>
        AlarmEvent(coreAlarmPayload_Append)
      }
      .via(alarmValidationFlowTest)
      .runWith(persistToAppendOnlyDbTest)

    CassandraDBTest.DeviceAlarms.getAlarm(coreAlarmPayload_Append.nodeid, coreAlarmPayload_Append.alarmType).map {
      case Some(alarm) =>
        assert(alarm.nodeId == coreAlarmPayload_Append.nodeid)
        assert(alarm.alarmType == coreAlarmPayload_Append.alarmType)
      case None => fail("Alarm not persisted")
    }
  }

  it should "insert a sample core node alert in alerts mapping table and alerts table" in new KafkaSettings {
    Await.ready(CassandraDBTest.DeviceAlerts.truncateTable, 1.seconds)
    Await.ready(CassandraDBTest.AlertsByNode.truncateTable(), 1.seconds)
    Source(0 until 1)
      .map { x =>
        AlarmEvent(coreAlarmPayload1)
      }
      .via(alarmValidationFlowTest)
      .via(lookupOrgForNewAlertTest)
      .via(lookupExistingAlertTest)
      .runWith(persistToDbTest)
    CassandraDB.AlertsByNode
      .getByNodeIdAndSiteId(coreAlarmPayload1.nodeid, coreAlarmPayload1.alarmType, orgId1, siteId1)
      .map {
        case Some(mapping) =>
          assert(mapping.nodeId.equals(coreAlarmPayload1.nodeid))
          assert(mapping.`type`.equals(coreAlarmPayload1.alarmType))
          assert(mapping.severity.getOrElse("").equals(coreAlarmPayload1.alarmSeverity))
          CassandraDB.DeviceAlerts.getAlertForAlertId(mapping.alertId, mapping.orgId, mapping.siteId).map {
            case Some(alert) =>
              assert(alert.severity.getOrElse("") == AlarmSeverity(coreAlarmPayload1.alarmSeverity).toString)
              assert(alert.`type` == coreAlarmPayload1.alarmType)
              assert(alert.msg.getOrElse("") == coreAlarmPayload1.msg)
              assert(alert.active)
            case None => fail("Alert not persisted")
          }
        case None => fail("Alert not persisted in Mappings table")
      }
  }

  it should "update a sample core node alert in alerts mapping table and alerts table" in new KafkaSettings {
    Source(0 until 1)
      .map { x =>
        AlarmEvent(coreAlarmPayload_updated)
      }
      .via(alarmValidationFlowTest)
      .via(lookupOrgForNewAlertTest)
      .via(lookupExistingAlertTest)
      .runWith(persistToDbTest)
    CassandraDB.AlertsByNode
      .getByNodeIdAndSiteId(coreAlarmPayload_updated.nodeid, coreAlarmPayload_updated.alarmType, orgId1, siteId1)
      .map {
        case Some(mapping) =>
          assert(mapping.nodeId.equals(coreAlarmPayload_updated.nodeid))
          assert(mapping.`type`.equals(coreAlarmPayload_updated.alarmType))
          assert(mapping.severity.getOrElse("").equals(coreAlarmPayload_updated.alarmSeverity))
          CassandraDB.DeviceAlerts.getAlertForAlertId(mapping.alertId, mapping.orgId, mapping.siteId).map {
            case Some(alert) =>
              assert(alert.severity.getOrElse("") == AlarmSeverity(coreAlarmPayload_updated.alarmSeverity).toString)
              assert(alert.`type` == coreAlarmPayload_updated.alarmType)
              assert(alert.msg.getOrElse("") == coreAlarmPayload_updated.msg)
              assert(alert.active)
            case None => fail("Alert not persisted")
          }
        case None => fail("Alert not persisted in Mappings table")
      }
  }

  it should "mark alert as inactive when received a clear core node alarm in alerts mappings table and alerts table" in new KafkaSettings {
    Source(0 until 1)
      .map { x =>
        AlarmEvent(coreAlarmPayload2)
      }
      .via(alarmValidationFlowTest)
      .via(lookupOrgForNewAlertTest)
      .via(lookupExistingAlertTest)
      .runWith(persistToDbTest)
    CassandraDB.AlertsByNode
      .getByNodeIdAndSiteId(coreAlarmPayload2.nodeid, coreAlarmPayload2.alarmType, orgId1, siteId1)
      .map {
        case Some(mapping) =>
          assert(mapping.nodeId.equals(coreAlarmPayload2.nodeid))
          assert(mapping.`type`.equals(coreAlarmPayload2.alarmType))
          assert(mapping.severity.getOrElse("").equals(coreAlarmPayload2.alarmSeverity))
          CassandraDB.DeviceAlerts.getAlertForAlertId(mapping.alertId, mapping.orgId, mapping.siteId).map {
            case Some(alert) =>
              assert(alert.severity.getOrElse("") == AlarmSeverity(coreAlarmPayload2.alarmSeverity).toString)
              assert(alert.`type` == coreAlarmPayload2.alarmType)
              assert(alert.msg.getOrElse("") == coreAlarmPayload2.msg)
              assert(!alert.active)
            case None => fail("Alert not persisted")
          }
        case None => fail("Alert not persisted in Mappings table")
      }
  }

  it should "not persist a clear core node alarm in alerts table" in new KafkaSettings {
    Await.ready(CassandraDBTest.DeviceAlerts.truncateTable(), 1.seconds)
    Await.ready(CassandraDBTest.AlertsByNode.truncateTable(), 1.seconds)
    Source(0 until 1)
      .map { x =>
        AlarmEvent(coreAlarmPayload2)
      }
      .via(alarmValidationFlowTest)
      .via(lookupOrgForNewAlertTest)
      .via(lookupExistingAlertTest)
      .runWith(persistToDbTest)
    CassandraDB.AlertsByNode
      .getByNodeIdAndSiteId(coreAlarmPayload2.nodeid, coreAlarmPayload2.alarmType, orgId1, siteId1)
      .map {
        case Some(mapping) => fail("Clear Alert persisted")
        case None          => succeed
      }
  }

}
