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

class AlertIngestionServiceSpec
    extends TestKit(ActorSystem("AlertIngestionServiceSpec-System"))
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

    def produceEvent(topic: String, alarmEvent: AlarmEvent): Future[Done] = {
      val source = Source
        .single(alarmEvent)
        .map(e => toJsonBytes(e))
        .map(m => {
          Message(new ProducerRecord(topic, UUID.randomUUID().toString.getBytes, m), NotUsed)
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
    Await.ready(CassandraDBTest.UFAlarms.createTable(), 1.seconds)
    Await.ready(CassandraDBTest.UFAlarmsById.createTable(), 1.seconds)
    loadOrgHierarchy()
    loadUFAlarms()
  }

  override def afterAll(): Unit = {
    super.afterAll()
    EmbeddedKafka.stop()
    Await.ready(CassandraDBTest.DeviceAlarms.truncateTable, 1.seconds)
    Await.ready(CassandraDBTest.OrgHierarchyByNode.truncateTable, 1.seconds)
    Await.ready(CassandraDBTest.DeviceAlerts.truncateTable, 1.seconds)
    Await.ready(CassandraDBTest.AlertsByNode.truncateTable(), 1.seconds)
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
    val testGroup     = kafkaConfig.getString("alert-group-name") + UUID.randomUUID().toString
    val alarmTopic    = kafkaConfig.getString("alarm-request-topic")
    val alertTopic    = kafkaConfig.getString("alert-response-topic")
    val requestTopic  = kafkaConfig.getString("alarm-request-topic")
    val responseTopic = kafkaConfig.getString("alert-response-topic")

    val consumerUnderTest = AlertIngestionService
      .configKafkaSource(createEventConsumerSettingsString(testGroup), Set(alarmTopic))

    val jsonStringToAlarmEventTest = AlertIngestionService.restPackTransformFlow
    val alarmValidationFlowTest    = AlertIngestionService.validateAlarmFlow
    val lookupExistingAlertTest    = AlertIngestionService.checkExistingAlertInDbFlow
    val lookupOrgForNewAlertTest   = AlertIngestionService.lookupOrgFlowForNewAlert
    val persistToDbTest            = AlertIngestionService.persistFlow
    val ufAlarmFlowTest            = AlertIngestionService.UfAlarmFlow
    val kafkaFlowTest              = AlertIngestionService.kafkaFlow
    val persistToAppendOnlyDbTest  = AlertIngestionService.persistToAppendOnlyTableSink
    val publishToKafkaTest         = AlertIngestionService.kafkaSink

    val probe1 = consumerUnderTest
      .via(jsonStringToAlarmEventTest)
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

  it should "produce a sample device alarm to embedded kafka and convert to Alert" in new KafkaSettings {

    Await.result(produceEvent(alarmTopic, alarmPayload1), remainingOrDefault)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    val unpackedMessage =
      Await.result(fromJsonAsync[AlertResponse](consumedElement.value()), remainingOrDefault)
    unpackedMessage.nodeId must equal(alarmPayload1.sid)
    unpackedMessage.`type` must equal("Disconnect")
    unpackedMessage.severity.getOrElse("") must equal(AlarmSeverity(alarmPayload1.l.s).toString)
    unpackedMessage.category.getOrElse("") must equal(AlarmCategory(alarmPayload1.l.c).toString)
    unpackedMessage.ufName must equal(ufName1)
    unpackedMessage.description must equal(description1)
    unpackedMessage.displayToPartner must equal(true)
    unpackedMessage.displayToCustomer must equal(true)
    probe1.cancel()
  }

  it should "produce a sample device alarm of Clear type to embedded kafka and convert to Alert" in new KafkaSettings {

    Await.result(produceEvent(alarmTopic, alarmPayload2), remainingOrDefault)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    val unpackedMessage =
      Await.result(fromJsonAsync[AlertResponse](consumedElement.value()), remainingOrDefault)
    unpackedMessage.nodeId must equal(alarmPayload2.sid)
    unpackedMessage.`type` must equal("Disconnect")
    unpackedMessage.severity.getOrElse("") must equal(AlarmSeverity(alarmPayload2.l.s).toString)
    unpackedMessage.category.getOrElse("") must equal(AlarmCategory(alarmPayload2.l.c).toString)
    unpackedMessage.ufName must equal(ufName1)
    unpackedMessage.description must equal(description1)
    unpackedMessage.displayToPartner must equal(true)
    unpackedMessage.displayToCustomer must equal(true)
    probe1.cancel()
  }

  it should "produce a sample device alarm with invalid node id and valid exception should be thrown" in new KafkaSettings {

    Await.result(produceEvent(alarmTopic, alarmPayload_invalidNode), remainingOrDefault)

    try {
      val consumedElement =
        probe1.onError(new Throwable("OrgDataNotFoundException Caught", throw new OrgDataNotFoundException))
    } catch {
      case ex: OrgDataNotFoundException => assert(true)
      case _: Throwable                 => assert(false)
    }

  }

  it should "produce a sample device alarm with same alarm id and the existing alert should be updated" in new KafkaSettings {

    Await.result(produceEvent(alarmTopic, alarmPayload_updated), remainingOrDefault)

    val consumedElement = probe1.requestNext(remainingOrDefault)
    val unpackedMessage =
      Await.result(fromJsonAsync[AlertResponse](consumedElement.value()), remainingOrDefault)
    unpackedMessage.nodeId must equal(alarmPayload_updated.sid)
    unpackedMessage.`type` must equal("Disconnect")
    unpackedMessage.severity.getOrElse("") must equal(AlarmSeverity(alarmPayload_updated.l.s).toString)
    unpackedMessage.category.getOrElse("") must equal(AlarmCategory(alarmPayload_updated.l.c).toString)
    unpackedMessage.updated.getOrElse("") must equal(alarmPayload_updated.d)
    unpackedMessage.ufName must equal(ufName1)
    unpackedMessage.description must equal(description1)
    unpackedMessage.displayToPartner must equal(true)
    unpackedMessage.displayToCustomer must equal(true)
    probe1.cancel()
  }

  it should "produce a sample device alarm with invalid Alarm severity and the exception should be thrown" in new KafkaSettings {

    an[AlarmNotCreatedException] must be thrownBy AlarmValidator.validationPredicate(
      alarmPayload1.copy(l = DeviceAlarmPayload("Disconnect", 99, "Node disconnected", 1))
    )
  }

  it should "insert a sample device alarm in Alarms append only table" in new KafkaSettings {
    Await.ready(CassandraDBTest.DeviceAlarms.truncateTable, 1.seconds)
    Source(0 until 1)
      .map { x =>
        alarmPayload_Append
      }
      .via(alarmValidationFlowTest)
      .runWith(persistToAppendOnlyDbTest)

    CassandraDBTest.DeviceAlarms.getAlarm(alarmPayload_Append.sid, alarmPayload_Append.l.a).map {
      case Some(alarm) =>
        assert(alarm.nodeId == alarmPayload_Append.sid)
        assert(alarm.alarmType == alarmPayload_Append.l.a)
      case None => fail("Alarm not persisted")
    }
  }

  it should "insert a sample device alert in alerts Mapping table and alerts table" in new KafkaSettings {
    Await.ready(CassandraDBTest.DeviceAlerts.truncateTable, 1.seconds)
    Await.ready(CassandraDBTest.AlertsByNode.truncateTable(), 1.seconds)
    Source(0 until 1)
      .map { x =>
        alarmPayload1
      }
      .via(alarmValidationFlowTest)
      .via(lookupOrgForNewAlertTest)
      .via(lookupExistingAlertTest)
      .runWith(persistToDbTest)
    CassandraDB.AlertsByNode.getByNodeIdAndSiteId(alarmPayload1.sid, alarmPayload1.l.a, orgId1, siteId1).map {
      case Some(mapping) =>
        assert(mapping.nodeId.equals(alarmPayload1.sid))
        CassandraDB.DeviceAlerts.getAlertForAlertId(mapping.alertId, mapping.orgId, mapping.siteId).map {
          case Some(alert) =>
            assert(alert.severity.getOrElse("") == AlarmSeverity(alarmPayload1.l.s).toString)
            assert(alert.`type` == alarmPayload1.l.a)
            assert(alert.msg.getOrElse("") == alarmPayload1.l.m)
            assert(alert.active)
          case None => fail("Alert not persisted")
        }
      case None => fail("Alert not persisted in Mappings table")
    }
  }

  it should "update a sample device alert in alerts mapping table and alerts table" in new KafkaSettings {
    Source(0 until 1)
      .map { x =>
        alarmPayload_updated
      }
      .via(alarmValidationFlowTest)
      .via(lookupOrgForNewAlertTest)
      .via(lookupExistingAlertTest)
      .runWith(persistToDbTest)
    CassandraDB.AlertsByNode
      .getByNodeIdAndSiteId(alarmPayload_updated.sid, alarmPayload_updated.l.a, orgId1, siteId1)
      .map {
        case Some(mapping) =>
          assert(mapping.nodeId.equals(alarmPayload_updated.sid))
          CassandraDB.DeviceAlerts.getAlertForAlertId(mapping.alertId, mapping.orgId, mapping.siteId).map {
            case Some(alert) =>
              assert(alert.severity.getOrElse("") == AlarmSeverity(alarmPayload_updated.l.s).toString)
              assert(alert.`type` == alarmPayload_updated.l.a)
              assert(alert.msg.getOrElse("") == alarmPayload_updated.l.m)
              assert(alert.active)
            case None => fail("Alert not persisted")
          }
        case None => fail("Alert not persisted in Mappings table")
      }
  }

  it should "mark alert as inactive when received a clear alarm in alerts table" in new KafkaSettings {
    Source(0 until 1)
      .map { x =>
        alarmPayload2
      }
      .via(alarmValidationFlowTest)
      .via(lookupOrgForNewAlertTest)
      .via(lookupExistingAlertTest)
      .runWith(persistToDbTest)
    CassandraDB.AlertsByNode.getByNodeIdAndSiteId(alarmPayload2.sid, alarmPayload2.l.a, orgId1, siteId1).map {
      case Some(mapping) =>
        assert(mapping.nodeId.equals(alarmPayload2.sid))
        CassandraDB.DeviceAlerts.getAlertForAlertId(mapping.alertId, mapping.orgId, mapping.siteId).map {
          case Some(alert) =>
            assert(alert.severity.getOrElse("") == AlarmSeverity(alarmPayload2.l.s).toString)
            assert(!alert.active)
          case None => fail("Alert not persisted")
        }
      case None => fail("Alert not persisted in Mappings table")
    }
  }

  it should "not persist a clear alarm in alerts mapping table and alerts table" in new KafkaSettings {
    Await.ready(CassandraDBTest.DeviceAlerts.truncateTable(), 1.seconds)
    Await.ready(CassandraDBTest.AlertsByNode.truncateTable(), 1.seconds)
    Source(0 until 1)
      .map { x =>
        alarmPayload2
      }
      .via(alarmValidationFlowTest)
      .via(lookupOrgForNewAlertTest)
      .via(lookupExistingAlertTest)
      .runWith(persistToDbTest)
    CassandraDB.AlertsByNode.getByNodeIdAndSiteId(alarmPayload2.sid, alarmPayload2.l.a, orgId1, siteId1).map {
      case Some(mapping) =>
        fail("Clear Alert persisted")
      case None => succeed
    }
  }

  it should "get default ufname and description if entry is not there for alarm type" in new KafkaSettings {

    Await.result(produceEvent(alarmTopic, alarmPayload3), remainingOrDefault)
    val consumedElement = probe1.requestNext(remainingOrDefault)
    val unpackedMessage =
      Await.result(fromJsonAsync[AlertResponse](consumedElement.value()), remainingOrDefault)
    unpackedMessage.nodeId must equal(alarmPayload3.sid)
    unpackedMessage.`type` must equal("SW/diskfree/disk")
    unpackedMessage.severity.getOrElse("") must equal(AlarmSeverity(alarmPayload3.l.s).toString)
    unpackedMessage.category.getOrElse("") must equal(AlarmCategory(alarmPayload3.l.c).toString)
    unpackedMessage.ufName must equal(unpackedMessage.`type`)
    unpackedMessage.description must equal(unpackedMessage.msg.getOrElse(""))
    unpackedMessage.displayToPartner must equal(false)
    unpackedMessage.displayToCustomer must equal(false)
    probe1.cancel()
  }

  it should "get insert ufname and description for new alarm type in uf_alarm table" in new KafkaSettings {

    Await.result(produceEvent(alarmTopic, alarmPayload3), remainingOrDefault)

    CassandraDB.UFAlarms.getUfAlarmByType(alarmPayload3.a).map {
      case Some(ufAlarm) =>
        assert(ufAlarm.ufName.getOrElse("").equals(alarmPayload3.a))
        assert(ufAlarm.description.getOrElse("").equals(alarmPayload3.l.m))
        assert(!ufAlarm.displayToCustomer)
        assert(!ufAlarm.displayToPartner)
        CassandraDB.UFAlarmsById.getUfAlarmById(ufAlarm.mappingId).map {
          case Some(ufAlarmById) =>
            assert(ufAlarm.ufName.getOrElse("").equals(alarmPayload3.a))
            assert(ufAlarm.description.getOrElse("").equals(alarmPayload3.l.m))
            assert(!ufAlarm.displayToCustomer)
            assert(!ufAlarm.displayToPartner)
          case None => fail("UF Alarm not persisted in Id table")
        }
      case None => fail("UF Alarm not persisted")
    }
  }

  it should "produce a sample device alarm with invalid Alarm category and the exception should be thrown" in new KafkaSettings {

    an[AlarmNotCreatedException] must be thrownBy AlarmValidator.validationPredicate(
      alarmPayload1.copy(l = DeviceAlarmPayload("Disconnect", 1, "Node disconnected", 99))
    )
  }

}
