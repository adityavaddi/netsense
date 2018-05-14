package com.verizon.netsense.services.gps.service

import akka.stream.scaladsl.Source
import com.outworkers.phantom.connectors.KeySpace
import com.verizon.netsense.services.gps.BaseSpec
import com.verizon.netsense.services.gps.config.SchEventKafkaConfig
import com.verizon.netsense.services.gps.constants.TestData._
import com.verizon.netsense.services.gps.db.Defaults
import com.verizon.netsense.services.gps.exceptions.OrgDataNotFoundException
import com.verizon.netsense.services.gps.helper.KafkaConfig
import com.verizon.netsense.services.gps.model.Gps
import com.verizon.netsense.services.gps.util.ObjectMapperUtil
import net.manub.embeddedkafka.{EmbeddedKafka, EmbeddedKafkaConfig}
import org.apache.kafka.clients.producer.ProducerRecord

import scala.concurrent.Await

class SchEventIngestionServiceSpec extends BaseSpec{

  implicit val embeddedKafkaConfig = EmbeddedKafkaConfig(kafkaPort = embeddedKafkaPort, zooKeeperPort = embeddedZkPort)
  implicit val space: KeySpace = KeySpace(Defaults.cassandraKeyspace)

  def loadOrgHierArchy(): Unit = {
    database.OrgHierarchy.storeHierArchy(orgHierarchy)
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    database.GpsRecord.create.ifNotExists()
    database.OrgHierarchy.create.ifNotExists()
    loadOrgHierArchy()
  }

  override def afterAll(): Unit = {
    super.afterAll()
    database.GpsRecord.truncate()
    database.GpsRecord.truncate()

  }


  override def beforeEach(): Unit = {
    super.beforeAll()
    EmbeddedKafka.start()
  }

  override def afterEach(): Unit = {
    super.beforeAll()
    EmbeddedKafka.stop()
  }

  "Kafka Stream" should "start the embedded kafka & close the probe" in new SchEventKafkaConfig {
    val test = createEventProbe(KafkaConfig.gpsSchTopic)
    probe.ensureSubscription()
    probe.cancel()

  }

  it should "produce sch gps event populate Gps model suceessfully" in new SchEventKafkaConfig {

    Await.result(produceEvent(schTopic,schGpsEvent),remainingOrDefault)

    val consumedEvent: ProducerRecord[String,String] = probe.requestNext(remainingOrDefault)

    val unpackMessage = Await.result(ObjectMapperUtil.fromJsonAsync[Gps](consumedEvent.value()),remainingOrDefault)

    unpackMessage.get.nodeid eq schGpsEvent.sid
    unpackMessage.get.latitude  mustBe Some(schGpsEvent.l.lat)
    unpackMessage.get.longitude mustBe Some(schGpsEvent.l.lon)
    unpackMessage.get.lonuseradded mustBe Some(schGpsEvent.l.lon)
    unpackMessage.get.latuseradded mustBe Some(schGpsEvent.l.lat)
    unpackMessage.get.orgid mustBe orgId
    unpackMessage.get.siteid mustBe siteId
    probe.cancel()
  }


  it should "produce a gps event with invalid node id and valid exception should be thrown" in new SchEventKafkaConfig {

    Await.result(produceEvent(schTopic,schGpsEventWithNoOrgHierarchy), remainingOrDefault)

    try {
      val consumedElement =
        probe.onError(new Throwable("OrgDataNotFoundException Caught", throw new OrgDataNotFoundException()))
    } catch {
      case ex: OrgDataNotFoundException => assert(true)
      case _: Throwable                 => assert(false)
    }

  }

  it should "produce sch gps event and persist into database successfully" in new SchEventKafkaConfig {
    Source(0 until 1)
      .map { x =>
        Some(schGpsEvent)
      }
      .via(validateGpsEvent)
      .via(populateOrgHierArchy)
      .via(populateGpsModelFlow)
      .runWith(persistToCassandra)

    database.GpsRecord.getGps(schGpsEvent.sid,orgId,siteId).map(x => x match {
      case Some(x) => assert(true)
      case None => assert(false)
    })



  }

  it should "produce sch gps event and failed to persist into database" in new SchEventKafkaConfig {
    Source(0 until 1)
      .map { x =>
        Some(schGpsEventWithNoOrgHierarchy)
      }
      .via(validateGpsEvent)
      .via(populateOrgHierArchy)
      .via(populateGpsModelFlow)
      .runWith(persistToCassandra)

    database.GpsRecord.getGps(schGpsEventWithNoOrgHierarchy.sid, orgId, siteId).map(x => x match {
      case Some(x) => assert(false)
      case None => assert(true)
    })
  }

}
