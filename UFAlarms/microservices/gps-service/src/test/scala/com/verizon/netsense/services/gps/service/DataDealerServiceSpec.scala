package com.verizon.netsense.services.gps.service

import akka.stream.scaladsl.Source
import com.outworkers.phantom.connectors.KeySpace
import com.verizon.netsense.services.gps.BaseSpec
import com.verizon.netsense.services.gps.config.DataDealerKafkaConfig
import com.verizon.netsense.services.gps.constants.TestData.{orgHierarchy, _}
import com.verizon.netsense.services.gps.db.Defaults
import com.verizon.netsense.services.gps.exceptions.{GpsPropsEmptyException, InvalidCaselTypeException, OrgDataNotFoundException}
import com.verizon.netsense.services.gps.model.AppRequest
import net.manub.embeddedkafka.{EmbeddedKafka, EmbeddedKafkaConfig}

import scala.concurrent.Await

class DataDealerServiceSpec extends BaseSpec {

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

  "Kafka Stream" should "start the embedded kafka & close the probe" in new DataDealerKafkaConfig {
    val test = createEventProbe(ddRequestTopic)
    probeddtopic.ensureSubscription()
    probeddtopic.cancel()

  }

  it should "produce App request and consume via probe successfully" in new DataDealerKafkaConfig {
    Await.result(produceEvent[AppRequest](ddRequestTopic,validAppRequest),remainingOrDefault)

    val consumeElement = probeddtopic.requestNext(remainingOrDefault)
    val gps = consumeElement

    gps.nodeid eq nodeId
    gps.orgid eq orgId
    gps.siteid eq siteId
    gps.latuseradded mustBe Some(lat)
    gps.lonuseradded mustBe  Some(long)
    probeddtopic.cancel()

  }

  it should "throw Gps Props empty exception" in new DataDealerKafkaConfig {

    Await.result(produceEvent[AppRequest](ddRequestTopic,emptyGpsPropsappRequest),remainingOrDefault)

    try {
      probeddtopic.onError(new Throwable("Gps properties are missing.",new GpsPropsEmptyException()))
    } catch {
      case ex: GpsPropsEmptyException => assert(true)
      case _: Throwable => assert(false)
    }

  }

  it should "throw Invalid Casel type exception" in new DataDealerKafkaConfig {

    Await.result(produceEvent[AppRequest](ddRequestTopic,wrongCaselappRequest),remainingOrDefault)

    try {
      probeddtopic.onError(new Throwable("Casel Type is invlid.",new InvalidCaselTypeException()))
    } catch {
      case ex: InvalidCaselTypeException => assert(true)
      case _: Throwable => assert(false)
    }

  }

  it should "throw Org Not Found exception" in new DataDealerKafkaConfig {

    Await.result(produceEvent[AppRequest](ddRequestTopic,nonExisteOrgSiteappRequest),remainingOrDefault)

    try {
      probeddtopic.onError(new Throwable("No org data found for the node.",new OrgDataNotFoundException()))
    } catch {
      case ex: OrgDataNotFoundException => assert(true)
      case _: Throwable => assert(false)
    }

  }


  it should "throw Unknown Org Found exception" in new DataDealerKafkaConfig {

    Await.result(produceEvent[AppRequest](ddRequestTopic,unknownValueOrgSiteappRequest),remainingOrDefault)

    try {
      probeddtopic.onError(new Throwable("No org data found for the node.",new OrgDataNotFoundException()))
    } catch {
      case ex: OrgDataNotFoundException => assert(true)
      case _: Throwable => assert(false)
    }

  }

  it should "persist Gps into Database" in new DataDealerKafkaConfig {

    Await.result(produceEvent(ddRequestTopic,validAppRequest),remainingOrDefault)

    Source(0 until 1).map(x => Some(validAppRequest))
      .via(validateRequestFlow)
      .via(processAppRequestFlow)
      .via(populateGpsModelFlow)
      .runWith(persistToDbFlow)


    database.GpsRecord.getGps(nodeId,orgId,siteId).map(x => x match {
      case None => assert(false)
      case Some(gps) => {
        gps.lonuseradded mustBe Some(lat)
        gps.latuseradded mustBe Some(long)
      }

    })

  }

  it should "fail to persist Gps into Database" in new DataDealerKafkaConfig {

    Await.result(produceEvent(ddRequestTopic,validAppRequest),remainingOrDefault)

    Source(0 until 1).map(x => Some(nonExisteOrgSiteWithDifferentNodeIdappRequest))
      .via(validateRequestFlow)
      .via(processAppRequestFlow)
      .via(populateGpsModelFlow)
      .runWith(persistToDbFlow)


    database.GpsRecord.getGps(nodeId,orgId,siteId).map(x => x match {
      case None => assert(true)
      case Some(gps) => assert(false)

    })

  }




}
