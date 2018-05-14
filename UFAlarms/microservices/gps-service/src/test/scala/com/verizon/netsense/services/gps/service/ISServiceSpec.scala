package com.verizon.netsense.services.gps.service

import com.outworkers.phantom.connectors.KeySpace
import com.verizon.netsense.services.gps.BaseSpec
import com.verizon.netsense.services.gps.config.ISServiceKafkaConfig
import com.verizon.netsense.services.gps.constants.TestData.{orgHierarchy, _}
import com.verizon.netsense.services.gps.db.Defaults
import com.verizon.netsense.services.gps.helper.KafkaConfig
import com.verizon.netsense.services.gps.model._
import com.verizon.netsense.services.gps.util.ObjectMapperUtil
import net.manub.embeddedkafka.{EmbeddedKafka, EmbeddedKafkaConfig}

import scala.concurrent.Await

class ISServiceSpec extends BaseSpec {

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

  "Kafka Stream" should "start the embedded kafka & close the probe" in new ISServiceKafkaConfig {
    val test = createEventProbe(isRequestTopic)
    probeistopic.ensureSubscription()
    probeistopic.cancel()

  }

  it should "get Gps from database through IS service request successfully" in new ISServiceKafkaConfig {
    database.GpsRecord.storeGps(gps)
    Await.result(produceEvent(KafkaConfig.isRequestTopic,getGpsByNodeIdAppRequest),remainingOrDefault)

    val consumedrecord = probeistopic.requestNext(remainingOrDefault)

    val appResponse: Option[AppSuccessResponse[GpsIsModel]] =
      Await.result(ObjectMapperUtil.fromJsonAsync[AppSuccessResponse[GpsIsModel]](consumedrecord.value()),remainingOrDefault)

    appResponse match {
      case None => assert(false)
      case Some(appres) => {
        appres.messageid mustBe messageId
        appres.response.success mustBe true
        appres.response.result.orgid mustBe orgId
        appres.response.result.siteid mustBe siteId
        appres.response.result.latitude mustBe Some(lat.toString)
        appres.response.result.longitude mustBe Some(long.toString)}
    }
  }


  it should "get Gps by id should send Org Props missing failure response" in new ISServiceKafkaConfig {
    database.GpsRecord.storeGps(gps)
    Await.result(produceEvent(KafkaConfig.isRequestTopic,getGpsByNodeIdAppRequestWithMissingOrgProps),remainingOrDefault)

    val consumedrecord = probeistopic.requestNext(remainingOrDefault)

    val appResponse: Option[AppFailureResponse] =
      Await.result(ObjectMapperUtil.fromJsonAsync[AppFailureResponse](consumedrecord.value()),remainingOrDefault)

    appResponse match {
      case None => assert(false)
      case Some(appRes) => {
        appRes.messageid mustBe  messageId
        appRes.response.status mustBe 400
        appRes.response.error mustBe "Invalid request: Org Props are missing"
      }
    }
  }

  it should "get Gps by id should send Site Props missing failure response" in new ISServiceKafkaConfig {
    database.GpsRecord.storeGps(gps)
    Await.result(produceEvent(KafkaConfig.isRequestTopic,getGpsByNodeIdAppRequestWithMissingSiteProps),remainingOrDefault)

    val consumedrecord = probeistopic.requestNext(remainingOrDefault)

    val appResponse: Option[AppFailureResponse] =
      Await.result(ObjectMapperUtil.fromJsonAsync[AppFailureResponse](consumedrecord.value()),remainingOrDefault)

    appResponse match {
      case None => assert(false)
      case Some(appRes) => {
        appRes.messageid mustBe  messageId
        appRes.response.status mustBe 400
        appRes.response.error mustBe "Invalid request: Site Props are missing"
      }
    }
  }

  it should "get Gps by id should send Node Props missing failure response" in new ISServiceKafkaConfig {
    database.GpsRecord.storeGps(gps)
    Await.result(produceEvent(KafkaConfig.isRequestTopic,getGpsByNodeIdAppRequestWithMissingNodeProps),remainingOrDefault)

    val consumedrecord = probeistopic.requestNext(remainingOrDefault)

    val appResponse: Option[AppFailureResponse] =
      Await.result(ObjectMapperUtil.fromJsonAsync[AppFailureResponse](consumedrecord.value()),remainingOrDefault)

    appResponse match {
      case None => assert(false)
      case Some(appRes) => {
        appRes.messageid mustBe  messageId
        appRes.response.status mustBe 400
        appRes.response.error mustBe "Invalid request: Node Props are missing"
      }
    }
  }

  it should "get Gps by id should send required fields are missing failure response" in new ISServiceKafkaConfig {
    database.GpsRecord.storeGps(gps)
    Await.result(produceEvent(KafkaConfig.isRequestTopic,getGpsByNodeIdAppRequestWithManyMissingFields),remainingOrDefault)

    val consumedrecord = probeistopic.requestNext(remainingOrDefault)

    val appResponse: Option[AppFailureResponse] =
      Await.result(ObjectMapperUtil.fromJsonAsync[AppFailureResponse](consumedrecord.value()),remainingOrDefault)

    appResponse match {
      case None => assert(false)
      case Some(appRes) => {
        appRes.messageid mustBe  messageId
        appRes.response.status mustBe 400
        appRes.response.error mustBe "Invalid request: Required Filed are missing"
      }
    }
  }


  it should "get Gps By org and site Should return response successfully" in new ISServiceKafkaConfig {
    database.GpsRecord.storeGps(gps)
    database.GpsRecord.storeGps(gps2)

    Await.result(produceEvent(KafkaConfig.isRequestTopic,getGpsByOrgAndSiteIdAppRequest),remainingOrDefault)

    val collectProduceeRecord = probeistopic.requestNext(remainingOrDefault)

    val appResponse: Option[AppSuccessResponse[List[GpsIsModel]]] =
      Await.result(ObjectMapperUtil.fromJsonAsync[AppSuccessResponse[List[GpsIsModel]]](collectProduceeRecord.value()),remainingOrDefault)

    appResponse match {
      case None => assert(false)
      case Some(appRes) => {
        appRes.messageid mustBe messageId
        appRes.response.success mustBe true
        appRes.response.result.size mustBe 2
      }
    }

  }

  it should "get Gps by Org and SiteId should send Org Props missing failure response" in new ISServiceKafkaConfig {
    database.GpsRecord.storeGps(gps)
    Await.result(produceEvent(KafkaConfig.isRequestTopic,getGpsByOrgAndSiteIdAppRequestMissingSiteProps),remainingOrDefault)

    val consumedrecord = probeistopic.requestNext(remainingOrDefault)

    val appResponse: Option[AppFailureResponse] =
      Await.result(ObjectMapperUtil.fromJsonAsync[AppFailureResponse](consumedrecord.value()),remainingOrDefault)

    appResponse match {
      case None => assert(false)
      case Some(appRes) => {
        appRes.messageid mustBe  messageId
        appRes.response.status mustBe 400
        appRes.response.error mustBe "Invalid request: Site Props are missing"
      }
    }
  }

  it should "get Gps by Org and SiteId should send Site Props missing failure response" in new ISServiceKafkaConfig {
    database.GpsRecord.storeGps(gps)
    Await.result(produceEvent(KafkaConfig.isRequestTopic,getGpsByOrgAndSiteIdAppRequesttWithMissingOrgProps),remainingOrDefault)

    val consumedrecord = probeistopic.requestNext(remainingOrDefault)

    val appResponse: Option[AppFailureResponse] =
      Await.result(ObjectMapperUtil.fromJsonAsync[AppFailureResponse](consumedrecord.value()),remainingOrDefault)

    appResponse match {
      case None => assert(false)
      case Some(appRes) => {
        appRes.messageid mustBe  messageId
        appRes.response.status mustBe 400
        appRes.response.error mustBe "Invalid request: Org Props are missing"
      }
    }
  }

  it should "get Gps by Org and SiteId should send many required fields are missing failure response" in new ISServiceKafkaConfig {
    database.GpsRecord.storeGps(gps)
    Await.result(produceEvent(KafkaConfig.isRequestTopic,getGpsByOrgAndSiteIdAppRequestWithManyMissingFields),remainingOrDefault)

    val consumedrecord = probeistopic.requestNext(remainingOrDefault)

    val appResponse: Option[AppFailureResponse] =
      Await.result(ObjectMapperUtil.fromJsonAsync[AppFailureResponse](consumedrecord.value()),remainingOrDefault)

    appResponse match {
      case None => assert(false)
      case Some(appRes) => {
        appRes.messageid mustBe  messageId
        appRes.response.status mustBe 400
        appRes.response.error mustBe "Invalid request: Required Filed are missing"
      }
    }
  }


}
