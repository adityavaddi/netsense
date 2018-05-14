package com.verizon.netsense.services.alert.services.alertsimulator

import java.util.{Calendar, UUID}

import akka.Done
import akka.actor.ActorSystem
import akka.kafka.ProducerSettings
import akka.kafka.scaladsl.Producer
import akka.stream.scaladsl.{Sink, Source}
import akka.stream.{ActorMaterializer, ThrottleMode}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.services.alert.customexceptions.CassandraDBException
import com.verizon.netsense.services.alert.database.CassandraDB
import com.verizon.netsense.services.alert.model.CaselType._
import com.verizon.netsense.services.alert.model._
import com.verizon.netsense.services.alert.services.database.CassandraDBTest
import com.verizon.netsense.services.alert.services.utils.AlertRequestBuilder
import com.verizon.netsense.utils.{ConfigLoader, Logging}
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.StringSerializer

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

object RestSimulator extends App with Logging with Instrumented {

  lazy val throttleLevel = 1000 //Events per second produced to kafka
  val recordCount        = 100 //Number of Records stored in Alerts Table
  val nodePrefix         = "falcon-q_"
  val orgList            = Seq("org1", "org2", "org3")
  val siteList           = Seq("site1", "site2", "site3")
  val alertId            = "alert1"

  implicit val system = ActorSystem("Alert-Rest-Simulator-Actor-System")
  implicit val mat    = ActorMaterializer()
  implicit val ec     = system.dispatcher

  private[this] val producerTimer: Timer = metrics.timer("kafka-rest-producer-timer")

  lazy val kafkaConfig = ConfigLoader.config.getConfig("kafka")
  val requestTopic     = kafkaConfig.getString("interface-service-request-topic")
  val responseTopic    = kafkaConfig.getString("interface-service-response-topic")

  val producerSettings = ProducerSettings(system, new StringSerializer, new StringSerializer)
    .withBootstrapServers(kafkaConfig.getString("host") + ":" + kafkaConfig.getInt("port"))

  def sink: Sink[ProducerRecord[String, String], Future[Done]] =
    Producer.plainSink(producerSettings)

  lazy val loadAlerts: Unit = {
    Await.ready(CassandraDBTest.DeviceAlerts.truncateTable, 5.seconds)
    for (i <- 1 to recordCount) {
      Await
        .ready(
          CassandraDB.DeviceAlerts.store(
            Alert(
              UUID.randomUUID.toString,
              Some("DeviceAlarm"),
              Some("Node Disconnected"),
              "falcon-q_" + i,
              orgList(Random.nextInt(orgList.length)),
              Some("Critical"),
              "Disconnect",
              Some("Network"),
              Some(Calendar.getInstance().toInstant.toString),
              Some(Calendar.getInstance().toInstant.toString),
              None,
              None,
              None,
              None,
              siteList(Random.nextInt(siteList.length)),
              None,
              None,
              true
            )
          ),
          2.seconds
        )
        .recover {
          case ex: Exception =>
            log.error("Error loading Alerts table: " + ex.getMessage)
            throw new CassandraDBException(ex.getMessage)
        }
      log.debug("Loading " + i + "th record to Alerts table")
    }
    Await
      .ready(
        CassandraDB.DeviceAlerts.store(
          Alert(
            alertId,
            Some("DeviceAlarm"),
            Some("Node Disconnected"),
            "falcon-q",
            orgList(Random.nextInt(orgList.length)),
            Some("Critical"),
            "Disconnect",
            Some("Network"),
            Some(Calendar.getInstance().toInstant.toString),
            Some(Calendar.getInstance().toInstant.toString),
            None,
            None,
            None,
            None,
            siteList(Random.nextInt(siteList.length)),
            None,
            None,
            true
          )
        ),
        2.seconds
      )
      .recover {
        case ex: Exception =>
          log.error("Error loading Alerts table: " + ex.getMessage)
          throw new CassandraDBException(ex.getMessage)
      }
  }

  def getRequest(requestType: String): String = {
    val orgId  = orgList(Random.nextInt(orgList.length))
    val siteId = siteList(Random.nextInt(siteList.length))
    val nodeId = nodePrefix + 2
    val request = requestType match {
      case "getAlert" =>
        AlertRequestBuilder()
          .withAlertProps(AlertProps(Some(alertId), None, None))
          .withOrgId(orgId)
          .withSiteId(siteId)
          .withReqType(GET_ALERT_FOR_ALERT_ID.value)
          .withResponseTopic(responseTopic)
          .build

      case "getAlertsForSite" =>
        AlertRequestBuilder()
          .withOrgId(orgId)
          .withSiteId(siteId)
          .withReqType(GET_ALERTS_FOR_ORG_ID_AND_SITE_ID.value)
          .withResponseTopic(responseTopic)
          .build

      case "getAlertsForNode" =>
        AlertRequestBuilder()
          .withOrgId(orgId)
          .withNodeId(nodeId)
          .withReqType(GET_ALERTS_FOR_NODE_ID.value)
          .withResponseTopic(responseTopic)
          .build

      case "getAlertSys" =>
        AlertRequestBuilder()
          .withAlertProps(AlertProps(Some(alertId), None, None))
          .withReqType(GET_ALERT_SYS.value)
          .withResponseTopic(responseTopic)
          .build

      case "dismissAlert" =>
        AlertRequestBuilder()
          .withAlertProps(AlertProps(Some(alertId), None, None))
          .withOrgId(orgId)
          .withSiteId(siteId)
          .withReqType(DISMISS_ALERT.value)
          .withResponseTopic(responseTopic)
          .build

      case _ => throw new RuntimeException("Invalid API Request Call")
    }
    request
  }

  lazy val eventSimulator = Source
    .fromIterator(() => Iterator from 1)
    .map(x => getRequest("getAlertsForSite"))
    .map { e =>
      producerTimer.time(e)
      new ProducerRecord[String, String](requestTopic, UUID.randomUUID().toString, e)
    }
    .throttle(throttleLevel, 1.seconds, throttleLevel, ThrottleMode.Shaping)
    .runWith(sink)

  log.info("Running Alert-Rest-Simulator...\nLoading sample alerts into Alerts table...")
  loadAlerts
  log.info(recordCount + " records loaded into Alerts table.\nPumping API Requests to Kafka...")
  Thread.sleep(2000)
  eventSimulator
}
