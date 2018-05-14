package com.verizon.netsense.services.eventsimulator.simulator

import java.net.ConnectException
import java.util.{Calendar, UUID}

import akka.{Done, NotUsed}
import akka.actor.ActorSystem
import akka.kafka.ProducerSettings
import akka.kafka.scaladsl.Producer
import akka.stream.scaladsl.{Flow, GraphDSL, RunnableGraph, Sink, Source}
import akka.stream._
import com.datastax.driver.core.exceptions.NoHostAvailableException
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.services.eventsimulator.database.SimulatorDB
import com.verizon.netsense.services.eventsimulator.model.CaselType._
import com.verizon.netsense.services.eventsimulator.model.{Alert, AlertProps, AlertRequestBuilder}
import com.verizon.netsense.utils.Logging
import com.verizon.netsense.services.eventsimulator.config.SimulatorConfig._
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.StringSerializer
import akka.stream.scaladsl.GraphDSL.Implicits._
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.util.Random
import scala.util.control.NonFatal

object RestSimulator extends Logging with Instrumented {

  lazy val systemDecider: Supervision.Decider = {
    case ex: ConnectException         => log.error("Unable to connect to Graphite " + ex.getMessage); Supervision.Resume
    case ex: NoHostAvailableException => log.error("Unable to connect to DB " + ex.getMessage); Supervision.Resume
    case NonFatal(ex)                 => log.error("Exception found in the Stream " + ex.getMessage); Supervision.Resume
    case ex: Exception                => log.error("Unhandled Exception seen " + ex.getMessage); Supervision.Resume;
  }

  implicit val alertRestSimulatorActorSystem = ActorSystem("Alert-Rest-Simulator-system")
  implicit val ec: ExecutionContextExecutor  = alertRestSimulatorActorSystem.dispatcher
  implicit val restmat = ActorMaterializer(
    ActorMaterializerSettings(alertRestSimulatorActorSystem)
      .withInputBuffer(Math.pow(2, 10).toInt, Math.pow(2, 20).toInt)
      .withSupervisionStrategy(systemDecider)
  )

  val throttleLevel = throttler //Events per second produced to kafka
  val eventCount   = recordCount //Number of Records stored in Alerts Table
  val nodePrefix    = "falcon-q_"
  val orgList       = Seq("org1", "org2", "org3")
  val siteList      = Seq("site1", "site2", "site3")
  val alertId       = "alert1"

  private[this] val producerTimer: Timer = metrics.timer("kafka-rest-producer-timer")

  val producerSettings = ProducerSettings(alertRestSimulatorActorSystem, new StringSerializer, new StringSerializer)
    .withBootstrapServers(bootStrapServers)

  def sink: Sink[ProducerRecord[String, String], Future[Done]] =
    Producer.plainSink(producerSettings)

  lazy val loadAlerts: Unit = {
    Await.ready(SimulatorDB.DeviceAlerts.truncateTable, 10.seconds)
    for (i <- 1 to 99) {
      Await
        .ready(
          SimulatorDB.DeviceAlerts.store(
            Alert(
              UUID.randomUUID.toString,
              "DeviceAlarm",
              "Node Disconnected",
              "falcon-q_1",
              orgList(Random.nextInt(orgList.length)),
              "Critical",
              "Disconnect",
              "Network",
              Calendar.getInstance().toInstant.toString,
              Calendar.getInstance().toInstant.toString,
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
            throw new RuntimeException(ex.getMessage)
        }
      log.debug("Loading " + i + "th record to Alerts table")
    }
    Await
      .ready(
        SimulatorDB.DeviceAlerts.store(
          Alert(
            alertId,
            "DeviceAlarm",
            "Node Disconnected",
            "falcon-q",
            orgList(Random.nextInt(orgList.length)),
            "Critical",
            "Disconnect",
            "Network",
            Calendar.getInstance().toInstant.toString,
            Calendar.getInstance().toInstant.toString,
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
          throw new RuntimeException(ex.getMessage)
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
          .withResponseTopic(isResponseTopic)
          .build

      case "getAlertsForSite" =>
        AlertRequestBuilder()
          .withOrgId(orgId)
          .withSiteId(siteId)
          .withReqType(GET_ALERTS_FOR_ORG_ID_AND_SITE_ID.value)
          .withResponseTopic(isResponseTopic)
          .build

      case "getAlertsForNode" =>
        AlertRequestBuilder()
          .withOrgId(orgId)
          .withNodeId(nodeId)
          .withReqType(GET_ALERTS_FOR_NODE_ID.value)
          .withResponseTopic(isResponseTopic)
          .build

      case "getAlertSys" =>
        AlertRequestBuilder()
          .withAlertProps(AlertProps(Some(alertId), None, None))
          .withReqType(GET_ALERT_SYS.value)
          .withResponseTopic(isResponseTopic)
          .build

      case "dismissAlert" =>
        AlertRequestBuilder()
          .withAlertProps(AlertProps(Some(alertId), None, None))
          .withOrgId(orgId)
          .withSiteId(siteId)
          .withReqType(DISMISS_ALERT.value)
          .withResponseTopic(isResponseTopic)
          .build

      case _ => throw new RuntimeException("Invalid API Request Call")
    }
    request
  }

  val requestSource: Source[ProducerRecord[String, String], NotUsed] = Source(1 to eventCount)
    .map(x => getRequest("getAlertsForSite"))
    .map { e =>
      producerTimer.time(e)
      new ProducerRecord[String, String](isRequestTopic, UUID.randomUUID().toString, e)
    }

  val throttle =
    Flow[ProducerRecord[String, String]].throttle(throttleLevel, 1.second, throttleLevel, ThrottleMode.Shaping)

  lazy val runnableGraphCommit: RunnableGraph[NotUsed] =
    RunnableGraph.fromGraph(g = GraphDSL.create() { implicit graphBuilder =>
      val restSource: Outlet[ProducerRecord[String, String]] = graphBuilder.add(requestSource).out

      val throttleFlow = graphBuilder.add(throttle)

      val restSink: Inlet[ProducerRecord[String, String]] = graphBuilder.add(sink).in

      restSource ~> throttleFlow ~> restSink

      ClosedShape
    })
}
