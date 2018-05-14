package com.verizon.netsense.services.eventsimulator.simulator

import java.net.ConnectException
import java.util.{Calendar, UUID}

import akka.{Done, NotUsed}
import akka.actor.ActorSystem
import akka.kafka.ProducerSettings
import akka.kafka.scaladsl.Producer
import akka.stream._
import akka.stream.scaladsl.GraphDSL.Implicits._
import akka.stream.scaladsl.{Flow, GraphDSL, Merge, RunnableGraph, Sink, Source}
import com.datastax.driver.core.exceptions.NoHostAvailableException
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.services.eventsimulator.database.SimulatorDB
import com.verizon.netsense.services.eventsimulator.model.{AlarmEvent, CoreAlarmEvent, DeviceAlarmPayload, OrgHierarchy}
import com.verizon.netsense.utils.Logging
import com.verizon.netsense.services.eventsimulator.config.SimulatorConfig._
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.StringSerializer

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.util.control.NonFatal

object AlertIngestionSimulator extends Logging with Instrumented {

  lazy val systemDecider: Supervision.Decider = {
    case ex: ConnectException         => log.error("Unable to connect to Graphite " + ex.getMessage); Supervision.Resume
    case ex: NoHostAvailableException => log.error("Unable to connect to DB " + ex.getMessage); Supervision.Resume
    case NonFatal(ex)                 => log.error("Exception found in the Stream " + ex.getMessage); Supervision.Resume
    case ex: Exception                => log.error("Unhandled Exception seen " + ex.getMessage); Supervision.Resume;
  }

  implicit val alertSimulatorActorSystem    = ActorSystem("Alert-Simulator-system")
  implicit val ec: ExecutionContextExecutor = alertSimulatorActorSystem.dispatcher
  implicit val alertmat = ActorMaterializer(
    ActorMaterializerSettings(alertSimulatorActorSystem)
      .withInputBuffer(Math.pow(2, 10).toInt, Math.pow(2, 20).toInt)
      .withSupervisionStrategy(systemDecider)
  )

  val throttleLevel                                = throttler //Events per second produced to kafka
  val eventCount                                   = recordCount //Alarm generation count
  val nodePrefix                                   = "falcon-q_"
  val coreNodePrefix                               = "unode-v4_"
  val severity                                     = 3
  val category                                     = 1
  val msg                                          = "Disk Volume Fail"
  val alarmType                                    = "SW/storage/volmissing/data_"
  val sensorType                                   = "SW/storage/volmissing/data"
  val solicitationType                             = "UNSOL"
  val path                                         = "v1/falcon-q_1/out/UNSOL/alarm/{alarmid}"
  val random                                       = new scala.util.Random()
  private[this] val coreAlarmProducerTimer: Timer  = metrics.timer("kafka-core-alarm-producer-timer")
  private[this] val videoAlarmProducerTimer: Timer = metrics.timer("kafka-video-alarm-producer-timer")

  val producerSettings = ProducerSettings(alertSimulatorActorSystem, new StringSerializer, new StringSerializer)
    .withBootstrapServers(bootStrapServers)

  def sink1: Sink[ProducerRecord[String, String], Future[Done]] =
    Producer.plainSink(producerSettings)

  def sink2: Sink[ProducerRecord[String, String], Future[Done]] =
    Producer.plainSink(producerSettings)

  lazy val loadOrgHierarchy: Unit = {
    Await.ready(SimulatorDB.OrgHierarchy.truncateTable, 10.seconds)
    Await.ready(SimulatorDB.DeviceAlerts.truncateTable, 10.seconds)
    Await.ready(SimulatorDB.DeviceAlarms.truncateTable, 10.seconds)
    for (i <- 1 to nodeCount) {
      Await
        .ready(
          SimulatorDB.OrgHierarchy.store(
            OrgHierarchy(nodePrefix + i.toString,
                         None,
                         UUID.randomUUID().toString,
                         None,
                         UUID.randomUUID().toString,
                         None)
          ),
          2.seconds
        )
        .recover {
          case ex: Exception =>
            log.error("Error loading org hierarchy table: " + ex.getMessage)
            throw new RuntimeException(ex.getMessage)
        }
      Await
        .ready(
          SimulatorDB.OrgHierarchy.store(
            OrgHierarchy(coreNodePrefix + i.toString,
                         None,
                         UUID.randomUUID().toString,
                         None,
                         UUID.randomUUID().toString,
                         None)
          ),
          2.seconds
        )
        .recover {
          case ex: Exception =>
            log.error("Error loading org hierarchy table: " + ex.getMessage)
            throw new RuntimeException(ex.getMessage)
        }
      log.debug("Loading " + i + "th record to Org Hierarchy table")
    }
  }

  def getAlarm(i: Long): AlarmEvent = {
    val alarmPayload = DeviceAlarmPayload(alarmType + i.toString, severity, msg, category)
    val alarmEvent = AlarmEvent(
      UUID.randomUUID().toString,
      solicitationType,
      sensorType,
      path,
      alarmPayload,
      nodePrefix + (1 + random.nextInt(( nodeCount - 1) + 1)).toString,
      Calendar.getInstance().toInstant.toString
    )
    log.debug("Alarm " + i + " sent with payload: " + alarmEvent.toJSON)
    alarmEvent
  }

  def getCoreAlarm(i: Long): CoreAlarmEvent = {
    val coreAlarm = CoreAlarmEvent("DeviceAlarm",
                                   coreNodePrefix + (1 + random.nextInt(( nodeCount - 1) + 1)).toString,
                                   "Comm_Fail" + i.toString,
                                   "Minor",
                                   msg)
    log.debug("Alarm " + i + " sent with payload: " + coreAlarm.toJSON)
    coreAlarm
  }

  val videoSource: Source[ProducerRecord[String, String], NotUsed] = Source(1 to eventCount)
    .map { x =>
      getAlarm((x))
    }
    .map { e =>
      videoAlarmProducerTimer.time(e)
      new ProducerRecord[String, String](alarmTopic, UUID.randomUUID().toString, e.toJSON)
    }

  val coreSource: Source[ProducerRecord[String, String], NotUsed] = Source(1 to eventCount)
    .map(x => getCoreAlarm(x))
    .map { e =>
      coreAlarmProducerTimer.time(e)
      new ProducerRecord[String, String](coreAlarmTopic, UUID.randomUUID().toString, e.toJSON)
    }

  val throttle =
    Flow[ProducerRecord[String, String]].throttle(throttleLevel, 1.second, throttleLevel, ThrottleMode.Shaping)

  lazy val runnableGraphCommit: RunnableGraph[NotUsed] =
    RunnableGraph.fromGraph(g = GraphDSL.create() { implicit graphBuilder =>
      val videoNodeSource: Outlet[ProducerRecord[String, String]] = graphBuilder.add(videoSource).out

      val coreNodeSource: Outlet[ProducerRecord[String, String]] = graphBuilder.add(coreSource).out

      val coreThrottleFlow = graphBuilder.add(throttle)

      val videoThrottleFlow = graphBuilder.add(throttle)

      val coreSink: Inlet[ProducerRecord[String, String]] = graphBuilder.add(sink1).in

      val videoSink: Inlet[ProducerRecord[String, String]] = graphBuilder.add(sink2).in

      videoNodeSource ~> videoThrottleFlow ~> videoSink
      coreNodeSource ~> coreThrottleFlow ~> coreSink

      ClosedShape
    })

}
