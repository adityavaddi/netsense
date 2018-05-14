package com.verizon.netsense.services.eventsimulator.simulator

import java.net.ConnectException
import java.util.{Calendar, UUID}

import akka.actor.ActorSystem
import akka.kafka.ProducerSettings
import akka.kafka.scaladsl.Producer
import akka.stream._
import akka.stream.scaladsl.GraphDSL.Implicits._
import akka.stream.scaladsl.{Flow, GraphDSL, RunnableGraph, Sink, Source}
import akka.{Done, NotUsed}
import com.datastax.driver.core.exceptions.NoHostAvailableException
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.services.eventsimulator.config.SimulatorConfig._
import com.verizon.netsense.services.eventsimulator.model._
import com.verizon.netsense.utils.Logging
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.StringSerializer

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.Random
import scala.util.control.NonFatal

object SensorIngestionSimulator extends Logging with Instrumented {

  lazy val systemDecider: Supervision.Decider = {
    case ex: ConnectException         => log.error("Unable to connect to Graphite " + ex.getMessage); Supervision.Resume
    case ex: NoHostAvailableException => log.error("Unable to connect to DB " + ex.getMessage); Supervision.Resume
    case NonFatal(ex)                 => log.error("Exception found in the Stream " + ex.getMessage); Supervision.Resume
    case ex: Exception                => log.error("Unhandled Exception seen " + ex.getMessage); Supervision.Resume;
  }

  implicit val sensorSimulatorActorSystem   = ActorSystem("Sensor-Simulator-system")
  implicit val ec: ExecutionContextExecutor = sensorSimulatorActorSystem.dispatcher
  implicit val sensormat = ActorMaterializer(
    ActorMaterializerSettings(sensorSimulatorActorSystem)
      .withInputBuffer(Math.pow(2, 10).toInt, Math.pow(2, 20).toInt)
      .withSupervisionStrategy(systemDecider)
  )

  val throttleLevel    = throttler //Events per second produced to kafka
  val eventCount       = recordCount //Alarm generation count
  val nodePrefix       = "falcon-q_1"
  val coreNodePrefix   = "unode-v4_1"
  val name             = "SensorSample"
  val sensor           = "p"
  val solicitationType = "UNSOL"
  val path             = "v1/falcon-q_1/out/UNSOL/sensor/{sensorid}"

  private[this] val coreSensorProducerTimer: Timer  = metrics.timer("kafka-core-sensor-producer-timer")
  private[this] val videoSensorProducerTimer: Timer = metrics.timer("kafka-video-sensor-producer-timer")

  val producerSettings = ProducerSettings(sensorSimulatorActorSystem, new StringSerializer, new StringSerializer)
    .withBootstrapServers(bootStrapServers)

  def sink1: Sink[ProducerRecord[String, String], Future[Done]] =
    Producer.plainSink(producerSettings)

  def sink2: Sink[ProducerRecord[String, String], Future[Done]] =
    Producer.plainSink(producerSettings)

  def getSensor(i: Long): SensorSample = {
    val sensorPayload =
      SensorPayload(nodePrefix, "units", sensor + i.toString, BigDecimal(i), System.currentTimeMillis() * 1000)
    val sensorEvent =
      SensorSample(UUID.randomUUID().toString,
                   nodePrefix + Random.nextInt(nodeCount).toString,
                   solicitationType,
                   sensor + i.toString,
                   path,
                   sensorPayload)
    log.debug("Sensor " + i + " sent with payload: " + sensorEvent.toJSON)
    sensorEvent
  }

  def getCoreSensor(i: Long): CoreNodeSensorSample = {
    val coreSensor = CoreNodeSensorSample(name,
                                          coreNodePrefix + Random.nextInt(nodeCount).toString,
                                          sensor + i.toString,
                                          System.currentTimeMillis() * 1000,
                                          "units",
                                          BigDecimal(i))
    log.debug("Sensor " + i + " sent with payload: " + coreSensor.toJSON)
    coreSensor
  }

  val videoSource: Source[ProducerRecord[String, String], NotUsed] = Source(1 to eventCount)
    .map { x =>
      getSensor((x))
    }
    .map { e =>
      videoSensorProducerTimer.time(e)
      new ProducerRecord[String, String](sensorTopic, UUID.randomUUID().toString, e.toJSON)
    }

  val coreSource: Source[ProducerRecord[String, String], NotUsed] = Source(1 to eventCount)
    .map(x => getCoreSensor(x))
    .map { e =>
      coreSensorProducerTimer.time(e)
      new ProducerRecord[String, String](coreSensorTopic, UUID.randomUUID().toString, e.toJSON)
    }

  val throttle =
    Flow[ProducerRecord[String, String]].throttle(throttleLevel, 1.second, throttleLevel, ThrottleMode.Shaping)

  lazy val runnableGraphCommit: RunnableGraph[NotUsed] =
    RunnableGraph.fromGraph(g = GraphDSL.create() { implicit graphBuilder =>
      val videoNodeSensorSource: Outlet[ProducerRecord[String, String]] = graphBuilder.add(videoSource).out

      val coreNodeSensorSource: Outlet[ProducerRecord[String, String]] = graphBuilder.add(coreSource).out

      val coreSensorThrottleFlow = graphBuilder.add(throttle)

      val videoSensorThrottleFlow = graphBuilder.add(throttle)

      val coreSensorSink: Inlet[ProducerRecord[String, String]] = graphBuilder.add(sink1).in

      val videoSensorSink: Inlet[ProducerRecord[String, String]] = graphBuilder.add(sink2).in

      videoNodeSensorSource ~> videoSensorThrottleFlow ~> videoSensorSink
      coreNodeSensorSource ~> coreSensorThrottleFlow ~> coreSensorSink

      ClosedShape
    })

}
