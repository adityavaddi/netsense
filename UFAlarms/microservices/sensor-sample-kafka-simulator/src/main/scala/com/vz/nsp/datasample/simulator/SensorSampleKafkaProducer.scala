package com.vz.nsp.datasample.simulator

import java.util.UUID

import akka.actor.ActorSystem
import akka.kafka._
import akka.kafka.scaladsl.Producer
import akka.stream.scaladsl.{Flow, Sink, Source, Tcp}
import akka.stream.{ActorMaterializer, ThrottleMode}
import akka.util.ByteString
import com.vz.nsp.datasample.simulator.metrics.Instrumented
import com.vz.nsp.datasample.simulator.model.{SensorPayload, SensorSampleEvent}
import com.vz.nsp.datasample.simulator.util.KafkaConfiguration.throttleLevel
import com.vz.nsp.datasample.simulator.util.{KafkaConfiguration, NodeSensorReader, ObjectMapperUtil}
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{ByteArraySerializer}

import scala.concurrent.duration._
import scala.util.{Failure, Random, Success}

/**
 * Created by maidapr on 4/20/17.
 */
object SensorSampleKafkaProducer extends App with Instrumented {

  implicit val system = ActorSystem("Kafka-Producer-Actor-System")
  implicit val mat    = ActorMaterializer()
  implicit val ec     = system.dispatcher

  private[this] val producerTimer: Timer = metrics.timer("kafka-producer-timer")

  lazy val eventType = List("SOL", "UNSOL")

  def generateRandomEvent = {
    val _nodeSensor = NodeSensorReader.randomElement
    val randomSensorPayload = SensorPayload(_nodeSensor.get.nodeid,
                                            "SensorSample",
                                            _nodeSensor.get.sensor,
                                            Random.nextInt((1000 - 0) + 1).toLong,
                                            System.currentTimeMillis())
    val randomSensorSampleEvent = SensorSampleEvent(
      i = UUID.randomUUID().toString,
      a = eventType(1),
      f = _nodeSensor.get.sensor,
      p = s"v1/${_nodeSensor.get.nodeid}/out/sensor/${_nodeSensor.get.sensor}",
      l = randomSensorPayload
    )
    (_nodeSensor.get.sensor, ObjectMapperUtil.toJson(randomSensorSampleEvent))
  }

  val handler = Sink.foreach[Tcp.IncomingConnection] { conn =>
    println("Client connected from: " + conn.remoteAddress)
    conn handleWith Flow[ByteString]
  }

  val bindHost = KafkaConfiguration.config.getString("http.host")
  val bindPort = KafkaConfiguration.config.getInt("http.port")

  val connections = Tcp().bind(bindHost, bindPort)
  val binding     = connections.to(handler).run()

  binding.onComplete {
    case Success(b) =>
      println("Server started, listening on: " + b.localAddress)
    case Failure(e) =>
      println(s"Server could not bind to $bindHost:$bindPort: ${e.getMessage}")
      system.terminate()
  }

  val producerSettings = ProducerSettings(system, new ByteArraySerializer, new ByteArraySerializer)
    .withBootstrapServers(KafkaConfiguration.kafkaHost + ":" + KafkaConfiguration.kafkaPort)

  val eventSimulator = Source
    .fromIterator(() => Iterator from 0)
    .map(_ => generateRandomEvent)
    .map { e =>
      producerTimer.time(e)
      new ProducerRecord[Array[Byte], Array[Byte]](KafkaConfiguration.kafkaTopic, e._1.getBytes(), e._2.getBytes())
    }
    .throttle(throttleLevel, 1000.milli, throttleLevel, ThrottleMode.Shaping)
    .runWith(Producer.plainSink(producerSettings))(mat)

}
