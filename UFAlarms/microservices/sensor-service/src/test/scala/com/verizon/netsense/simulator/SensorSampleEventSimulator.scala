package com.verizon.netsense.simulator

import java.io.{InputStream, InputStreamReader}
import java.util.UUID

import akka.actor.ActorSystem
import akka.kafka.ProducerSettings
import akka.kafka.scaladsl.Producer
import akka.stream.scaladsl.Source
import akka.stream.{ActorMaterializer, ThrottleMode}
import com.github.tototoshi.csv.CSVReader
import com.typesafe.config.ConfigFactory
import com.verizon.netsense.constants.SensorSampleConstants
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.model.{SensorPayload, SensorSampleEvent}
import com.verizon.netsense.utils.{Logging, ObjectMapperUtil}
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.ByteArraySerializer

import scala.concurrent.duration._
import scala.util.Random

/**
  * Created by maidapr on 12/14/17.
  */
object SensorSampleEventSimulator extends App with Instrumented with Logging {

  implicit val system = ActorSystem("Kafka-Producer-Actor-System")
  implicit val mat = ActorMaterializer()
  implicit val ec = system.dispatcher

  lazy val config = ConfigFactory.load()
  // Graphite Configuration
  lazy val graphiteConfig = config.getConfig("graphite")

  lazy val graphiteHost = graphiteConfig.getString("host")
  lazy val graphitePort = graphiteConfig.getInt("port")
  // Kafka Configuration
  lazy val kafkaConfig = config.getConfig("kafka")

  lazy val kafkaHost = kafkaConfig.getString("host")
  lazy val kafkaPort = kafkaConfig.getInt("port")
  lazy val kafkaTopic = kafkaConfig.getString("videonode-sensor-sample-topic")

  lazy val throttleLevel: Int = config.getInt("simulator.event.throttle")

  lazy val maxSimulatedCount = config.getString("simulator.event.maxlimit") match {
    case "none" => Long.MaxValue
    case x => x.toLong
  }

  log.info("Connecting to kafka: " + kafkaHost + ":" + kafkaPort)
  log.info("Using Kafka topic for simulation:" + kafkaTopic)


  private[this] val producerTimer: Timer = metrics.timer("kafka-producer-timer")

  lazy val eventType = List("SOL", "UNSOL")

  case class NodeSensor(nodeid: String, sensor: String)

  private val inputStream: InputStream = this.getClass.getClassLoader.getResourceAsStream("nodesensors.csv")
  private val inputStreamReader: InputStreamReader = new InputStreamReader(inputStream)

  implicit val reader = CSVReader.open(inputStreamReader)
  implicit val elementList = reader.all().head

  val regex = "NodeSensor\\((.*),(.*)\\)".r

  object NodeSensorString {
    def unapply(str: String): Option[NodeSensor] = str match {
      case regex(n, s) => Some(NodeSensor(n, s))
      case _ => None
    }
  }

  def generateRandomId = UUID.randomUUID().toString

  def randomNodeSensor = NodeSensorString.unapply(elementList(Random.nextInt(999)))

  lazy val jtValue       = BigDecimal(4684316664362221708L)
  lazy val bRValue       = 264
  lazy val rfValue       = java.lang.Long.parseUnsignedLong("18446744073703063472")

  def randomValueFromRange(midValue: BigDecimal): BigDecimal = midValue.+(Random.nextInt(10))

  def generateNearRealNodeValues(sensorType: String, _value: Long) = sensorType match {
    case SensorSampleConstants.jt   => randomValueFromRange(jtValue)
    case SensorSampleConstants.bR   => randomValueFromRange(bRValue)
    case SensorSampleConstants.rf   => randomValueFromRange(rfValue)
    case SensorSampleConstants.lt   => BigDecimal(Random.nextInt(101))
    case "p"    => BigDecimal(Random.nextInt(2))
    case "v"    => BigDecimal(Random.nextInt(240))
    case "w"    => BigDecimal(Random.nextInt(40))
    case "t"    => BigDecimal(Random.nextInt(100))
    case "pf"    => BigDecimal(Random.nextInt(40))
    case "vp"    => BigDecimal(Random.nextInt(240))
    case "lIR"  => BigDecimal(Random.nextInt(40))
    case "degC" => BigDecimal(Random.nextInt(35))
    case _      => BigDecimal(_value)
  }

  def generateRandomEvent = {
    val _nodeSensor = randomNodeSensor
    val randomSensorPayload = SensorPayload(_nodeSensor.get.nodeid, "",
      _nodeSensor.get.sensor,
      generateNearRealNodeValues(_nodeSensor.get.sensor, Random.nextInt((1000 - 0) + 1).toLong),
      System.currentTimeMillis())
    val randomSensorSampleEvent = SensorSampleEvent(
      uuid = UUID.randomUUID().toString,
      sid = _nodeSensor.get.nodeid,
      a = eventType(1),
      f = _nodeSensor.get.sensor,
      p = s"v1/${_nodeSensor.get.nodeid}/out/UNSOL/sensor/${_nodeSensor.get.sensor}",
      l = randomSensorPayload)
    (randomSensorSampleEvent.uuid, ObjectMapperUtil.toJson(randomSensorSampleEvent))
  }

  lazy val producerSettings = ProducerSettings(system, new ByteArraySerializer, new ByteArraySerializer)
    .withBootstrapServers(kafkaHost + ":" + kafkaPort)

  val eventSimulator = Source
    .fromIterator(() => Iterator from 0)
    .limit(maxSimulatedCount)
    .map(_ => generateRandomEvent)
      .map { e =>
      log.debug("Publishing simulated event " + e._2)
      producerTimer.time(e)
      new ProducerRecord[Array[Byte], Array[Byte]](kafkaTopic, e._1.getBytes(), e._2.getBytes())
    }
    .throttle(throttleLevel, 1.second, throttleLevel, ThrottleMode.Shaping)
    .runWith(Producer.plainSink(producerSettings))(mat)

}
