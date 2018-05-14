package com.verizon.netsense.graph

import java.net.InetAddress
import java.util.UUID

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.ActorAttributes
import akka.stream.scaladsl._
import com.verizon.netsense.config.KafkaConfig._
import com.verizon.netsense.connector.KafkaConnector
import com.verizon.netsense.helper.Common
import com.verizon.netsense.metrics.{Instrumented, JvmMetrics}
import com.verizon.netsense.util._
import nl.grons.metrics.scala.{MetricName, Timer}
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer}
import org.velvia.MsgPackUtils

import scala.concurrent.Future
import scala.concurrent.duration._

/** Provides Graph Stages common to all Traffic Event FLows.
  * Mixin Types: consumerSrcTopic, producerRespTopic, Actor System
  *
  */
trait TrafficGraphStages extends Common {

  val hostName     = InetAddress.getLocalHost
//  val metricsPrefix = hostName + ".traffic.service-flow."

  var consumerSrcTopic : String
  var producerRespTopic : String
  var _system : ActorSystem
  implicit val system = _system
  import scala.concurrent.ExecutionContext.Implicits.global

  var kSourceRetryCount: Long = 1

  lazy val kafkaConsumerSettings = KafkaConnector.configKafkaConsumerSettings(bootStrapServers,
    groupId, new ByteArrayDeserializer, new ByteArrayDeserializer)

  lazy val kafkaProducerSettings = KafkaConnector.configKafkaProducerSettings(bootStrapServers,
    _keySerializer = new ByteArraySerializer,
    _valueSerializer = new ByteArraySerializer)

  private[this] val kafkaSourceTimer: Timer  = metrics.timer("kafka-source-consumer")
  private[this] val unMarshallingFlowTimer: Timer    = metrics.timer("json-unmarshall-flow-timer")


  lazy val kafkaTrafficConsumerSource = configKafkaSource(kafkaConsumerSettings, Set(consumerSrcTopic)).map { e =>
    kafkaSourceTimer.time(e)
  }

  lazy val restartableKafkaTrafficConsumerSource: Source[ConsumerRecord[Array[Byte], Array[Byte]], NotUsed] =
    RestartSource.withBackoff(
      minBackoff = 1.seconds,
      maxBackoff = 5.seconds,
      randomFactor = 0.2
    ) { () =>
      if(kSourceRetryCount == 1) {
        kSourceRetryCount = kSourceRetryCount + 1
        log.info("Starting kafkaSourceRetryCount Consumer for Traffic Service " + bootStrapServers)
      }
      else {
        kSourceRetryCount = kSourceRetryCount + 1
        log.error("Restarting kafkaSourceRetryCount Consumer on failure " + kSourceRetryCount)
      }
      kafkaTrafficConsumerSource
    }

  lazy val trafficUnmarshallFlow = Flow[ConsumerRecord[Array[Byte], Array[Byte]]].map { x =>
    unMarshallingFlowTimer.time {
        log.debug(s"Traffic Event Received =  topic: ${x.topic()} , key: ${(x.key().map(_.toChar)).mkString} ,  msg: ${x.value()}")
        val msgKey = x.key().map(_.toChar).mkString
        val keyDetails = TrafficUtil.parseKeyForDetails(msgKey)
        val msg = x.value()
        log.info(s"x: ${MsgPackUtils.unpackMap(msg)}")
        val m = MsgPackUtils.unpackMap(msg)
        m.++(keyDetails)
    }
  }.async

  lazy val uuidDecodeFlow = Flow[Map[Any,Any]].map { x => decodeUUID(x) }.async

  lazy val updateMetadataKeys = Flow[Map[Any,Any]].map { x =>
    val y = TrafficUtil.updateTrafficMsgKeys(x)
    val j = ObjectMapperUtil.toJson(y)
    log.debug(s"JsonMap: ${j}")
    j
  }.async

  lazy val responseKafkaFlow = Flow[String]
    .mapAsync(parallelism) { x =>
        log.debug(s"response  ${x} sent to Kafka response topic ${producerRespTopic}")
        Future(new ProducerRecord[Array[Byte], Array[Byte]](producerRespTopic,  UUID.randomUUID().toString.getBytes, x.getBytes))
    }

  lazy val kafkaTrafficSink =
    responseKafkaFlow.toMat(configKafkaProducer(_producerSettings = kafkaProducerSettings))(Keep.right)

  var sinkRetryCount: Long = 0

  def incSinkRetryCounter(): Unit = sinkRetryCount = sinkRetryCount + 1

  lazy val restartableKafkaTrafficSink = RestartSink.withBackoff(
    minBackoff = 1.seconds,
    maxBackoff = 5.seconds,
    randomFactor = 0.2
  ) { () =>
    if (sinkRetryCount == 0) {
      incSinkRetryCounter()
      log.info("Starting Kafka producer Traffic: " + bootStrapServers)
    }
    else {
      {
        incSinkRetryCounter()
        log.error(s"Reconnecting kafka producer for: ${producerRespTopic} "
          + sinkRetryCount + " broker: " + bootStrapServers)
      }
    }
    kafkaTrafficSink
  }.withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  lazy val consoleSink = Sink.foreach[String] { x =>
    log.info(s"traffic Sink: ${x}")
  }


}
