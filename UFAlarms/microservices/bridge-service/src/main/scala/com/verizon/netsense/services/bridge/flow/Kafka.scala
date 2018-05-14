package com.verizon.netsense.services.bridge.flow

import java.net.InetAddress

import akka.Done
import akka.actor.ActorSystem
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.kafka.{ConsumerSettings, ProducerSettings, Subscriptions}
import akka.stream.scaladsl.{Flow, Keep, RestartSink, RestartSource, Sink}
import akka.stream.{ActorAttributes, Supervision}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.services.bridge.flow.Common.attributes
import com.verizon.netsense.services.bridge.utils.ConfigLoader._
import com.verizon.netsense.utils.Logging
import nl.grons.metrics.scala.MetricName
import org.apache.kafka.clients.consumer.ConsumerConfig
import org.apache.kafka.clients.producer.{ProducerConfig, ProducerRecord}
import org.apache.kafka.common.KafkaException
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer}

import scala.concurrent.Future
import scala.concurrent.duration._
import nl.grons.metrics.scala

/**
 * Created by wswaney on 6/6/17.
 */
class Kafka(system: ActorSystem) extends /*App with*/ Instrumented with Logging {

  val localhost     = InetAddress.getLocalHost
  val metricsPrefix = localhost + ".bridge.kafka-flow."

  override lazy val metricBaseName = MetricName(metricsPrefix)

  private lazy val consumerSettings = ConsumerSettings(system, new ByteArrayDeserializer, new ByteArrayDeserializer)
    .withBootstrapServers(kafkaBootStrapServers)
    .withGroupId(fromKafka_consumer_group)
    .withProperty(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "latest")
    .withProperty(ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG, "true")
    .withProperty(ConsumerConfig.AUTO_COMMIT_INTERVAL_MS_CONFIG, "5000")
    .withProperty(ConsumerConfig.MAX_POLL_INTERVAL_MS_CONFIG, "30000")
    .withProperty(ConsumerConfig.MAX_POLL_RECORDS_CONFIG, "2000")
    .withProperty(ConsumerConfig.SESSION_TIMEOUT_MS_CONFIG, "10000")
    .withProperty(ConsumerConfig.HEARTBEAT_INTERVAL_MS_CONFIG, "3000")
    .withProperty(ConsumerConfig.METADATA_MAX_AGE_CONFIG, "60000")
    .withProperty(ConsumerConfig.RECONNECT_BACKOFF_MS_CONFIG, "10000")
    .withProperty(ConsumerConfig.PARTITION_ASSIGNMENT_STRATEGY_CONFIG,
      "org.apache.kafka.clients.consumer.RoundRobinAssignor")
    .withProperty(ConsumerConfig.CONNECTIONS_MAX_IDLE_MS_CONFIG, "604800000")
    .withWakeupTimeout(30 seconds)
    .withMaxWakeups(60000)

  lazy val producerSettings = ProducerSettings(system, new ByteArraySerializer, new ByteArraySerializer)
    .withBootstrapServers(kafkaBootStrapServers)
    .withProperty(ProducerConfig.RETRIES_CONFIG, "1000")
    .withParallelism(1000)


  private[this] lazy val kafkaSourceTime:scala.Timer = metrics.timer("fromKafka-timer")
  private[this] lazy val kafkaSinkTime:scala.Timer = metrics.timer("toKafka-timer")

  lazy val sourceDecider: Supervision.Decider = {
    case ex: NullPointerException => log.error("Null element in stream " + ex.getMessage); Supervision.Restart
    case ex: KafkaException =>
      log.error("KafkaException in Consumer: " + ex.getClass.getName + ": " + ex.getMessage); Supervision.Restart
    case ex =>
      log.error("Unhandled exception in stream source: " + ex.getClass.getName + ": " + ex.getMessage)
      Supervision.Restart
  }

  lazy val sinkDecider: Supervision.Decider = {
    case ex: NullPointerException => log.error("Null element in stream " + ex.getMessage); Supervision.Restart
    case ex: KafkaException =>
      log.error("KafkaException in Producer: " + ex.getClass.getName + ": " + ex.getMessage); Supervision.Restart
    case ex =>
      log.error("Unhandled exception in stream sink: " + ex.getClass.getName + ": " + ex.getMessage)
      Supervision.Restart
  }

  lazy val kafkaSource = Consumer
    .plainSource(consumerSettings, Subscriptions.topics(fromKafka_topic))
    .map { msg =>
      kafkaSourceTime.time{
      msg
    }}
    .withAttributes(ActorAttributes.supervisionStrategy(sourceDecider))

  var sourceRetryCount: Long = 0

  def incSourceRetryCounter(): Unit = sourceRetryCount = sourceRetryCount + 1

  lazy val kafkaRestartableConsumerSource = RestartSource.withBackoff(
    minBackoff = 1.seconds,
    maxBackoff = 5.seconds,
    randomFactor = 0.2
  ) { () =>
    if (sourceRetryCount == 0) {
      incSourceRetryCounter()
      log.info("Starting Kafka Consumer fromKafka: " + kafkaBootStrapServers)
    }
    else {
      {
        incSourceRetryCounter()
        log.error("Reconnecting kafka consumer on failure "
          + sourceRetryCount + " broker: " + kafkaBootStrapServers)
      }
    }
    kafkaSource
  }.withAttributes(attributes)

  lazy val kafkaSink: Sink[Array[Byte], Future[Done]] =
    Flow[Array[Byte]]
      .map { body =>
        new ProducerRecord[Array[Byte], Array[Byte]](toKafka_topic, body)
      }
      .toMat(Producer.plainSink(producerSettings))(Keep.right)
      .withAttributes(ActorAttributes.supervisionStrategy(sinkDecider))

  var sinkRetryCount: Long = 0

  def incSinkRetryCounter(): Unit = sinkRetryCount = sinkRetryCount + 1

  lazy val kafkaRestartableProducerSink = RestartSink.withBackoff(
    minBackoff = 1.seconds,
    maxBackoff = 5.seconds,
    randomFactor = 0.2
  ) { () =>
    if (sinkRetryCount == 0) {
      incSinkRetryCounter()
      log.info("Starting Kafka producer ToKafka: " + kafkaBootStrapServers)
    }
    else {
      {
        incSinkRetryCounter()
        log.error("Reconnecting kafka producer on ToKafka "
          + sinkRetryCount + " broker: " + kafkaBootStrapServers)
      }
    }
    kafkaSink
  }.withAttributes(attributes)

  lazy val kafkaRestartableSinkWithTopicName = RestartSink.withBackoff(
    minBackoff = 1.seconds,
    maxBackoff = 5.seconds,
    randomFactor = 0.2
  ) { () =>
    if (sinkRetryCount == 0) {
      incSinkRetryCounter()
      log.debug("Starting Kafka producer ToKafka: " + kafkaBootStrapServers)
    }
    else {
      {
        incSinkRetryCounter()
        log.error("Reconnecting kafka producer on ToKafka "
          + sinkRetryCount + " broker: " + kafkaBootStrapServers)
      }
    }
    kafkaSinkWithTopicName
  }.withAttributes(attributes)


  lazy val kafkaSinkWithTopicName: Sink[(String, String, Array[Byte]), Future[Done]] =
    Flow[(String,String, Array[Byte])]
      .map { e  =>
        kafkaSinkTime.time {
        val topic = e._1
        val key = e._2
        val msg = e._3
        log.debug(s"Kafka topic: ${topic}")
        log.debug(s"Kafka key  : ${key}")
        log.debug(s"Kafka msg  : ${msg}")
        new ProducerRecord[Array[Byte], Array[Byte]](topic,e._2.getBytes, msg)
      }}
      .toMat(Producer.plainSink(producerSettings))(Keep.right)
      .withAttributes(ActorAttributes.supervisionStrategy(sinkDecider))

}

object Kafka{
  def apply(implicit system: ActorSystem): Kafka = new Kafka(system)
}
