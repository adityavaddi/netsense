package com.verizon.netsense.businessalertservice.flow

import java.util.concurrent.atomic.AtomicLong

import akka.NotUsed
import akka.actor.ActorSystem
import akka.kafka.scaladsl.Producer
import akka.kafka.{ConsumerSettings, ProducerSettings}
import akka.stream.ActorAttributes
import akka.stream.scaladsl.{Flow, Keep, RestartSink, RestartSource, Source}
import com.verizon.netsense.businessalertservice.config.KafkaConfig
import com.verizon.netsense.businessalertservice.config.KafkaConfig.{bootStrapServers, groupId, requestTopicName}
import com.verizon.netsense.businessalertservice.model.AppRequest
import com.verizon.netsense.businessalertservice.util.AkkaStreamUtil.{configKafkaSource, queryFlowDecider}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.consumer.{ConsumerConfig, ConsumerRecord}
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

class KafkaConnector extends Instrumented with Logging {
  private[this] val businessAlertKafkaSourceTimer: Timer = metrics.timer("business-alert-kafka-source-consumer")
  private[this] val businessAlertKafkaSinkTimer: Timer   = metrics.timer("business-alert-kafka-sink-producer")
  implicit val system1                                   = ActorSystem.create("BusinessAlertSystemKafka")
  implicit val ec                                        = ExecutionContext.Implicits.global

  def configKafkaProducerSettings[K, V](
      _bootStrapServers: String = KafkaConfig.bootStrapServers,
      _keySerializer: Serializer[K],
      _valueSerializer: Serializer[V]
  )(implicit _system: ActorSystem): ProducerSettings[K, V] =
    ProducerSettings(system = _system, _keySerializer, _valueSerializer)
      .withBootstrapServers(_bootStrapServers)
      .withParallelism(10000)
      .withCloseTimeout(60 seconds)

  def configKafkaConsumerSettings[K, V](
      _bootstrapServer: String = KafkaConfig.bootStrapServers,
      _groupId: String,
      _keySerializer: Deserializer[K],
      _valueSerializer: Deserializer[V]
  )(implicit _system: ActorSystem): ConsumerSettings[K, V] =
    ConsumerSettings(system = _system, _keySerializer, _valueSerializer)
      .withBootstrapServers(_bootstrapServer)
      .withGroupId(_groupId)
      .withProperty(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest")
      .withProperty(ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG, "true")
      .withProperty(ConsumerConfig.AUTO_COMMIT_INTERVAL_MS_CONFIG, "5000")
      .withProperty(ConsumerConfig.MAX_POLL_INTERVAL_MS_CONFIG, "30000")
      .withProperty(ConsumerConfig.MAX_POLL_RECORDS_CONFIG, "2000")
      .withProperty(ConsumerConfig.SESSION_TIMEOUT_MS_CONFIG, "10000")
      .withProperty(ConsumerConfig.HEARTBEAT_INTERVAL_MS_CONFIG, "3000")
      .withProperty(ConsumerConfig.METADATA_MAX_AGE_CONFIG, "60000")
      .withProperty(ConsumerConfig.PARTITION_ASSIGNMENT_STRATEGY_CONFIG,
        "org.apache.kafka.clients.consumer.RoundRobinAssignor")
      .withProperty(ConsumerConfig.RECONNECT_BACKOFF_MS_CONFIG, "10000")
      .withProperty(ConsumerConfig.CONNECTIONS_MAX_IDLE_MS_CONFIG, "604800000")
      .withWakeupTimeout(30 seconds)
      .withMaxWakeups(60000)

  lazy val setupConsumerSettings =
    configKafkaConsumerSettings(bootStrapServers, groupId, new StringDeserializer, new StringDeserializer)

  lazy val kafkaConsumerSource = configKafkaSource(setupConsumerSettings, Set(requestTopicName)).map { e =>
    businessAlertKafkaSourceTimer.time(e)
  }

  var sourceRetryCount = new AtomicLong(0L)

  import scala.concurrent.duration._

  lazy val kafkaRestartableConsumerSource: Source[ConsumerRecord[String, String], NotUsed] = RestartSource.withBackoff(
    minBackoff = 1.seconds,
    maxBackoff = 2.seconds,
    randomFactor = 0.2
  ) { () =>
    if (sourceRetryCount.get == 0) {
      log.info(
        s"##### Starting Consumer for Business Alert Service with sourceRetryCount ${sourceRetryCount.getAndIncrement}"
      )
    } else {
      log.error("Restarting Consumer on failure " + sourceRetryCount.getAndIncrement)
    }
    kafkaConsumerSource
  }

  lazy val setupProducerSettings = configKafkaProducerSettings(bootStrapServers,
                                                               _keySerializer = new ByteArraySerializer,
                                                               _valueSerializer = new ByteArraySerializer)
  var sinkRetryCount: Long = 0

  def incSinkRetryCounter(): Unit = sinkRetryCount = sinkRetryCount + 1

  lazy val restartableProducerSinkBusinessAlert = RestartSink
    .withBackoff(
      minBackoff = 1.seconds,
      maxBackoff = 5.seconds,
      randomFactor = 0.2
    ) { () =>
      if (sinkRetryCount == 0) {
        incSinkRetryCounter()
        log.info("Starting Kafka producer BusinessAlertService: " + bootStrapServers)
      } else {
        {
          incSinkRetryCounter()
          log.error(
            "Reconnecting kafka producer on BusinessAlertService "
            + sinkRetryCount + " broker: " + bootStrapServers
          )
        }
      }
      responseKafkaSink
    }
    .withAttributes(ActorAttributes.supervisionStrategy(queryFlowDecider))

  val responseKafkaSink = Flow[(AppRequest, String)]
    .mapAsync(1000) { x =>
      businessAlertKafkaSinkTimer.time {
        Future(new ProducerRecord[Array[Byte], Array[Byte]](x._1.responsetopic, x._1.messageid.getBytes, x._2.getBytes))
      }
    }
    .toMat(Producer.plainSink(setupProducerSettings))(Keep.right)

}
object KafkaConnector {
  def apply(implicit system: ActorSystem): KafkaConnector = new KafkaConnector()
}
