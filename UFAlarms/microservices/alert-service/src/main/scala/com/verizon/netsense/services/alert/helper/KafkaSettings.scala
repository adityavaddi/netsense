package com.verizon.netsense.services.alert.helper

import akka.actor.ActorSystem
import akka.kafka.scaladsl.Consumer.Control
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.kafka.{ConsumerSettings, ProducerSettings, Subscriptions}
import akka.stream.scaladsl.{RestartSource, Sink, Source}
import akka.{Done, NotUsed}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.services.alert.helper.KafkaConfig.{bootStrapServers, groupId}
import com.verizon.netsense.utils.Logging
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer}

import scala.concurrent.Future
import scala.concurrent.duration._

trait KafkaSettings extends Instrumented with Logging {

  private[this] val kafkaSourceTimer: Timer = metrics.timer("kafka-source-consumer")

  def configKafkaSource[K, V](_consumerSettings: ConsumerSettings[K, V],
                              _topics: Set[String]): Source[ConsumerRecord[K, V], Control] =
    Consumer.plainSource(_consumerSettings, Subscriptions.topics(_topics))

  def configKafkaProducer[K, V](_producerSettings: ProducerSettings[K, V]): Sink[ProducerRecord[K, V], Future[Done]] =
    Producer.plainSink(_producerSettings)

  def setupConsumerSettings(implicit _system: ActorSystem): ConsumerSettings[Array[Byte], Array[Byte]] =
    KafkaConnector.configKafkaConsumerSettings(bootStrapServers,
                                               groupId,
                                               new ByteArrayDeserializer,
                                               new ByteArrayDeserializer)

  def setupProducerSettings(implicit _system: ActorSystem): ProducerSettings[Array[Byte], Array[Byte]] =
    KafkaConnector.configKafkaProducerSettings(bootStrapServers,
                                               _keySerializer = new ByteArraySerializer,
                                               _valueSerializer = new ByteArraySerializer)

  def kafkaConsumerSource(
      topics: Set[String]
  )(implicit _system: ActorSystem): Source[ConsumerRecord[Array[Byte], Array[Byte]], Control] =
    configKafkaSource(setupConsumerSettings, topics).map { e =>
      kafkaSourceTimer.time(e)
    }

  def kafkaRestartableConsumerSource(
      topicNames: Set[String]
  )(implicit system: ActorSystem): Source[ConsumerRecord[Array[Byte], Array[Byte]], NotUsed] =
    RestartSource.withBackoff(
      minBackoff = 1.seconds,
      maxBackoff = 5.seconds,
      randomFactor = 0.2
    ) { () =>
      log.info("Starting Kafka Consumer with broker(s): " + bootStrapServers + "for topic(s): " + topicNames)
      kafkaConsumerSource(topicNames)
    }

}
