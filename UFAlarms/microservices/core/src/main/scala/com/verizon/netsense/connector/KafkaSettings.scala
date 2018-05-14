package com.verizon.netsense.connector

import java.util.UUID

import akka.Done
import akka.actor.{ActorRef, ActorSystem}
import akka.kafka.scaladsl.Consumer.{Control, plainExternalSource}
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.kafka.{ConsumerSettings, KafkaConsumerActor, ProducerSettings, Subscriptions}
import akka.stream.scaladsl.{Sink, Source}
import org.apache.kafka.clients.consumer.{ConsumerConfig, ConsumerRecord}
import org.apache.kafka.clients.producer.{ProducerConfig, ProducerRecord}
import org.apache.kafka.common.TopicPartition
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer, StringDeserializer}

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.util.Random

/**
 * Created by maidapr on 4/24/17.
 */
trait KafkaSettings {

  def kafkaBootstrapServers: String
  def kafkaClientId: String
  def kafkaGroupId: String
  def kafkaOffsetReset: String

  def producerSettings()(implicit actorSystem: ActorSystem): ProducerSettings[Array[Byte], Array[Byte]] =
    ProducerSettings(system = actorSystem, new ByteArraySerializer, new ByteArraySerializer)
      .withBootstrapServers(kafkaBootstrapServers)
      .withProperty(ProducerConfig.RETRIES_CONFIG, "1000")
      .withParallelism(1000)

  def consumerSettings()(implicit actorSystem: ActorSystem): ConsumerSettings[Array[Byte], String] =
    ConsumerSettings(system = actorSystem,
                     keyDeserializer = new ByteArrayDeserializer,
                     valueDeserializer = new StringDeserializer)
      .withBootstrapServers(kafkaBootstrapServers)
      .withClientId(kafkaClientId + "_" + UUID.randomUUID())
      .withGroupId(kafkaGroupId)
      .withProperty(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, kafkaOffsetReset)
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

  def consumerSettings1()(implicit actorSystem: ActorSystem): ConsumerSettings[Array[Byte], Array[Byte]] =
    ConsumerSettings(system = actorSystem,
                     keyDeserializer = new ByteArrayDeserializer,
                     valueDeserializer = new ByteArrayDeserializer)
      .withBootstrapServers(kafkaBootstrapServers)
      .withClientId(kafkaClientId + "_" + UUID.randomUUID())
      .withGroupId(kafkaGroupId)
      .withProperty(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, kafkaOffsetReset)
      .withProperty(ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG, "true")
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

  def consumeTopics(
      topicName: Set[String]
  )(implicit actorSystem: ActorSystem): Source[ConsumerRecord[Array[Byte], String], Control] =
    Consumer.plainSource(consumerSettings, Subscriptions.topics(topicName))

  def consumeTopic(
      topicName: String
  )(implicit actorSystem: ActorSystem): Source[ConsumerRecord[Array[Byte], String], Control] =
    Consumer.plainSource(consumerSettings, Subscriptions.topics(topicName))

  def consumeTopic1(
      topicName: String
  )(implicit actorSystem: ActorSystem): Source[ConsumerRecord[Array[Byte], Array[Byte]], Control] =
    Consumer.plainSource(consumerSettings1, Subscriptions.topics(topicName))

  def consumeTopicByPartition(
      topicName: String,
      partition: Int
  )(implicit actorSystem: ActorSystem): Source[ConsumerRecord[Array[Byte], String], Control] = {
    val c: ActorRef =
      actorSystem.actorOf(KafkaConsumerActor.props(consumerSettings.withGroupId(kafkaGroupId + Random.nextInt())))
    plainExternalSource[Array[Byte], String](c, Subscriptions.assignment(new TopicPartition(topicName, partition)))
  }

  def source(
      topicName: String
  )(implicit actorSystem: ActorSystem): Source[ConsumerRecord[Array[Byte], String], Consumer.Control] =
    Consumer.plainSource(consumerSettings, Subscriptions.topics(topicName))

  def sink()(implicit actorSystem: ActorSystem): Sink[ProducerRecord[Array[Byte], Array[Byte]], Future[Done]] =
    Producer.plainSink(producerSettings)

}
