package com.verizon.netsense.connector

import akka.actor.ActorSystem
import akka.kafka.{ConsumerSettings, ProducerSettings}
import com.verizon.netsense.utils.ConfigLoader
import org.apache.kafka.clients.consumer.ConsumerConfig
import org.apache.kafka.clients.producer.ProducerConfig
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer, Deserializer, Serializer}

import scala.concurrent.duration._

class KafkaConnection(var system: ActorSystem) {

  //implicit val actorSystem = ActorSystem()
  val kafkaHost = ConfigLoader.config.getString("kafka.host")
  val kafkaPort        = ConfigLoader.config.getInt("kafka.port")
  val bootStrapServers = kafkaHost + ":" + kafkaPort
  var groupId          = ConfigLoader.config.getString("kafka.group-name")

  lazy implicit val producerSettings =
    ProducerSettings(system = system, new ByteArraySerializer, new ByteArraySerializer)
      .withBootstrapServers(bootStrapServers)
      .withProperty(ProducerConfig.RETRIES_CONFIG, "1000")
      .withParallelism(1000)

  lazy implicit val consumerSettings = ConsumerSettings(system = system,
    keyDeserializer = new ByteArrayDeserializer,
    valueDeserializer = new ByteArrayDeserializer)
    .withBootstrapServers(bootStrapServers)
    .withGroupId(groupId)
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

  def configKafkaProducerSettings[K, V](
                                         _bootStrapServers: String = bootStrapServers,
                                         _keySerializer: Serializer[K],
                                         _valueSerializer: Serializer[V]
                                       )(implicit _system: ActorSystem): ProducerSettings[K, V] =
    ProducerSettings(system = _system, _keySerializer, _valueSerializer)
      .withBootstrapServers(_bootStrapServers)

  def configKafkaConsumerSettings[K, V](
                                         _bootstrapServer: String = bootStrapServers,
                                         _groupId: String,
                                         _keySerializer: Deserializer[K],
                                         _valueSerializer: Deserializer[V]
                                       )(implicit _system: ActorSystem): ConsumerSettings[K, V] =
    ConsumerSettings(system = _system, _keySerializer, _valueSerializer)
      .withBootstrapServers(_bootstrapServer)
      .withGroupId(_groupId)
      .withProperty(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest")
      .withProperty(ConsumerConfig.METADATA_MAX_AGE_CONFIG, "60000")

  def configExternalConsumerSettings(_groupId: String): Unit = {
    this.groupId = _groupId
  }

}