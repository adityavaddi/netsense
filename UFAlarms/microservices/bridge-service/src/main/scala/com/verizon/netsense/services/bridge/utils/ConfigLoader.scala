package com.verizon.netsense.services.bridge.utils

import com.typesafe.config.ConfigFactory

import scala.collection.JavaConverters._

object ConfigLoader {

  lazy val config = ConfigFactory.load()

  lazy val rabbitConfig = config.getConfig("amqp")
  lazy val addresses    = rabbitConfig.getList("addresses")

  // Graphite Configuration
  lazy val graphiteConfig = config.getConfig("graphite")
  lazy val graphiteHost   = graphiteConfig.getString("host")
  lazy val graphitePort   = graphiteConfig.getInt("port")

  lazy val bridgeConfig     = config.getConfig("bridge")
  lazy val bridgeType       = bridgeConfig.getString("bridge.type")
  lazy val log_info_rate    = bridgeConfig.getInt("log.info.rate")
  lazy val kafkaToMqttMsgExpiryTimeInSec = bridgeConfig.getInt("tomqtt.msg.expirytime.insec")

  //MQTT to Kafka Bridge Configuration
  lazy val mqttToKafkaTopicMappings = bridgeConfig.getConfigList("fromMqttToKafka.topic-mappings").asScala.seq.map(m => {
    (m.getString("kafka.topic"),m.getString("mqtt.topic"))
  }).toList

  //RabbitMQ to Kafka Bridge Configuration
   lazy val rabbitMQToKafkaTopicMappings: Map[String, String] = bridgeConfig.getConfigList("fromRabbit.topic-mappings").asScala.seq.map(m => {
    (m.getString("rabbitmq.msg.type"), m.getString("kafka.topic"))
  }).toMap


  lazy val from_mapping_key = bridgeConfig.getString("from.mapping.key")
  lazy val to_mapping_key   = bridgeConfig.getString("to.mapping.key")

  lazy val bridgeToKafkaConfig   = bridgeConfig.getConfig("toKafka")
  lazy val bridgeFromKafkaConfig = bridgeConfig.getConfig("fromKafka")

  lazy val bridgeFromRabbit = bridgeConfig.getConfig("fromRabbit")
  lazy val bridgeToRabbit   = bridgeConfig.getConfig("toRabbit")

  lazy val bridgeFromMQTT = bridgeConfig.getConfig("fromMQTT")
  lazy val bridgeToMQTT   = bridgeConfig.getConfig("toMQTT")

  lazy val toKafka_topic = bridgeToKafkaConfig.getString("kafka.topic")

  lazy val fromRabbit_exchange = bridgeFromRabbit.getString("rabbitmq.exchange")
  lazy val fromRabbit_routing  = bridgeFromRabbit.getString("rabbitmq.routing").split(",").toSet
  lazy val fromRabbit_queue    = bridgeFromRabbit.getString("rabbitmq.queue")
  lazy val fromRabbitAsyncParallelism    = bridgeFromRabbit.getInt("async.parallelism")
  lazy val fromRabbitPrefetchCount    = bridgeFromRabbit.getInt("prefetch.count")

  lazy val kafkaConfig = ConfigLoader.config.getConfig("kafka")
  lazy val kafkaHost  = kafkaConfig.getString("host")
  lazy val kafkaPort  = kafkaConfig.getString("port")
  lazy val kafkaBootStrapServers  = kafkaHost + ":" + kafkaPort
  require(!kafkaBootStrapServers.isEmpty, "BootStrapServers are missing")

  lazy val fromKafka_topic          = bridgeFromKafkaConfig.getString("kafka.topic")
  lazy val fromKafka_consumer_group = bridgeFromKafkaConfig.getString("kafka.consumer.group")

  lazy val toRabbit_exchange = bridgeToRabbit.getString("rabbitmq.exchange")
  lazy val toRabbit_routing  = bridgeToRabbit.getString("rabbitmq.routing")

  lazy val mqttConfig       = config.getConfig("mqtt")
  lazy val mqttBroker       = mqttConfig.getString("broker")
  lazy val mqttClientId     = mqttConfig.getString("clientId")
  lazy val mqttAuthUser     = mqttConfig.getString("auth_user")
  lazy val mqttAuthPW       = mqttConfig.getString("auth_pw")
  lazy val mqttCleanSession = mqttConfig.getBoolean("cleanSession")
  lazy val mqttFromTopic    = mqttConfig.getString("fromTopic")
  lazy val mqttToTopic      = mqttConfig.getString("toTopic")
  lazy val mqttBufferSize   = mqttConfig.getInt("buffer_size")

  lazy val healthCheckConfig = config.getConfig("health-check")
  lazy val healthCheckHost   = healthCheckConfig.getString("host")
  lazy val healthCheckPort   = healthCheckConfig.getInt("port")
}
