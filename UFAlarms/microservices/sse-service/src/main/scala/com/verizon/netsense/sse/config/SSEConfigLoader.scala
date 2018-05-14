package com.verizon.netsense.sse.config

import com.typesafe.config.ConfigFactory
import com.verizon.netsense.connector.CassandraConnector
import com.verizon.netsense.config.ConfigLoader.{envSuffix => environmentSuffix}

/**
 * Created by maidapr on 4/7/17.
 */
object SSEConfigLoader {

  lazy val config = ConfigFactory.load()

  /*
   * Graphite Config
   */
  lazy val graphiteConfig = config.getConfig("graphite")
  lazy val grapHostParam  = graphiteConfig.getString("host")
  lazy val grapPortParam  = graphiteConfig.getInt("port")

  /*
   * Kafka Config
   */
  lazy val kafka             = config.getConfig("kafka")
  lazy val kafkaHost         = kafka.getString("host")
  lazy val kafkaPort         = kafka.getInt("port")
  lazy val kafkaServersParam = kafkaHost + ":" + kafkaPort
  require(!kafkaServersParam.isEmpty, "BootStrapServers are missing")

  lazy val kafkaGroupIdParam  = kafka.getString("sse-group-id")
  lazy val kafkaClientIdParam = kafka.getString("client-id")

  lazy val envSuffix = environmentSuffix

  lazy val kafkaTopicNamesParam = kafka.getString("topic-names").split(",").toSet
  require(kafkaTopicNamesParam.nonEmpty, "SSE Kafka Sources shouldn't be empty")

  lazy val businessAlertkafkaTopicName = kafka.getString("businessalert-topic-name").split(",").toSet
  require(businessAlertkafkaTopicName.nonEmpty, "SSE Kafka Sources shouldn't be empty")

  lazy val kafkaOffsetResetParam     = kafka.getString("offset-reset")
  lazy val kafkaStreamingTopicPrefix = kafka.getString("streaming_topic_prefix")
  lazy val businessAlertTopicSuffix  = kafka.getString("businessalert_topic_suffix")

  lazy val sseFilterTopicParam         = kafka.getString("sse-filter-topic-name")
  lazy val sseBusinessFilterTopicParam = kafka.getString("sse-businessfilter-topic-name")
  lazy val parallellismParam           = kafka.getInt("parallellism")

  /*
   * Cassandra Config
   */
  lazy val cassandraConfig       = config.getConfig("cassandra")
  lazy val sseCassandraConnector = CassandraConnector.connector

  lazy val sseLookupTable               = cassandraConfig.getString("sse_lookup_table")
  lazy val sseStreamSubscribeTtlTable   = cassandraConfig.getString("sse_stream_subscribe_ttl_table")
  lazy val sseStreamBusinnesalertsTable = cassandraConfig.getString("sse_stream_businnessalerts_table")
  lazy val sseStreamTtl                 = cassandraConfig.getLong("sse_stream_subscribe_ttl")

  /*
   * Mqtt Config
   */
  lazy val mqtt                      = config.getConfig("mqtt")
  lazy val mqttProtocolParam         = mqtt.getString("protocol")
  lazy val mqttPortParam             = mqtt.getInt("mqtt-port")
  lazy val mqttUserNameParam         = mqtt.getString("username")
  lazy val mqttPasswordParam         = mqtt.getString("password")
  lazy val mqttClientIdParam         = mqtt.getString("clientId")
  lazy val mqttBusinessClientIdParam = mqtt.getString("businessclientId")
  lazy val mqttSourceBufferSizeParam = mqtt.getInt("buffer-size")
  lazy val mqtthostParam             = mqtt.getString("host")
}
