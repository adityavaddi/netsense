package com.vz.nsp.trigger.config

import com.typesafe.config.ConfigFactory
import com.verizon.netsense.config.ConfigLoader.{envSuffix => environmentSuffix}
import com.verizon.netsense.connector.CassandraConnector

/**
 * Created by maidapr on 4/7/17.
 */
object ConfigLoader {

  lazy val config = ConfigFactory.load()

  lazy val graphiteConfig = config.getConfig("graphite")
  lazy val grapHostParam  = graphiteConfig.getString("host")
  lazy val grapPortParam  = graphiteConfig.getInt("port")

  lazy val kafka                             = config.getConfig("kafka")
  lazy val kafkaHost                         = kafka.getString("host")
  lazy val kafkaPort                         = kafka.getInt("port")
  lazy val kafkaServersParam                 = kafkaHost + ":" + kafkaPort
  require(!kafkaServersParam.isEmpty, "BootStrapServers are missing")

  lazy val kafkaGroupIdParam                 = kafka.getString("trigger-group-id")
  lazy val kafkaClientIdParam                = kafka.getString("client-id")

  lazy val envSuffix                         = environmentSuffix

  lazy val kafkaTopicNamesParam: Set[String] = kafka.getString("topic-names").split(",").map(x => s"$x$envSuffix").toSet
  lazy val kafkaOffsetResetParam             = kafka.getString("offset-reset")
  lazy val kafkaStreamingTopicPrefix         = kafka.getString("streaming_topic_prefix")

  lazy val sseFilterTopicParam = kafka.getString("trigger-filter-topic-name")
  lazy val parallellismParam   = kafka.getInt("parallellism")

  //Cassandra Config
  lazy val cassandraConfig            = config.getConfig("cassandra")
  lazy val sseCassandraConnector      = CassandraConnector.connector

 // lazy val sseLookupTable             = cassandraConfig.getString("trigger_lookup_table")
  //lazy val sseStreamSubscribeTtlTable = cassandraConfig.getString("trigger_stream_subscribe_ttl_table")
  //lazy val sseStreamTtl               = cassandraConfig.getLong("trigger_stream_subscribe_ttl")

  lazy val mqtt                      = config.getConfig("mqtt")
  lazy val mqttProtocolParam         = mqtt.getString("protocol")
  lazy val mqttPortParam             = mqtt.getInt("mqtt-port")
  lazy val mqttUserNameParam         = mqtt.getString("username")
  lazy val mqttPasswordParam         = mqtt.getString("password")
  lazy val mqttClientIdParam         = mqtt.getString("clientId")
  lazy val mqttSourceBufferSizeParam = mqtt.getInt("buffer-size")
  lazy val mqtthostParam             = mqtt.getString("host")
}
