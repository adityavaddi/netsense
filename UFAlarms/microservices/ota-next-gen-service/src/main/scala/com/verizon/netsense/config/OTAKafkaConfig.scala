package com.verizon.netsense.config

import com.typesafe.config.ConfigFactory

object OTAKafkaConfig {
  lazy val config = ConfigFactory.load()

  lazy val kafkaConfig = config.getConfig("kafka")

  lazy val kafkaHost = kafkaConfig.getString("host")
  lazy val kafkaPort = kafkaConfig.getInt("port")
  var bootStrapServers = kafkaHost + ":" + kafkaPort
  lazy val apiGroupId = kafkaConfig.getString("ota-api-group-id")
  lazy val jobGroupId = kafkaConfig.getString("ota-job-group-id")
  lazy val mqttGroupId = kafkaConfig.getString("ota-mqtt-group-id")
  lazy val deviceServiceGroupId = kafkaConfig.getString("ota-device-service-group-id")
  lazy val otaTopic = kafkaConfig.getString("ota-topic")
  lazy val mqttReqTopic = kafkaConfig.getString("mqtt-req-topic")
  lazy val mqttResTopic = kafkaConfig.getString("mqtt-res-topic")
  lazy val apiReqTopic = kafkaConfig.getString("api-req-topic")
  lazy val apiResTopic = kafkaConfig.getString("api-res-topic")
  lazy val deviceServiceReqTopic = kafkaConfig.getString("device-service-req-topic")
  lazy val deviceServiceResTopic = kafkaConfig.getString("device-service-res-topic")
}
