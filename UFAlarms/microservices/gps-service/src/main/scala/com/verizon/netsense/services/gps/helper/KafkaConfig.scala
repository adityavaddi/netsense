package com.verizon.netsense.services.gps.helper

import com.verizon.netsense.utils.{ConfigLoader, Logging}

object KafkaConfig extends Logging {

  lazy val kafkaConfig = ConfigLoader.config.getConfig("kafka")
  lazy val cassandraConfig = ConfigLoader.config.getConfig("cassandra")
  lazy val kafkaHost = kafkaConfig.getString("host")
  lazy val kafkaPort = kafkaConfig.getInt("port")
  var bootStrapServers = kafkaHost + ":" + kafkaPort
  require(!bootStrapServers.isEmpty, "BootStrapServers are missing")
  var groupId = kafkaConfig.getString("gps-group-name")
  var groupId2 = kafkaConfig.getString("gps-group2-name")
  var gpsTopic = kafkaConfig.getString("gps-request-topic")
  var gpsSchTopic = kafkaConfig.getString("gps-schEvent-topic")
  var gpsResTopic = kafkaConfig.getString("gps-response-topic")
  var isRequestTopic = kafkaConfig.getString("interface-service-request-topic")
  var isResponseTopic = kafkaConfig.getString("interface-service-response-topic")
  var gpsTable = cassandraConfig.getString("gpsTableName")
  log.info("Connecting to the Kafka bootStrapServers: " + bootStrapServers)
  log.info("Using gpsTopic: " + gpsTopic)
  log.info("Using gpsTopic: " + gpsResTopic)
  log.info("Using Interface Service Request Topic: " + isRequestTopic)
  log.info("Using Interface Service Response Topic: " + isResponseTopic)

}
