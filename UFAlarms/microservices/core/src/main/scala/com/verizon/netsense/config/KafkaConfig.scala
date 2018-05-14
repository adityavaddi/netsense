package com.verizon.netsense.config

import com.verizon.netsense.utils.Logging

/**
 * Created by maidapr on 6/1/17.
 */
object KafkaConfig extends Logging {

  lazy val kafkaConfig = ConfigLoader.config.getConfig("kafka")
  lazy val kafkaHost   = kafkaConfig.getString("host")
  lazy val kafkaPort   = kafkaConfig.getInt("port")
  lazy val messageSizeCutoff = kafkaConfig.getInt("message-size-cutoff")
  var bootStrapServers = kafkaHost + ":" + kafkaPort
  require(!bootStrapServers.isEmpty, "BootStrapServers are missing")
  var groupId              = kafkaConfig.getString("group-id")
  var videoNodeSensorSampleTopic    = kafkaConfig.getString("videonode-sensor-sample-topic")
  var coreNodeSensorSampleTopic    = kafkaConfig.getString("corenode-sensor-sample-topic")
  var coreNodeAlarmTopic    = kafkaConfig.getString("corenode-alarm-sample-topic")
  var requestTopic         = kafkaConfig.getString("request-topic")
  var responseTopic        = kafkaConfig.getString("response-topic")
  var sensorCommandCoreNodeTopic   = kafkaConfig.getString("to-core-node-topic")
  var sensorCommandVideoNodeTopic   = kafkaConfig.getString("to-device-topic")
  lazy val parsedUpstreamTopicSet = kafkaConfig.getString("sensor-upstream-topics").split(",").toSet
  lazy val sensorTopicList = kafkaConfig.getString("sensor-upstream-topics").split(",").toList
  log.info("Connecting to the Kafka bootStrapServers: " + bootStrapServers)
  log.info("Using video node sensor sample topic: " + videoNodeSensorSampleTopic)
  log.info("Using core node sensor sample topic: " + coreNodeSensorSampleTopic)
  log.info("Using requestTopic: " + requestTopic)
  log.info("Using sensorCommandCoreNodeTopic: " + sensorCommandCoreNodeTopic)
  log.info("Using sensorCommandVideoNodeTopic: " + sensorCommandVideoNodeTopic)
  log.info("Using sensorCoreNodeAlarmTopc: " + coreNodeAlarmTopic)

}
