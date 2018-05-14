package com.verizon.netsense.util

import com.typesafe.config.ConfigFactory
import com.verizon.netsense.config.ConfigLoader

/**
  * Created by nalamte on 2/26/18.
  */
object TestKafkaConfig{

  lazy val configLoader = ConfigFactory.load()
  lazy val kafkaConfig = configLoader.getConfig("embedded-kafka")
  lazy val kafkaHost   = kafkaConfig.getString("kafka-host")
  lazy val kafkaPort   = kafkaConfig.getInt("kafka-port")
  var bootStrapServers = kafkaHost + ":" + kafkaPort
  require(!bootStrapServers.isEmpty, "BootStrapServers are missing")
  var groupId              = kafkaConfig.getString("group-id")
  var videoNodeSensorSampleTopic    = kafkaConfig.getString("videonode-sensor-sample-topic")
  var coreNodeSensorSampleTopic    = kafkaConfig.getString("corenode-sensor-sample-topic")
  var requestTopic         = kafkaConfig.getString("request-topic")
  var responseTopic        = kafkaConfig.getString("response-topic")
  var sensorCommandCoreNodeTopic   = kafkaConfig.getString("to-core-node-topic")
  var sensorCommandVideoNodeTopic   = kafkaConfig.getString("to-device-topic")

  /**
    * Parameterizing the Kafka Configuration for Testing
    */
  def setupEmbeddedKafkaConfig(_bootStrapServer: String = bootStrapServers,
                               _groupId: String = groupId,
                               _videoNodeTopicName: String = videoNodeSensorSampleTopic,
                               _coreNodeTopicName: String = coreNodeSensorSampleTopic,
                               _requestTopicName: String = requestTopic,
                               _responseTopicName: String = responseTopic,
                               _sensorCommandCoreNodeTopic: String = sensorCommandCoreNodeTopic,
                               _sensorCommandVideoNodeTopic: String = sensorCommandVideoNodeTopic): Unit = {
    this.bootStrapServers = _bootStrapServer
    this.groupId = _groupId
    this.videoNodeSensorSampleTopic = _videoNodeTopicName
    this.coreNodeSensorSampleTopic = _coreNodeTopicName
    this.requestTopic = _requestTopicName
    this.responseTopic = _responseTopicName
    this.sensorCommandCoreNodeTopic = _sensorCommandCoreNodeTopic
    this.sensorCommandVideoNodeTopic = _sensorCommandVideoNodeTopic

  }

}
