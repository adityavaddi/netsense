package com.verizon.netsense.config

import com.verizon.netsense.utils.Logging

object KafkaConfig extends Logging {

  lazy val kafkaConfig = ConfigLoader.config.getConfig("kafka")
  lazy val kafkaHost   = kafkaConfig.getString("host")
  lazy val kafkaPort   = kafkaConfig.getInt("port")
  var bootStrapServers = kafkaHost + ":" + kafkaPort
  require(!bootStrapServers.isEmpty, "BootStrapServers are missing")
  var groupId              = kafkaConfig.getString("group-id")
  var sicResponseTopic    = kafkaConfig.getString("media-info-topic")
  var sicCommandTopic   = kafkaConfig.getString("media-request-topic")
  var sicStatusTopic    = kafkaConfig.getString("sic-status-topic")
  var requestTopic         = kafkaConfig.getString("request-topic")
  var responseTopic        = kafkaConfig.getString("response-topic")
  log.info("Connecting to the Kafka bootStrapServers: " + bootStrapServers)
  log.info("Using IS requestTopic: " + requestTopic)
  log.info("Using IS responseTopic: " + responseTopic)
  log.info("Using SIC RequestTopic: " + sicCommandTopic)
  log.info("Using SIC ResponseTopic: " + sicResponseTopic)
  log.info("Using SIC StatusTopic: " + sicStatusTopic)

  /**
   * Parameterizing the Kafka Configuration for Testing
   */
  def setupEmbeddedKafkaConfig(_bootStrapServer: String = bootStrapServers,
                               _groupId: String = groupId,
                               _topicName: String = sicResponseTopic,
                               _requestTopicName: String = requestTopic,
                               _responseTopicName: String = responseTopic,
                               _sensorCommandTopicName: String = sicCommandTopic): Unit = {
    this.bootStrapServers = _bootStrapServer
    this.groupId = _groupId
    this.sicResponseTopic = _topicName
    this.requestTopic = _requestTopicName
    this.responseTopic = _responseTopicName
    this.sicCommandTopic = _sensorCommandTopicName
  }

}
