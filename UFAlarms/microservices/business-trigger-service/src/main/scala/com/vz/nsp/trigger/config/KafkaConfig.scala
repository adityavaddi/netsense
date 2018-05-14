package com.vz.nsp.trigger.config

import com.typesafe.config.ConfigFactory
import com.verizon.netsense.utils.Logging

object KafkaConfig extends Logging {

  val appconfig = ConfigFactory.load()

  val kafkaHost = appconfig.getString("kafka.host")
  val kafkaPort = appconfig.getInt("kafka.port")
  var bootStrapServers = kafkaHost + ":" + kafkaPort
  require(!bootStrapServers.isEmpty, "BootStrapServers are missing")
  var groupId = appconfig.getString("kafka.service-group-name")
  var requestTopicName = appconfig.getString("kafka.trigger-request-topic-name")
  var responseTopicName = appconfig.getString("kafka.trigger-response-topic-name")
  log.info("Connecting to the Kafka bootStrapServers: " + bootStrapServers)
  log.info("Listening to requestTopicName: " + requestTopicName)

  /**
    * Parameterizing the Kafka Configuration for Testing
    */
  def setupEmbeddedKafkaConfig(_bootStrapServer: String = bootStrapServers,
                               _groupId: String = groupId,
                               _topicName: String = requestTopicName,
                               _requestTopicName: String = requestTopicName,
                               _responseTopicName: String = responseTopicName): Unit = {
    this.bootStrapServers = _bootStrapServer
    this.groupId = _groupId
    this.requestTopicName = _topicName
    this.requestTopicName = _requestTopicName
    this.responseTopicName = _responseTopicName
  }

}
