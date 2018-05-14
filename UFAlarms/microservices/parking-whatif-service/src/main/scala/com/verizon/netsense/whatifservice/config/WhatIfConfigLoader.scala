package com.verizon.netsense.whatifservice.config

import com.typesafe.config.ConfigFactory
import com.verizon.netsense.utils.Logging

object WhatIfConfigLoader extends Logging {

  val whatIfconfig = ConfigFactory.load()

  lazy val whatIfJobTableName = whatIfconfig.getString("cassandra.parking-Whatif-job-table")
  require(!whatIfJobTableName.isEmpty, "parking-Whatif-job-table is missing in config")

  lazy val whatIfParkingPolicyTableName = whatIfconfig.getString("cassandra.whatif-parking-policy-table")
  require(!whatIfParkingPolicyTableName.isEmpty, "whatif-parking-policy-table is missing in config")

  val kafkaHost        = whatIfconfig.getString("kafka.host")
  val kafkaPort        = whatIfconfig.getInt("kafka.port")
  var bootStrapServers = kafkaHost + ":" + kafkaPort
  require(!bootStrapServers.isEmpty, "BootStrapServers are missing")
  var groupId           = whatIfconfig.getString("kafka.whatif-group-name")
  var requestTopicName  = whatIfconfig.getString("kafka.parking-request-topic-name")
  var responseTopicName = whatIfconfig.getString("kafka.parking-response-topic-name")
  var sparkTopicName    = whatIfconfig.getString("kafka.spark-whatif-response-topic-name")

  val sparkLivyServer      = whatIfconfig.getString("spark.livy-server")
  val sparkResourceManager = whatIfconfig.getString("spark.resource-manager")
  val sparkClassName       = whatIfconfig.getString("spark.class-name")
  val sparkFileName        = whatIfconfig.getString("spark.file-name")
  val sparkDriverMemory    = whatIfconfig.getString("spark.driver-memory")
  val sparkName            = whatIfconfig.getString("spark.spark-name")
  val sparkProxyUser       = whatIfconfig.getString("spark.proxy-user")
  val livyPort            = whatIfconfig.getString("spark.livy-port")
  val resourcePort       = whatIfconfig.getString("spark.resource-port")
  val whatIfEnvSuffix       = whatIfconfig.getString("spark.what-env-suffix")
  val confKey       = whatIfconfig.getString("spark.conf-key")
  val confValue      = whatIfconfig.getString("spark.conf-value")

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
