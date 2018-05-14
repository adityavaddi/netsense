package com.verizon.netsense.config

import com.verizon.netsense.utils.Logging

object KafkaConfig extends Logging {

  lazy val kafkaConfig = ConfigLoader.config.getConfig("kafka")
  lazy val kafkaHost   = kafkaConfig.getString("host")
  lazy val kafkaPort   = kafkaConfig.getInt("port")
  var bootStrapServers = kafkaHost + ":" + kafkaPort
  require(!bootStrapServers.isEmpty, "BootStrapServers are missing")
  var groupId              = kafkaConfig.getString("group-id")
  var traffic_linec_topic    = kafkaConfig.getString("traffic-linec-topic")
  var traffic_linec_replay_topic   = kafkaConfig.getString("traffic-linec-replay-topic")
  var traffic_objent_topic    = kafkaConfig.getString("traffic-objent-topic")
  var traffic_objent_replay_topic   = kafkaConfig.getString("traffic-objent-replay-topic")
  var traffic_objlev_topic    = kafkaConfig.getString("traffic-objlev-topic")
  var traffic_objlev_replay_topic   = kafkaConfig.getString("traffic-objlev-replay-topic")
  var traffic_objdwl_topic    = kafkaConfig.getString("traffic-objdwl-topic")
  var traffic_objdwl_replay_topic   = kafkaConfig.getString("traffic-objdwl-replay-topic")
  log.info("Connecting to the Kafka bootStrapServers: " + bootStrapServers)
  log.info("Using traffic-linec-topic               : " + traffic_linec_topic )
  log.info("Using traffic-linec-replay-topic        : " + traffic_linec_replay_topic)
  log.info("Using traffic-objent-topic              : " + traffic_objent_topic )
  log.info("Using traffic-objent-replay-topic       : " + traffic_objent_replay_topic)
  log.info("Using traffic-objlev-topic              : " + traffic_objlev_topic )
  log.info("Using traffic-objlev-replay-topic       : " + traffic_objlev_replay_topic)
  log.info("Using traffic-objdwl-topic              : " + traffic_objdwl_topic )
  log.info("Using traffic-objdwl-replay-topic       : " + traffic_objdwl_replay_topic)

  /**
    * Parameterizing the Kafka Configuration for Testing
    */
  def setupEmbeddedKafkaConfig(_bootStrapServer: String = bootStrapServers,
                               _groupId: String = groupId,
                               _trafficLinecTopicName: String = traffic_linec_topic,
                               _trafficLinecReplayTopicName: String = traffic_linec_replay_topic,
                               _trafficObjentTopicName: String = traffic_objent_topic,
                               _trafficObjentReplayTopicName: String = traffic_objent_replay_topic,
                               _trafficObjlevTopicName: String = traffic_objlev_topic,
                               _trafficObjlevReplayTopicName: String = traffic_objlev_replay_topic,
                               _trafficObjdwlTopicName: String = traffic_objdwl_topic,
                               _trafficObjdwlReplayTopicName: String = traffic_objdwl_replay_topic) : Unit = {
    this.bootStrapServers = _bootStrapServer
    this.groupId = _groupId
    this.traffic_linec_topic = _trafficLinecTopicName
    this.traffic_linec_replay_topic = _trafficLinecReplayTopicName
    this.traffic_objent_topic = _trafficObjentTopicName
    this.traffic_objent_replay_topic = _trafficObjentReplayTopicName
    this.traffic_objlev_topic = _trafficObjlevTopicName
    this.traffic_objlev_replay_topic = _trafficObjlevReplayTopicName
    this.traffic_objdwl_topic = _trafficObjdwlTopicName
    this.traffic_objdwl_replay_topic = _trafficObjdwlReplayTopicName
  }

}
