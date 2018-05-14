package com.verizon.netsense.services.alert.helper

import com.verizon.netsense.utils.{ConfigLoader, Logging}

/**
 * Created by maidapr on 6/1/17.
 */
object KafkaConfig extends Logging {

  lazy val kafkaConfig      = ConfigLoader.config.getConfig("kafka")
  lazy val kafkaHost        = kafkaConfig.getString("host")
  lazy val kafkaPort        = kafkaConfig.getInt("port")
  lazy val bootStrapServers = kafkaHost + ":" + kafkaPort
  lazy val groupId          = kafkaConfig.getString("alert-group-name")
  lazy val alarmTopic       = kafkaConfig.getString("alarm-request-topic")
  lazy val coreAlarmTopic   = kafkaConfig.getString("core-alarm-request-topic")
  lazy val alertTopic       = kafkaConfig.getString("alert-response-topic")
  lazy val isRequestTopic   = kafkaConfig.getString("interface-service-request-topic")
  lazy val isResponseTopic  = kafkaConfig.getString("interface-service-response-topic")
  require(!bootStrapServers.isEmpty, "BootStrapServers are missing")
  log.info("Kafka bootStrapServers: " + bootStrapServers)
  log.info("AlarmTopics: " + alarmTopic + " and " + coreAlarmTopic)
  log.info("AlertTopic: " + alertTopic)
  log.info("API Service Request Topic: " + isRequestTopic)
  log.info("API Service Response Topic: " + isResponseTopic)

}
