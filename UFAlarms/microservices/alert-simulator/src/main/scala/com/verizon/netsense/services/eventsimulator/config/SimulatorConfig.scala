package com.verizon.netsense.services.eventsimulator.config

import com.verizon.netsense.utils.{ConfigLoader, Logging}

/**
 * Created by maidapr on 6/1/17.
 */
object SimulatorConfig extends Logging {

  lazy val simulatorConfig  = ConfigLoader.config.getConfig("simulator")
  lazy val simType          = simulatorConfig.getString("simtype")
  lazy val throttler        = simulatorConfig.getInt("throttle")
  lazy val recordCount      = simulatorConfig.getInt("eventcount")
  lazy val nodeCount = simulatorConfig.getInt("nodecount")
  lazy val kafkaConfig      = ConfigLoader.config.getConfig("kafka")
  lazy val kafkaHost        = kafkaConfig.getString("host")
  lazy val kafkaPort        = kafkaConfig.getInt("port")
  lazy val bootStrapServers = kafkaHost + ":" + kafkaPort
  lazy val alarmGroupId     = kafkaConfig.getString("alert-group-name")
  lazy val alarmTopic       = kafkaConfig.getString("alarm-request-topic")
  lazy val coreAlarmTopic   = kafkaConfig.getString("core-alarm-request-topic")
  lazy val alertTopic       = kafkaConfig.getString("alert-response-topic")
  lazy val isRequestTopic   = kafkaConfig.getString("interface-service-request-topic")
  lazy val isResponseTopic  = kafkaConfig.getString("interface-service-response-topic")
  lazy val sensorGroupId    = kafkaConfig.getString("sensor-group-name")
  lazy val sensorTopic      = kafkaConfig.getString("sensor-request-topic")
  lazy val coreSensorTopic  = kafkaConfig.getString("core-sensor-request-topic")

  require(!bootStrapServers.isEmpty, "BootStrapServers are missing")
  log.info("Connecting to the Kafka bootStrapServers: " + bootStrapServers)
  log.info("Using alarmTopic: " + alarmTopic + " and " + coreAlarmTopic)
  log.info("Using alertTopic: " + alertTopic)
  log.info("Using sensorTopic: " + sensorTopic + " and " + coreSensorTopic)
  log.info("Using Interface Service Request Topic: " + isRequestTopic)
  log.info("Using Interface Service Response Topic: " + isResponseTopic)
  log.info("Throttle Level: " + throttler + " , Event Count: " + recordCount + ", Node Count: " + nodeCount)
}
