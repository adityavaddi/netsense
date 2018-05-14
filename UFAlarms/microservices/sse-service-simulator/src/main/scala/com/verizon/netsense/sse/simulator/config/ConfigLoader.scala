package com.verizon.netsense.sse.simulator.config

import com.typesafe.config.ConfigFactory
import com.verizon.netsense.connector.CassandraConnector

/**
 * Created by subrsi9 on 11/20/17.
 */
object ConfigLoader {

  lazy val config = ConfigFactory.load()

  lazy val graphiteConfig = config.getConfig("graphite")
  lazy val grapHostParam  = graphiteConfig.getString("host")
  lazy val grapPortParam  = graphiteConfig.getInt("port")

  lazy val nodeConfig      = config.getConfig("node")
  lazy val orgName         = nodeConfig.getString("org-name")
  lazy val orgId           = nodeConfig.getString("org-id")
  lazy val siteName        = nodeConfig.getString("site-name")
  lazy val siteId          = nodeConfig.getString("site-id")
  lazy val triggerIdPrefix = nodeConfig.getString("trigger-id")
  lazy val application     = nodeConfig.getString("application")
  lazy val nodeName        = nodeConfig.getString("node-name")
  lazy val nodeIdPrefix    = nodeConfig.getString("node-id-prefix")
  lazy val numerOfNode     = nodeConfig.getInt("number-of-node")
  lazy val msgCount        = nodeConfig.getInt("message-count")
  lazy val subscribeCount  = nodeConfig.getInt("subscribe-count")

  lazy val simulatorConfig                = config.getConfig("simulator")
  lazy val parallelism                    = simulatorConfig.getInt("parallelism")
  lazy val parProcess                     = simulatorConfig.getInt("parallel-process")
  lazy val throttleLevel                  = config.getInt("simulator.throttle")
  lazy val isInitDB                       = simulatorConfig.getBoolean("is-init-sse-db")
  lazy val isSimulateLoginReq             = simulatorConfig.getBoolean("is-simulate-login-req")
  lazy val isSimulateSensorSample         = simulatorConfig.getBoolean("is-simulate-sensor-sample")
  lazy val isSimulateCoreNodeSensorSample = simulatorConfig.getBoolean("is-simulate-corenode-sensor-sample")
  lazy val isSimulateDeviceAlarm          = simulatorConfig.getBoolean("is-simulate-device-alarm")
  lazy val isSimulateGpsSample            = simulatorConfig.getBoolean("is-simulate-gps-sample")
  lazy val isSimulateBusinessAlert        = simulatorConfig.getBoolean("is-simulate-business-alert")
  lazy val isSimulateConStatus            = simulatorConfig.getBoolean("is-simulate-connection-status")
  lazy val isSimulateSubscribe            = simulatorConfig.getBoolean("is-simulate-subscribe")
  lazy val isSimulateHeartBeat            = simulatorConfig.getBoolean("is-simulate-heartbeat")
  lazy val isSimulateDisconnect           = simulatorConfig.getBoolean("is-simulate-disconnect")
  lazy val isSimulateBusinessSubscribe    = simulatorConfig.getBoolean("is-simulate-businesssubscribe")
  lazy val isSimulateBusinessHeartBeat    = simulatorConfig.getBoolean("is-simulate-businessheartbeat")
  lazy val isSimulateBusinessDisconnect   = simulatorConfig.getBoolean("is-simulate-businessdisconnect")
  lazy val streamingMQTTTopicPrefix       = simulatorConfig.getString("sse-streaming-topic-prefix")

  lazy val msgInitialDelay    = simulatorConfig.getInt("message-initial-delay")
  lazy val msgInterval        = simulatorConfig.getInt("message-interval")
  lazy val subscribeInterval  = simulatorConfig.getInt("subscribe-interval")
  lazy val heartBeatInterval  = simulatorConfig.getInt("heart-beat-interval")
  lazy val disconnectInterval = simulatorConfig.getInt("disconnect-interval")

  lazy val cassandraConfig            = config.getConfig("cassandra")
  lazy val sseCassandraConnector      = CassandraConnector.connector
  lazy val sseLookupTable             = cassandraConfig.getString("sse-lookup-table")
  lazy val sseStreamSubscribeTtlTable = cassandraConfig.getString("sse-stream-subscribe-ttl-table")
  lazy val sseStreambusinessAlertSubscribeTtlTable =
    cassandraConfig.getString("sse-stream-businessalert-subscribe-ttl-table")
  lazy val sseStreamTtl = cassandraConfig.getLong("sse-stream-subscribe-ttl")

  lazy val kafkaConfig       = config.getConfig("kafka")
  lazy val kafkaHost         = kafkaConfig.getString("host")
  lazy val kafkaPort         = kafkaConfig.getInt("port")
  lazy val kafkaServersParam = kafkaHost + ":" + kafkaPort
  require(!kafkaServersParam.isEmpty, "BootStrapServers are missing")

  lazy val kafkaGroupIdParam     = kafkaConfig.getString("group-name")
  lazy val kafkaClientIdParam    = kafkaConfig.getString("client-id")
  lazy val kafkaOffsetResetParam = kafkaConfig.getString("offset-reset")

  lazy val loginKafkaTopic           = kafkaConfig.getString("sse-login-topic")
  lazy val sensorKafkaTopic          = kafkaConfig.getString("sse-sensor-topic")
  lazy val corenodesensorKafkaTopic  = kafkaConfig.getString("sse-corenodesensor-topic")
  lazy val alertKafkaTopic           = kafkaConfig.getString("sse-alert-topic")
  lazy val gpsKafkaTopic             = kafkaConfig.getString("sse-gps-topic")
  lazy val busalertKafkaTopic        = kafkaConfig.getString("sse-businessalert-topic")
  lazy val conStatusKafkaTopic       = kafkaConfig.getString("sse-connectionstatus-topic")
  lazy val sseSubscribeTopic         = kafkaConfig.getString("sse-filter-topic")
  lazy val sseBusinessSubscribeTopic = kafkaConfig.getString("sse-businessfilter-topic-name")
  lazy val sseSubscribeReplyTopic    = kafkaConfig.getString("sse-filter-reply-topic")

}
