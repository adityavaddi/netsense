package com.verizon.netsense.utils

import com.typesafe.config.ConfigFactory

/**
 * Created by davor on 5/4/17.
 */
object ConfigLoader{
  lazy val config          = ConfigFactory.load()
  lazy val graphiteHost    = ConfigLoader.config.getString("graphite.host")
  lazy val graphitePort    = ConfigLoader.config.getInt("graphite.port")
  lazy val healthCheckHost = ConfigLoader.config.getString("health-check.host")
  lazy val healthCheckPort = ConfigLoader.config.getInt("health-check.port")
  lazy val kafkaHost       = ConfigLoader.config.getString("kafka.host")
  lazy val kafkaPort       = ConfigLoader.config.getInt("kafka.port")
  lazy val groupId         = ConfigLoader.config.getString("kafka.group-name")
  lazy val topicName       = ConfigLoader.config.getString("kafka.topic-name")
  lazy val parallellism    = ConfigLoader.config.getInt("kafka.parallellism")
  lazy val metricsEnabled  = ConfigLoader.config.getString("metrics.enabled")
  lazy val metricsPeriod  = ConfigLoader.config.getLong("metrics.period")
  lazy val jvmMetricsEnabled = ConfigLoader.config.getString("metrics.enabled-jvm-metrics")
  lazy val metricsPrefix =  ConfigLoader.config.hasPath("metrics.prefix") match {
      case true => ConfigLoader.config.getString("metrics.prefix")
      case _ => "ns"
    }
  lazy val rabbitMQRetryCount = ConfigLoader.config.getInt("amqp.retry.count")
  lazy val mqttRetryCount     = ConfigLoader.config.getInt("mqtt.retry.count")
}