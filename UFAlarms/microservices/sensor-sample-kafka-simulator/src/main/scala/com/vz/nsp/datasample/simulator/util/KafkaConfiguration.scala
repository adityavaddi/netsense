package com.vz.nsp.datasample.simulator.util

import com.typesafe.config.ConfigFactory

/**
 * Created by maidapr on 4/7/17.
 */
object KafkaConfiguration {

  lazy val config = ConfigFactory.load()

  // Graphite Configuration
  lazy val graphiteConfig = config.getConfig("graphite")

  lazy val graphiteHost = graphiteConfig.getString("host")
  lazy val graphitePort = graphiteConfig.getInt("port")
  // Kafka Configuration
  lazy val kafkaConfig = config.getConfig("kafka")

  lazy val kafkaHost  = kafkaConfig.getString("host")
  lazy val kafkaPort  = kafkaConfig.getInt("port")
  lazy val kafkaTopic = kafkaConfig.getString("sensor-sample-topic-name")

  lazy val throttleLevel: Int    = config.getInt("simulator.throttle")
  lazy val simulatorHost: String = config.getString("simulator.host")

  println("Connecting to kafka: " + kafkaHost + ":" + kafkaPort)
  println("Using Kafka topic:" + kafkaTopic)

}
