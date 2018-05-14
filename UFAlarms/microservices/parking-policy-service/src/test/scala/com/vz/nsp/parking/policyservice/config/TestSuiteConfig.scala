package com.vz.nsp.parking.policyservice.config

import com.typesafe.config.ConfigFactory

trait TestSuiteConfig {

  lazy val configLoader = ConfigFactory.load()

  lazy val embeddedKafkaPort = configLoader.getInt("embedded-kafka.kafka-port")
  lazy val embeddedZkPort    = configLoader.getInt("embedded-kafka.zookeeper-port")

}
