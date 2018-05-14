package com.verizon.netsense.services.alert.services.config

import com.typesafe.config.ConfigFactory

/**
 * Created by maidapr on 6/13/17.
 */
trait TestSuiteConfig {

  lazy val configLoader = ConfigFactory.load()

  lazy val embeddedKafkaPort = configLoader.getInt("embedded-kafka.kafka-port")
  lazy val embeddedZkPort    = configLoader.getInt("embedded-kafka.zookeeper-port")

}
