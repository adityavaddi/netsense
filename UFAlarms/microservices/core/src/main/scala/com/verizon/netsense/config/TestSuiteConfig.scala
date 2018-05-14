package com.verizon.netsense.config

import com.typesafe.config.ConfigFactory

/**
  * Created by maleva on 2/28/18.
  */
trait TestSuiteConfig {


  lazy val configLoader = ConfigFactory.load()

  lazy val embeddedKafkaPort = configLoader.getInt("embedded-kafka.kafka-port")
  lazy val embeddedZkPort    = configLoader.getInt("embedded-kafka.zookeeper-port")


}
