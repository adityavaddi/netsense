package com.verizon.netsense.kafka.init

import com.typesafe.config.ConfigFactory

object ConfigLoader {
  lazy val config = ConfigFactory.load()
  lazy val zkConfig    = config.getConfig("zk")
  lazy val zkPort      = zkConfig.getString("port")
  lazy val zkHost      = zkConfig.getString("host") + ":" + zkPort
  lazy val zkSessionTimeout      = zkConfig.getInt("sessionTimeOut")
  lazy val zkConnectionTimeout   = zkConfig.getInt("connectionTimeOut")

  //kafka topics configuration
  lazy val topicsConfig = ConfigFactory.load("topics.conf")
  lazy val topics      = topicsConfig.getConfigList("kafka-topics")
  lazy val envSuffix = config.getString("envSuffix")
}
