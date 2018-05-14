package com.verizon.netsense.config

import com.typesafe.config.ConfigFactory

/**
  * Created by maidapr on 4/7/17.
  */
object ConfigLoader {
  lazy val config = ConfigFactory.load()

  lazy val graphiteConfig = config.getConfig("graphite")
  lazy val graphiteHost   = graphiteConfig.getString("host")
  lazy val graphitePort   = graphiteConfig.getInt("port")

  implicit var metricsEnabled = config.getBoolean("metrics-flag.enabled")
  implicit var serviceInstanceId = config.getString("metrics-flag.service_instance_id")

}
