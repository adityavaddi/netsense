package com.verizon.netsense.config

import com.typesafe.config.ConfigFactory

/**
 * Created by maidapr on 4/7/17.
 */
object ConfigLoader {
  lazy val config = ConfigFactory.load()

  lazy val graphiteConfig       = config.getConfig("graphite")
  lazy val graphiteHost         = graphiteConfig.getString("host")
  lazy val graphitePort         = graphiteConfig.getInt("port")
  lazy val envSuffix            = config.getString("env-suffix")
  lazy val streamParallelism    = config.getInt("stream-parallelism")
  require(streamParallelism > 0, "Stream parallelism can't be zero")

}
