package com.verizon.netsense.whatifservice.config

import com.typesafe.config.ConfigFactory

object ServiceConfig {
  lazy val config = ConfigFactory.load()

  /**
   * kafkaRestartableSource
   */
  lazy val kafkaRestartableSource = config.getConfig("kafkaRestartableSource")
  lazy val minBackoffSource       = kafkaRestartableSource.getInt("minBackoffSource")
  lazy val maxBackoffSource       = kafkaRestartableSource.getInt("maxBackoffSource")
  lazy val randomFactorSource     = kafkaRestartableSource.getDouble("randomFactorSource")

  /**
   * kafkaRestartableSink
   */
  lazy val kafkaRestartableSink = config.getConfig("kafkaRestartableSink")
  lazy val minBackoffSink       = kafkaRestartableSink.getInt("minBackoffSink")
  lazy val maxBackoffSink       = kafkaRestartableSink.getInt("maxBackoffSink")
  lazy val randomFactorSink     = kafkaRestartableSink.getDouble("randomFactorSink")

  lazy val sparkCallRetrials = config.getConfig("service").getInt("sparkCallRetrials")

  lazy val sparkImplicitTimeout = config.getConfig("service").getInt("sparkImplicitTimeout")

  lazy val parallelism = config.getConfig("service").getInt("parallelism")

}
