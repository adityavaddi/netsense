package com.vz.nsp.datasample.simulator.metrics

import java.net.InetSocketAddress
import java.util.UUID
import java.util.concurrent.TimeUnit

import com.codahale.metrics.graphite.{Graphite, GraphiteReporter}
import com.codahale.metrics.{ConsoleReporter, MetricFilter}
import com.vz.nsp.datasample.simulator.util.KafkaConfiguration._
import nl.grons.metrics.scala.InstrumentedBuilder

trait Instrumented extends InstrumentedBuilder {
  val metricRegistry = Metrics.metricRegistry
}

object Metrics {

  private val intervalSecondPicked   = 7
  private val timeInSeconds          = (System.currentTimeMillis() / 1000).toString.takeRight(1).toInt
  private var outOfSyncDiffInSeconds = 0
  if (timeInSeconds < intervalSecondPicked) {
    outOfSyncDiffInSeconds = intervalSecondPicked - timeInSeconds
  } else {
    outOfSyncDiffInSeconds = (intervalSecondPicked + 10) - timeInSeconds
  }

  lazy val metricsId       = "sensity-" + UUID.randomUUID().toString
  lazy val graphiteService = new Graphite(new InetSocketAddress(graphiteHost, graphitePort))
  val metricRegistry       = new com.codahale.metrics.MetricRegistry()
  val healthCheckRegistry  = new com.codahale.metrics.health.HealthCheckRegistry()

  if (outOfSyncDiffInSeconds > 0) {
    Thread.sleep(outOfSyncDiffInSeconds * 1000) // To start the instances reporting the metrics with same interval
  }
  invokeReporter()

  def invokeReporter(): Unit = {
    ConsoleReporter
      .forRegistry(metricRegistry)
      .convertRatesTo(TimeUnit.SECONDS)
      .convertDurationsTo(TimeUnit.MILLISECONDS)
      .build()
      .start(10, TimeUnit.SECONDS)

    GraphiteReporter
      .forRegistry(metricRegistry)
      .prefixedWith {
        println("Metrics reported with prefix " + metricsId)
        metricsId
      }
      .convertRatesTo(TimeUnit.SECONDS)
      .convertDurationsTo(TimeUnit.MILLISECONDS)
      .filter(MetricFilter.ALL)
      .build(graphiteService)
      .start(10, TimeUnit.SECONDS)
  }

}
