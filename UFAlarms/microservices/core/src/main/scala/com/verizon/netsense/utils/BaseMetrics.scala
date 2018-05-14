package com.verizon.netsense.utils

import java.net.InetSocketAddress
import java.util.concurrent.TimeUnit

import com.codahale.metrics.graphite.{Graphite, GraphiteReporter}
import com.codahale.metrics.{ConsoleReporter, MetricFilter}
import nl.grons.metrics.scala.InstrumentedBuilder

trait BaseInstrumented extends InstrumentedBuilder {
  def graphiteHost: String
  def graphitePort: Int
  val metricRegistry = new BaseMetrics(graphiteHost, graphitePort).metricRegistry
}

class BaseMetrics(graphiteHost: String, graphitePort: Int) {

  val metricRegistry      = new com.codahale.metrics.MetricRegistry()
  val healthCheckRegistry = new com.codahale.metrics.health.HealthCheckRegistry()

  ConsoleReporter
    .forRegistry(metricRegistry)
    .convertRatesTo(TimeUnit.SECONDS)
    .convertDurationsTo(TimeUnit.MILLISECONDS)
    .build()
    .start(10, TimeUnit.SECONDS)

  val graphiteService = new Graphite(new InetSocketAddress(graphiteHost, graphitePort))

  GraphiteReporter
    .forRegistry(metricRegistry)
    .prefixedWith("sensity")
    .convertRatesTo(TimeUnit.SECONDS)
    .convertDurationsTo(TimeUnit.MILLISECONDS)
    .filter(MetricFilter.ALL)
    .build(graphiteService)
    .start(10, TimeUnit.SECONDS)

}
