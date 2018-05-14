package com.verizon.netsense.metrics

import java.net.{InetAddress, InetSocketAddress}
import java.util.concurrent.TimeUnit

import com.codahale.metrics.{ConsoleReporter, MetricFilter, MetricRegistry, Slf4jReporter}
import com.codahale.metrics.graphite.{Graphite, GraphiteReporter}
import com.verizon.netsense.utils.ConfigLoader._
import com.verizon.netsense.utils.{ConfigLoader, Logging}
import nl.grons.metrics.scala.InstrumentedBuilder
import org.joda.time.Instant
import org.scalatest.tools.FileReporterConfiguration
import org.slf4j.LoggerFactory

/**
  * Created by davor on 5/4/17.
  */
trait Instrumented extends InstrumentedBuilder {

  val metricRegistry = Metrics.getMetricRegistry
}

object Metrics extends Logging {

  private var metricRegistry = new com.codahale.metrics.MetricRegistry()

  val healthCheckRegistry = new com.codahale.metrics.health.HealthCheckRegistry()

  /*
    ConsoleReporter
      .forRegistry(metricRegistry)
      .convertRatesTo(TimeUnit.SECONDS)
      .convertDurationsTo(TimeUnit.MILLISECONDS)
      .build()
      .start(10, TimeUnit.SECONDS)
  */

  implicit val metricsReporter = metricsEnabled.toLowerCase match {
    case "true" =>

      log.info(s"Enabling app instrumented metrics, polling every ${metricsPeriod}s")

      jvmMetricsEnabled.toLowerCase match {
        case "true" => metricRegistry = JvmMetrics.jvmMetricsCalculator
        case _ => log.info("JVM Metrics disabled: Skipping JVM Metrics")
      }

      Slf4jReporter
        .forRegistry(metricRegistry)
        .outputTo(LoggerFactory.getLogger("com.verizon.netsense.metrics"))
        .convertRatesTo(TimeUnit.SECONDS)
        .convertDurationsTo(TimeUnit.MILLISECONDS)
        .build()
        .start(metricsPeriod, TimeUnit.SECONDS)

      val graphiteService = new Graphite(new InetSocketAddress(ConfigLoader.graphiteHost, ConfigLoader.graphitePort))

      val currentTime = Instant.now().toString().replace(".", ":")

      GraphiteReporter
        .forRegistry(metricRegistry)
        .prefixedWith("sensity-" + metricsPrefix + "-" + currentTime)
        .convertRatesTo(TimeUnit.SECONDS)
        .convertDurationsTo(TimeUnit.MILLISECONDS)
        .filter(MetricFilter.ALL)
        .build(graphiteService)
        .start(metricsPeriod, TimeUnit.SECONDS)
    case _ => log.info("Skipping instrumented metrics")
  }

  def getMetricRegistry : MetricRegistry = { metricRegistry }

}
