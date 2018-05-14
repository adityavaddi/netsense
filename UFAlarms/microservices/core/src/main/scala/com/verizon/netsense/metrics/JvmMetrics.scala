package com.verizon.netsense.metrics

import java.lang.management.ManagementFactory

import com.codahale.metrics.MetricRegistry
import com.codahale.metrics.jvm.{
  BufferPoolMetricSet,
  GarbageCollectorMetricSet,
  MemoryUsageGaugeSet,
  ThreadStatesGaugeSet
}

/**
 * Created by davor on 5/4/17.
 */
object JvmMetrics extends Instrumented {

  /**
   * This method is used to fetch JVM metrics
   * here we are adding jvm metrics to metrics registry
   */
  def jvmMetricsCalculator: MetricRegistry = {

    metricRegistry.register("jvm.buffers", new BufferPoolMetricSet(ManagementFactory.getPlatformMBeanServer()))

    metricRegistry.register("jvm.gc", new GarbageCollectorMetricSet())

    metricRegistry.register("jvm.memory", new MemoryUsageGaugeSet())

    metricRegistry.register("jvm.threads", new ThreadStatesGaugeSet())

    metricRegistry

  }

}
