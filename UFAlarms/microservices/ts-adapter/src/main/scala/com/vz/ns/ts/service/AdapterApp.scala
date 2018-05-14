package com.vz.ns.ts.service

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import com.verizon.netsense.metrics.JvmMetrics
import com.vz.ns.ts.service.config.ConfigLoader._
import com.vz.ns.ts.service.rest.HttpServices
import com.vz.ns.ts.service.service._
import com.vz.ns.ts.service.util._

object AdapterApp extends App with Logging {

  val _akkaRuntime = AkkaRuntime(ActorSystem.create("ts-adapter"))
  import _akkaRuntime._

  /**
   * Binding host and port with mat
   */
  val http = Http()
  http.bindAndHandle(HttpServices.routes, httpHost, httpPort)

  //Wiring required dependencies
  val httpRequestService = HttpRequestService(http, _akkaRuntime)
  val tsTokenUtil        = TsTokenService()
  val tsAccountUtil      = TsAccountService(tsTokenUtil, httpRequestService, _akkaRuntime)
  val tsDeviceUtil       = TsDeviceService(httpRequestService, _akkaRuntime)
  val tsEventUtil        = TsEventService(tsDeviceUtil, httpRequestService, _akkaRuntime)
  val tsProviderUtil     = TsProviderService(httpRequestService, _akkaRuntime)
  val nsEventConsumer    = NSEventConsumer(tsEventUtil, _akkaRuntime)

  // refresh the cache from casandra
  tsEventUtil.getAllMapping

  // create TS account and provider
  tsProviderUtil.createProvider
  tsAccountUtil.createTsAccount

  //TODO: Refactor accountId (used in TsEventService). It should not exposed from the app.
  val accountId = tsAccountUtil.getAccountId
  log.error(s"2.4 Stack TS Account Id ${accountId}")
  log.error(s"Environment Variable For cassandra Host: ${cassandraHost}")
  log.error(s"Environment Variable For cassandra Port: ${cassandraPort}")

  accountId match {
    case Some(s) => {
      nsEventConsumer.nodeDataConsumerStream.run()
    }
    case None => {
      log.error("ThingSpace Account Id missing")
    }
  }
  implicit val JvmMetricRegistry = JvmMetrics.jvmMetricsCalculator
}
