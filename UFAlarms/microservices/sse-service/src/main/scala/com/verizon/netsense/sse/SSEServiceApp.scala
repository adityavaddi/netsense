package com.verizon.netsense.sse

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Supervision}
import com.verizon.netsense.services.HealthCheckService
import com.verizon.netsense.sse.app.{
  SSEBusinessService,
  SSEBusinessSubscribeService,
  SSEFlowService,
  SSESubscribeService
}
import com.verizon.netsense.utils.{Logging, ConfigLoader => CoreConfig}

import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/**
 * Created by muppasw on 8/22/17.
 */
object SSEServiceApp extends App with Logging {
  implicit val system = ActorSystem.create("SSE-Service-system")
  implicit val ec     = scala.concurrent.ExecutionContext.Implicits.global

  lazy val mainSupervisor: Supervision.Decider = {

    case NonFatal(ex)  => log.warn("Exception found in the Stream " + ex.getMessage); Supervision.Restart
    case ex: Exception => log.error("Unhandled Exception found " + ex.getMessage); Supervision.stop;
  }

  implicit val mat = ActorMaterializer(
    ActorMaterializerSettings(system)
      .withSupervisionStrategy(mainSupervisor)
  )

  implicit val bind = HealthCheckService.bindHttp()

  bind.onComplete {
    case Success(b) =>
      log.info("SSE Service started, health check listening on: " + b.localAddress)

      /*
       *    Interface service will POST subsribe/heartbeat/disconnect message to Kafka for SSE service
       *    which inserts/updates/disconnect data in Cassandra
       */
      log.info("Starting SSE Subscription processing")
      SSESubscribeService.sseSubscribe.run()

      /*
       *  Subscribes the data in sse_filter_businessalert table
       */
      log.info("Starting SSE Business Alert Subscription processing")
      SSEBusinessSubscribeService.sseBusinessAlertSubscribe.run()

      /*
       *  Streaming of filtered messages to Mqtt
       */
      log.info("Starting SSE Stream processing")
      SSEFlowService.sseGraph.run()

      /*
       *  Streaming of filtered businessalert data to Mqtt
       */

      log.info("Starting SSE Business Alert Stream processing")
      SSEBusinessService.sseBusinessGraph.run()

    case Failure(e) =>
      log.error(
        s"SSE Service could not bind to health check ${CoreConfig.healthCheckHost}:${CoreConfig.healthCheckPort}"
      )
      system.terminate()
  }

}
