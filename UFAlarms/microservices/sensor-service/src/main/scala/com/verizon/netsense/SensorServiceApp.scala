package com.verizon.netsense

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Supervision}
import com.verizon.netsense.utils.{ConfigLoader => CoreConfig}
import com.verizon.netsense.metrics.JvmMetrics
import com.verizon.netsense.service.{SensorHistoryQueryService, SensorSampleIngestService}
import com.verizon.netsense.services.HealthCheckService
import com.verizon.netsense.utils.Logging

import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/**
 * Created by maidapr on 4/5/17.
 */
object SensorServiceApp extends App with Logging {

  implicit val system = ActorSystem.create("Sensor-Service-system")

  import system.dispatcher

  lazy val mainSupervisor: Supervision.Decider = {
    case NonFatal(ex)  => log.error("Exception found in the Stream " + ex.getMessage); Supervision.Restart
    case ex: Exception => log.error("Unhandled Exception seen " + ex.getMessage); Supervision.stop;
  }

  implicit val mat = ActorMaterializer(
    ActorMaterializerSettings(system)
      .withSupervisionStrategy(mainSupervisor)
  )

  implicit val bind = HealthCheckService.bindHttp()

  bind.onComplete {
    case Success(b) =>
      log.info("Server started, listening on: " + b.localAddress)

      SensorSampleIngestService.eventIngestionInit
      SensorHistoryQueryService.queryServiceInit

//      implicit val JvmMetricRegistry = JvmMetrics.jvmMetricsCalculator
    case Failure(e) =>
      log.error(s"Server could not bind to health check ${CoreConfig.healthCheckHost}:${CoreConfig.healthCheckPort}")
      system.terminate()
  }

}
