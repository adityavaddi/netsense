package com.verizon.netsense

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Supervision}
import com.verizon.netsense.service.{APIService, DSResService, JobService, MQTTService}
import com.verizon.netsense.services.HealthCheckService
import com.verizon.netsense.utils.Logging
import com.verizon.netsense.utils.{ConfigLoader => CoreConfig}

import scala.util.{Failure, Success}
import scala.util.control.NonFatal


object OTAApp extends App with Logging {

  implicit val system = ActorSystem("OTA-Next-Gen-system")

  import system.dispatcher

  lazy val mainSupervisor: Supervision.Decider = {
    case NonFatal(ex) => log.error("Exception found in the Stream " + ex.getMessage); Supervision.Restart
    case ex: Exception => log.error("Unhandled Exception seen " + ex.getMessage); Supervision.stop;
  }

  implicit val materializer = ActorMaterializer(
    ActorMaterializerSettings(system)
      .withSupervisionStrategy(mainSupervisor)
  )

  implicit val bind = HealthCheckService.bindHttp()

  bind.onComplete {
    case Success(b) =>
      log.info("Server started, listening on: " + b.localAddress)

      JobService.init
      APIService.init
      DSResService.init
      MQTTService.init

    case Failure(e) =>
      log.error(s"Server could not bind to health check ${CoreConfig.healthCheckHost}:${CoreConfig.healthCheckPort}")
      system.terminate()
  }
}