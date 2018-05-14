package com.verizon.netsense.services.alert

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Supervision}
import com.verizon.netsense.services.HealthCheckService
import com.verizon.netsense.services.alert.customexceptions._
import com.verizon.netsense.services.alert.services._
import com.verizon.netsense.utils.{ConfigLoader, Logging}

import scala.util.control.NonFatal
import scala.util.{Failure, Success}

object AlertApp extends App with Logging {
  log.info("Alert Service Started")

  implicit val system = ActorSystem.create("Alert_Service")

  import system.dispatcher

  lazy val mainSupervisor: Supervision.Decider = {
    case NonFatal(ex: OrgDataNotFoundException) => Supervision.Resume
    case NonFatal(ex)                           => log.error("Exception found in the Stream " + ex.getMessage); Supervision.Restart
    case ex: Exception                          => log.error("Unhandled Exception seen " + ex.getMessage); Supervision.Resume;
  }

  implicit val mat = ActorMaterializer(
    ActorMaterializerSettings(system)
      .withSupervisionStrategy(mainSupervisor)
  )

  implicit val bind = HealthCheckService.bindHttp()

  bind.onComplete {
    case Success(b) =>
      log.info("Server started, listening on: " + b.localAddress)

      AlertIngestionService.runnableGraphCommit.run()
      RestService.runnableGraphCommit.run()

    case Failure(e) =>
      log.error(
        s"Server could not bind to health check ${ConfigLoader.healthCheckHost}:${ConfigLoader.healthCheckPort}"
      )
      system.terminate()
  }
}
