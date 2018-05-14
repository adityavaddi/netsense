package com.verizon.netsense

import akka.actor.{ActorSystem, Props}
import com.verizon.netsense.util.Logging
import com.verizon.netsense.utils.{ConfigLoader => CoreConfig}
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Supervision}
import com.verizon.netsense.service.{SICService}
import com.verizon.netsense.services.HealthCheckService
import scala.util.{Failure, Success}
import scala.util.control.NonFatal

object MediaService extends App with Logging {

  implicit val system = ActorSystem("MediaService")

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
      log.info("HealthCheck Service started, listening on: " + b.localAddress)
      val sicService = system.actorOf(Props(new SICService))
      sicService ! SICService.ImgCaptureInit

    case Failure(e) =>
      log.error(s"Server could not bind to health check ${CoreConfig.healthCheckHost}:${CoreConfig.healthCheckPort}")
      system.terminate()
  }


}
