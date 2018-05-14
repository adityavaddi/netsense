package com.verizon.netsense

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Supervision}
import com.verizon.netsense.database.{DbLayer, MicroServicesDb, PhantomService, ProductionDb}
import com.verizon.netsense.service.{SCHLightForceService, SCHTransformationService}
import com.verizon.netsense.utils.{Logging, ConfigLoader => CoreConfig}
import com.verizon.netsense.services.HealthCheckService

import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/**
  * Created by ssaurav on 1/2/18.
  */
object TransformerServiceApp extends App with Logging {

  implicit val system = ActorSystem.create("Schedule-Transformer-System")

  lazy val mainSupervisor: Supervision.Decider = {
    case NonFatal(ex)  => log.error("Exception found in the Stream " + ex.getMessage); Supervision.Restart
    case ex: Exception => log.error("Unhandled Exception seen " + ex.getMessage); Supervision.stop;
  }

  implicit val mat = ActorMaterializer(
    ActorMaterializerSettings(system)
      .withSupervisionStrategy(mainSupervisor)
  )
  implicit val ec = ExecutionContext.Implicits.global
  implicit val bind = HealthCheckService.bindHttp()
  implicit val dbLayer = new DbLayer(new PhantomService {
    override def database: MicroServicesDb = ProductionDb
  })

  bind.onComplete {
    case Success(b) =>
      log.info("Server started, listening on: " + b.localAddress)
      SCHTransformationService().assignScheduleGraph.run()
      SCHLightForceService().assignScheduleGraph.run()
    case Failure(e) =>
      log.error(s"Server could not bind to health check ${CoreConfig.healthCheckHost}:${CoreConfig.healthCheckPort}")
      system.terminate()
  }
}
