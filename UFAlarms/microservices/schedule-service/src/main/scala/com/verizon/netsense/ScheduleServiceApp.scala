package com.verizon.netsense

import java.net.ConnectException

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Supervision}
import com.datastax.driver.core.exceptions.NoHostAvailableException
import com.verizon.netsense.database.{MicroServicesDb, PhantomService, ProductionDb}
import com.verizon.netsense.helper.{Neo4jHelper, Neo4jService}
import com.verizon.netsense.utils.{Logging, ConfigLoader => CoreConfig}
import com.verizon.netsense.service.{DbLayer, ScheduleAssignService}
import com.verizon.netsense.services.HealthCheckService

import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/**
  * Created by ssaurav on 1/2/18.
  */
object ScheduleServiceApp extends App with Logging {

  implicit val system = ActorSystem.create("schedule-assign-system")

  lazy val mainSupervisor: Supervision.Decider = {
    case ex: ConnectException         => log.error("Unable to connect to Graphite " + ex.getMessage); Supervision.Resume
    case ex: NoHostAvailableException => log.error("Unable to connect to DB " + ex.getMessage); Supervision.Resume
    case NonFatal(ex)                 => log.error("Exception found in the Stream " + ex.getMessage)
      ex.printStackTrace(); Supervision.Resume
    case ex: Exception                => log.error("Unhandled Exception seen " + ex.getMessage)
      ex.printStackTrace(); Supervision.stop
  }

  implicit val mat = ActorMaterializer(
    ActorMaterializerSettings(system)
      .withInputBuffer(Math.pow(2, 10).toInt, Math.pow(2, 20).toInt)
      .withSupervisionStrategy(mainSupervisor))

  implicit val ec = ExecutionContext.Implicits.global
  implicit val bind = HealthCheckService.bindHttp()

  bind.onComplete {
    case Success(b) =>
      log.info("Server started, listening on: " + b.localAddress)

      implicit val dbLayer = new DbLayer(new PhantomService {
        override def database: MicroServicesDb = ProductionDb
      }, new Neo4jService(new Neo4jHelper) {})

      ScheduleAssignService().assignScheduleGraph.run()
    case Failure(e) =>
      log.error(s"Server could not bind to health check ${CoreConfig.healthCheckHost}:${CoreConfig.healthCheckPort}")
      system.terminate()
  }
}
