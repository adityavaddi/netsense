package com.verizon.netsense.services.gps

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Supervision}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.services.HealthCheckService
import com.verizon.netsense.services.gps.db.GpsSinglePointService
import com.verizon.netsense.services.gps.dbLayer.DbLayer
import com.verizon.netsense.services.gps.helper.{DataDealerHelper, GpsHelper, SchGpsEventHelper}
import com.verizon.netsense.services.gps.services.{DataDealerRequestHandlerService, GpsEventIngestionService, ISRequestHandlerservice, SchGpsEventIngestionService}
import com.verizon.netsense.services.gps.util.DBInitializer._
import com.verizon.netsense.utils.{ConfigLoader, Logging}

import scala.util.control.NonFatal
import scala.util.{Failure, Success}

object GpsServiceApp extends Logging with App with Instrumented{

  implicit val system = ActorSystem("Gps-ServiceApp")
  implicit val dispatcher = system.dispatcher


  val supervisor: Supervision.Decider = {
    case NonFatal(ex: Exception) => log.error("Unhandled Non-Fatal exception caught by main supervisor "
      + ex.getMessage); ex.printStackTrace(); Supervision.Resume
    case ex: Exception => log.error("Unhandled exception caught by main supervisor "
       + ex.getMessage); ex.printStackTrace(); Supervision.Restart
  }

  implicit val mat = ActorMaterializer(ActorMaterializerSettings(system)
    .withSupervisionStrategy(supervisor))

  val dbLayer = new DbLayer(new GpsSinglePointService {})
  val gpsHelper = new GpsHelper(dbLayer)
  val ddHelper = new DataDealerHelper(dbLayer)
  val schGpsEventHelper = new SchGpsEventHelper(dbLayer)
  val gpsEventIngestionService = new GpsEventIngestionService(gpsHelper)
  val dataDealerRequestHandlerService = new DataDealerRequestHandlerService(ddHelper)
  val iSRequestHandlerservice = new ISRequestHandlerservice(gpsHelper)
  val schGpsEventIngestionService = new SchGpsEventIngestionService(schGpsEventHelper)

  implicit val bind = HealthCheckService.bindHttp()

  bind.onComplete {
    case Success(b) =>
      log.info("Gps Service Server started, listening on: " + b.localAddress)

      phantomInitializer(GpsPhantom,dbLayer)

      gpsEventIngestionService.runnableGraph.run()

      dataDealerRequestHandlerService.runnableGraph.run()

      iSRequestHandlerservice.runnableGraph.run()

      schGpsEventIngestionService.runnableGraph.run()

    case Failure(e) =>
       log.error(
        s"Server could not bind to health check ${ConfigLoader.healthCheckHost}:${ConfigLoader.healthCheckPort}"
        )
        system.terminate()
      }
}
