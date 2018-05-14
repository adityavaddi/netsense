package com.vz.nsp.parking.grouppolicyservice

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Supervision}
import com.verizon.netsense.services.HealthCheckService
import com.verizon.netsense.utils.{Logging, ConfigLoader => CoreConfig}
import com.vz.nsp.parking.dblayer.DbLayer
import com.vz.nsp.parking.grouppolicyservice.helper.StreamHelper
import com.vz.nsp.parking.grouppolicyservice.service.{GroupPolicyService, GroupPolicyServiceAndSpaceService, ParkingSpaceService}
import com.vz.nsp.parking.util.APIInitializer._

import scala.util.control.NonFatal
import scala.util.{Failure, Success}
import com.vz.nsp.parking.config.KafkaConfig._


object ParkinggroupPolicyServiceApp extends App with Logging {

  implicit val system = ActorSystem.create("Parkinggroup-Policy-System")

  lazy val mainSupervisor: Supervision.Decider = {
    case NonFatal(ex) => log.error("Exception found in the Stream " + ex.getMessage); Supervision.Restart
    case ex: Exception => log.error("Unhandled Exception seen " + ex.getMessage); Supervision.stop;
  }

  implicit val mat = ActorMaterializer(
    ActorMaterializerSettings(system)
      .withSupervisionStrategy(mainSupervisor)
  )

  val dbLayer = DbLayer()
  val groupPolicyHelper = GroupPolicyService(dbLayer)
  val parkingSpotHelper = ParkingSpaceService(dbLayer)
  val streamHelper = StreamHelper(groupPolicyHelper, parkingSpotHelper)
  val groupPolicyService = GroupPolicyServiceAndSpaceService(streamHelper)

  implicit val bind = HealthCheckService.bindHttp()

  bind.onComplete {
    case Success(b) =>
      log.info("Server started, listening on: " + b.localAddress)
      apiInitializer(GroupPolicyPhantom,requestTopicName,bootStrapServers,responseTopicName)
      groupPolicyService.groupPolicyServiceInit

    case Failure(e) =>
      log.error(s"Server could not bind to health check ${CoreConfig.healthCheckHost}:${CoreConfig.healthCheckPort}")
      system.terminate()
  }

}
