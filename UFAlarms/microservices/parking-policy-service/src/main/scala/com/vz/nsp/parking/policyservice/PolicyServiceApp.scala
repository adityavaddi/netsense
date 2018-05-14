package com.vz.nsp.parking.policyservice

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Supervision}
import com.verizon.netsense.services.HealthCheckService
import com.verizon.netsense.utils.{Logging, ConfigLoader => CoreConfig}
import com.vz.nsp.parking.config.KafkaConfig._
import com.vz.nsp.parking.dblayer.DbLayer
import com.vz.nsp.parking.policyservice.helper.{PolicyHelper, StreamHelper}
import com.vz.nsp.parking.policyservice.service.PolicyService

import scala.util.control.NonFatal
import scala.util.{Failure, Success}
import com.vz.nsp.parking.util.APIInitializer._

object PolicyServiceApp extends App with Logging {

  implicit val system = ActorSystem.create("Parking-Policy-system")

  lazy val mainSupervisor: Supervision.Decider = {
    case NonFatal(ex) => log.error("Exception found in the Stream " + ex.getMessage); Supervision.Restart
    case ex: Exception => log.error("Unhandled Exception seen " + ex.getMessage); Supervision.stop;
  }

  implicit val mat = ActorMaterializer(
    ActorMaterializerSettings(system)
      .withSupervisionStrategy(mainSupervisor)
  )

  val dbLayer = DbLayer()
  val policyHelper = PolicyHelper(dbLayer)
  val streamHelper = StreamHelper(policyHelper)
  val policyService = PolicyService(streamHelper)

  implicit val bind = HealthCheckService.bindHttp()
  bind.onComplete {
    case Success(b) =>
      log.info("Server started, listening on: " + b.localAddress)
      apiInitializer(PolicyPhantom, requestTopicName, bootStrapServers, responseTopicName)
      policyService.queryServiceInit
    case Failure(e) =>
      log.error(s"Server could not bind to health check ${CoreConfig.healthCheckHost}:${CoreConfig.healthCheckPort}")
      system.terminate()
  }

}
