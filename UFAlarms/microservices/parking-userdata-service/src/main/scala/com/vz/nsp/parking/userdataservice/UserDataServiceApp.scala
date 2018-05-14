package com.vz.nsp.parking.userdataservice

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Supervision}
import com.verizon.netsense.services.HealthCheckService
import com.verizon.netsense.utils.{Logging, ConfigLoader => CoreConfig}
import com.vz.nsp.parking.dblayer.DbLayer
import com.vz.nsp.parking.userdataservice.helper.{ReqResGenerator, StreamHelper, UserDataHelper}
import com.vz.nsp.parking.userdataservice.service.UserDataService

import scala.util.control.NonFatal
import scala.util.{Failure, Success}
import com.vz.nsp.parking.util.APIInitializer._
import com.vz.nsp.parking.config.KafkaConfig._

object UserDataServiceApp extends App with Logging {

  implicit val system = ActorSystem.create("User-Data-Service")

  import system.dispatcher

  lazy val mainSupervisor: Supervision.Decider = {
    case NonFatal(ex) => log.error("Exception found in the Stream " + ex.getMessage); Supervision.Restart
    case ex: Exception => log.error("Unhandled Exception seen " + ex.getMessage); Supervision.stop;
  }

  implicit val mat = ActorMaterializer(
    ActorMaterializerSettings(system)
      .withSupervisionStrategy(mainSupervisor)
  )

  val dbLayer = DbLayer()
  val reqResGenerator = ReqResGenerator()
  val userDataHelper = UserDataHelper(dbLayer, reqResGenerator)
  val streamHelper = StreamHelper(userDataHelper, reqResGenerator)
  val userDataService = UserDataService(streamHelper)

  implicit val bind = HealthCheckService.bindHttp()

  bind.onComplete {
    case Success(b) =>
      log.info("Server started, listening on: " + b.localAddress)
      apiInitializer(UserdataPhantom,requestTopicName,bootStrapServers,responseTopicName)
      userDataService.queryServiceInit
    case Failure(e) =>
      log.error(s"Server could not bind to health check ${CoreConfig.healthCheckHost}:${CoreConfig.healthCheckPort}")
      system.terminate()
  }

  //dbLayer.storeUserData()
}
