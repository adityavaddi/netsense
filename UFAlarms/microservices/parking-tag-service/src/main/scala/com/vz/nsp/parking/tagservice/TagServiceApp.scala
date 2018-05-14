package com.vz.nsp.parking.tagservice

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Supervision}
import com.verizon.netsense.services.HealthCheckService
import com.verizon.netsense.utils.{Logging, ConfigLoader => CoreConfig}
import com.vz.nsp.parking.dblayer.DbLayer
import com.vz.nsp.parking.tagservice.helper.{ReqResGenerator, StreamHelper, TagHelper}
import com.vz.nsp.parking.tagservice.service.TagService
import com.vz.nsp.parking.util.APIInitializer._

import scala.util.control.NonFatal
import scala.util.{Failure, Success}
import com.vz.nsp.parking.config.KafkaConfig._

object TagServiceApp extends App with Logging {

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
  val reqResGenerator = ReqResGenerator()
  val tagHelper = TagHelper(dbLayer, reqResGenerator)
  val streamHelper = StreamHelper(tagHelper, reqResGenerator)
  val tagService = TagService(streamHelper)

  implicit val bind = HealthCheckService.bindHttp()

  bind.onComplete {
    case Success(b) =>
      log.info("Server started, listening on: " + b.localAddress)
      apiInitializer(TagPhantom,requestTopicName,bootStrapServers,responseTopicName)
      tagService.queryServiceInit
    case Failure( _ ) =>
      log.error(s"Server could not bind to health check ${CoreConfig.healthCheckHost}:${CoreConfig.healthCheckPort}")
      system.terminate()
  }

}
