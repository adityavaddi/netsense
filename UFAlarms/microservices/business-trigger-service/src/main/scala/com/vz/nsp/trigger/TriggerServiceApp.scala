package com.vz.nsp.trigger

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Supervision}
import com.verizon.netsense.services.HealthCheckService
import com.verizon.netsense.utils.{Logging, ConfigLoader => CoreConfig}
import com.vz.nsp.trigger.config.KafkaConfig._
import com.vz.nsp.triggerservice.helper.ReqResGenerator

import scala.util.control.NonFatal
import scala.util.{Failure, Success}
import com.vz.nsp.trigger.dblayer.DbLayer
import com.vz.nsp.trigger.service.{StreamService, TriggerProcessor, TriggerService}
import com.vz.nsp.trigger.util.APIInitializer.{TriggerPhantom, apiInitializer}
/**
  * Created by maleva on 2/9/18.
  */
object TriggerServiceApp extends App with Logging {

  implicit val system = ActorSystem.create("Business-Trigger-System")

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
  val triggerHelper = TriggerProcessor(dbLayer, reqResGenerator)
  val streamHelper = StreamService(triggerHelper, reqResGenerator)
  val triggerService = TriggerService(streamHelper)

  implicit val bind = HealthCheckService.bindHttp()


  bind.onComplete {
    case Success(b) =>
      log.info("Trigger Service started, health check listening on: " + b.localAddress)
      apiInitializer(TriggerPhantom,requestTopicName,bootStrapServers,responseTopicName)
      triggerService.queryServiceInit
      log.info("Starting Trigger Subscription processing")

    case Failure(e) =>
      log.error(s"Trigger Service could not bind to health check ${CoreConfig.healthCheckHost}:${CoreConfig.healthCheckPort}")
      system.terminate()
  }


}
