package com.verizon.netsense.businessalertservice

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Supervision}
import com.verizon.netsense.businessalertservice.config.KafkaConfig._
import com.verizon.netsense.services.HealthCheckService
import com.verizon.netsense.utils.{Logging, ConfigLoader => CoreConfig}
import com.verizon.netsense.businessalertservice.db.BusinessAlertsDbLayer
import com.verizon.netsense.businessalertservice.flow.{BusinessAlertFlow, KafkaConnector}
import com.verizon.netsense.businessalertservice.flow.RestApiFlow
import com.verizon.netsense.businessalertservice.util.BARequestResponseGenerator
import com.verizon.netsense.businessalertservice.validator.BusinessAlertRequestValidator
import com.verizon.netsense.businessalertservice.service._

import scala.util.control.NonFatal
import scala.util.{Failure, Success}
import com.verizon.netsense.businessalertservice.util.APIInitializer.{BusinessAlertPhantom, apiInitializer}

import scala.concurrent.ExecutionContext
object BusinessAlertServiceApp extends App with Logging {

  implicit val system = ActorSystem.create("BusinessAlertSystem")

  lazy val mainSupervisor: Supervision.Decider = {
    case NonFatal(ex)  => log.error("Exception found in the Stream " + ex.getMessage); Supervision.Restart
    case ex: Exception => log.error("Unhandled Exception seen " + ex.getMessage); Supervision.stop;
  }

  implicit val mat = ActorMaterializer(
    ActorMaterializerSettings(system)
      .withSupervisionStrategy(mainSupervisor)
  )
  implicit val ec = ExecutionContext.Implicits.global

  val dbLayer                       = BusinessAlertsDbLayer()
  val reqResGenerator               = BARequestResponseGenerator()
  val businessAlertHelper           = BusinessAlertService(dbLayer, reqResGenerator)
  val streamHelper                  = RestApiFlow(businessAlertHelper, reqResGenerator)
  val kafkaConnector                = KafkaConnector(system)
  val businessAlertRequestValidator = BusinessAlertRequestValidator(kafkaConnector)
  val businessAlertUnMarshalling    = BusinessAlertFlow(streamHelper, businessAlertRequestValidator)
  val businessAlertService          = BusinessAlertGraph(kafkaConnector, businessAlertUnMarshalling)

  implicit val bind = HealthCheckService.bindHttp()

  bind.onComplete {
    case Success(b) =>
      log.info("Server started, listening on: " + b.localAddress)
      apiInitializer(BusinessAlertPhantom,requestTopicName,bootStrapServers,responseTopicName)
      businessAlertService.queryServiceInit
    case Failure(e) =>
      log.error(s"Server could not bind to health check ${CoreConfig.healthCheckHost}:${CoreConfig.healthCheckPort}")
      system.terminate()
  }

}
