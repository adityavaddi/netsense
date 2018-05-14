package com.verizon.netsense.whatifservice

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Supervision}
import com.verizon.netsense.services.HealthCheckService
import com.verizon.netsense.whatifservice.config.WhatIfConfigLoader._
import com.verizon.netsense.utils.{Logging, ConfigLoader => CoreConfig}
import com.verizon.netsense.whatifservice.flow.{KafkaConnector, RestApiFlow, WhatIfFlow}
import com.verizon.netsense.whatifservice.util.RequestResponseGenerator
import com.verizon.netsense.whatifservice.db.WhatIfDbLayer
import com.verizon.netsense.whatifservice.service.{SparkJobRESTService, WhatIfGraph, WhatIfService}
import com.verizon.netsense.whatifservice.validator.WhatIfRequestValidator
import com.verizon.netsense.whatifservice.util.APIInitializer._

import scala.util.{Failure, Success}
import scala.util.control.NonFatal

/**
 * Created by maleva on 3/17/18.
 */
object WhatIfServiceApp extends App with Logging {

  implicit val system = ActorSystem.create("WhatIfSystem")

  lazy val mainSupervisor: Supervision.Decider = {
    case NonFatal(ex)  => log.error("Exception found in the Stream " + ex.getMessage); Supervision.Restart
    case ex: Exception => log.error("Unhandled Exception seen " + ex.getMessage); Supervision.stop;
  }

  implicit val mat = ActorMaterializer(
    ActorMaterializerSettings(system)
      .withSupervisionStrategy(mainSupervisor)
  )

  val dbLayer                = WhatIfDbLayer()
  val reqResGenerator        = RequestResponseGenerator()
  val sparkJobRESTService    = SparkJobRESTService(reqResGenerator, dbLayer)
  val whatIfHelper           = WhatIfService(dbLayer, reqResGenerator, sparkJobRESTService)
  val streamHelper           = RestApiFlow(whatIfHelper, reqResGenerator)
  val kafkaConnector         = KafkaConnector(system)
  val whatIfRequestValidator = WhatIfRequestValidator(kafkaConnector)
  val WhatIfUnMarshalling    = WhatIfFlow(streamHelper, whatIfRequestValidator, sparkJobRESTService)
  val whatIfService          = WhatIfGraph(kafkaConnector, WhatIfUnMarshalling)

  implicit val bind = HealthCheckService.bindHttp()

  bind.onComplete {
    case Success(b) =>
      log.info("Server started, listening on: " + b.localAddress)
      apiInitializer(WhatIFPhantom, requestTopicName, bootStrapServers, responseTopicName)
      whatIfService.queryServiceInit
    case Failure(e) =>
      log.error(s"Server could not bind to health check ${CoreConfig.healthCheckHost}:${CoreConfig.healthCheckPort}")
      system.terminate()
  }

}
