package com.verizon.netsense.services.bridge

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Supervision}
import com.verizon.netsense.services.HealthCheckService
import com.verizon.netsense.services.bridge.flow.Kafka
import com.verizon.netsense.services.bridge.utils.ConfigLoader.{bridgeType}
import com.verizon.netsense.services.bridge.utils.Logging
import com.verizon.netsense.utils.ConfigLoader

import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/**
 * Created by wswaney on 5/17/17.
 */
object Bridge extends App with Logging {

  implicit val system = ActorSystem()
  var withRabbit = true

  lazy val mainSupervisor: Supervision.Decider = {
    case NonFatal(ex)  => log.warn("Exception found in the Stream " + ex.getMessage); Supervision.Restart
    case ex: Exception => log.error("Unhandled Exception found " + ex.getMessage); Supervision.stop;
  }

  implicit val mat = ActorMaterializer(
      ActorMaterializerSettings(system).withSupervisionStrategy(mainSupervisor)
  )
  implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
  implicit val bind = HealthCheckService.bindHttp()


  val kafka = Kafka(system)
  val toKafka = ToKafka(kafka)
  val fromKafka = FromKafka(kafka)
  val fromMQTT: FromMQTT = FromMQTT(kafka)
  val toMQTT: ToMQTT = ToMQTT(kafka)


  bind.onComplete {
    case Success(b) =>
      log.info("Server started, listening on: " + b.localAddress)
      bridgeType.toString.toLowerCase match {
        case "withrabbit" => {
          withRabbit = true
          log.info("withrabbit - starting bridge 'fromRabbit'")
          val fromRabbit = toKafka.toKafkaFlow
          fromRabbit.run()
          log.info("withrabbit - starting bridge 'toRabbit'")
          val toRabbit = fromKafka.fromKafkaFlow
          toRabbit.run()
        }
        case "withmqtt" => {
          withRabbit = false
          log.info("Starting bridge 'fromMQTT'")
          fromMQTT.multipleTopicMqttGraph.run()
          log.info("Starting bridge 'toMQTT'")
          toMQTT.fromKafkaFlow.run()
        }

        case _ => {
          log.error("Invalid or empty bridgeType, exiting")
          system.terminate()
        }
      }
    case Failure(e) =>
      log.error(s"Server could not bind to health check ${ConfigLoader.healthCheckHost}:${ConfigLoader.healthCheckPort}")
      system.terminate()
  }
}
