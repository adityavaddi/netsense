package com.verizon.netsense.services.eventsimulator

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Supervision}
import com.verizon.netsense.services.HealthCheckService
import com.verizon.netsense.services.eventsimulator.simulator.{AlertIngestionSimulator, RestSimulator, SensorIngestionSimulator}
import com.verizon.netsense.services.eventsimulator.simulator.AlertIngestionSimulator.{eventCount, loadOrgHierarchy}
import com.verizon.netsense.utils.{ConfigLoader, Logging}
import com.verizon.netsense.services.eventsimulator.config.SimulatorConfig._
import com.verizon.netsense.services.eventsimulator.database.SimulatorDB
import com.verizon.netsense.services.eventsimulator.simulator.RestSimulator.loadAlerts

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

object SimulatorApp extends App with Logging {
  log.info(s"Event Simulator Started: $simType")

  implicit val system = ActorSystem.create("Event_Simulator")
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  lazy val mainSupervisor: Supervision.Decider = {
    case NonFatal(ex)  => log.error("Exception found in the Stream " + ex.getMessage); Supervision.Restart
    case ex: Exception => log.error("Unhandled Exception seen " + ex.getMessage); Supervision.Resume;
  }

  implicit val mat = ActorMaterializer(
    ActorMaterializerSettings(system)
      .withSupervisionStrategy(mainSupervisor)
  )

  implicit val bind = HealthCheckService.bindHttp()

  bind.onComplete {
    case Success(b) =>
      log.info("Server started, listening on: " + b.localAddress)
      simType match {
        case "alarm" =>
          log.info("Running Alert-Ingestion-Simulator...\nLoading org hierarchy data into Org Hierarchy table...")
          loadOrgHierarchy
          log.info("Test data loaded into Org Hierarchy table.")
          log.info("Pumping " + eventCount + " Video node alarms to Kafka topic: " + alarmTopic)
          AlertIngestionSimulator.runnableGraphCommit.run()

        case "sensor" =>
          log.info("Running Sensor-Ingestion-Simulator...")
          Await.ready(SimulatorDB.SensorSamples.truncateTable, 10.seconds)
          SensorIngestionSimulator.runnableGraphCommit.run()

        case _ =>
          log.info("Running Alert-Ingestion-Simulator...\nLoading org hierarchy data into Org Hierarchy table...")
          loadOrgHierarchy
          log.info("Test data loaded into Org Hierarchy table.")
          log.info("Pumping " + eventCount + " Video node alarms to Kafka topic: " + alarmTopic)
          AlertIngestionSimulator.runnableGraphCommit.run()

          log.info("Running Sensor-Ingestion-Simulator...")
          Await.ready(SimulatorDB.SensorSamples.truncateTable, 10.seconds)
          SensorIngestionSimulator.runnableGraphCommit.run()
      }

//      log.info("Running Alert-Rest-Simulator...\nLoading sample alerts into Alerts table...")
//      loadAlerts
//      log.info("Test data loaded into Alerts table.\nPumping API Requests to Kafka...")
//
//      RestSimulator.runnableGraphCommit.run()

    case Failure(e) =>
      log.error(
        s"Server could not bind to health check ${ConfigLoader.healthCheckHost}:${ConfigLoader.healthCheckPort}"
      )
      system.terminate()
  }
}
