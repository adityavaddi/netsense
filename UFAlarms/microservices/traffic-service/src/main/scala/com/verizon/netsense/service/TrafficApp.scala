package com.verizon.netsense.service


import akka.actor.{ActorSystem, Props}
import com.verizon.netsense.utils.{ConfigLoader, Logging}
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Supervision}
import com.verizon.netsense.services.HealthCheckService
import scala.util.{Failure, Success}
import scala.util.control.NonFatal


object TrafficApp extends App with Logging {

  implicit val system = ActorSystem("TrafficService")

  import system.dispatcher

  lazy val mainSupervisor: Supervision.Decider = {
    case NonFatal(ex)  => log.error("Exception found in the Stream " + ex.getMessage); Supervision.Restart
    case ex: Exception => log.error("Unhandled Exception seen " + ex.getMessage); Supervision.stop;
  }

  val trafficService = system.actorOf(Props(classOf[TrafficService], system))
  trafficService ! TrafficService.Init

}

