package com.verizon.netsense.service

import akka.actor.{Actor, ActorSystem, Props}
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Supervision}
import com.verizon.netsense.config.KafkaConfig._
import com.verizon.netsense.helper.Common
import com.verizon.netsense.service.TrafficService.Init
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import com.verizon.netsense.graph._
import com.verizon.netsense.metrics.Metrics


class TrafficService(var system: ActorSystem) extends Actor with Common {

  implicit val ec = ExecutionContext.Implicits.global

  lazy val decider: Supervision.Decider = {
    case NonFatal(ex)  => log.error("Exception found in the Stream " + ex.printStackTrace()); Supervision.Restart
    case ex: Exception => log.error("Unhandled Exception seen " + ex.printStackTrace()); Supervision.stop;
  }

  implicit val mat = ActorMaterializer(
    ActorMaterializerSettings(system)
      .withInputBuffer(Math.pow(2, 10).toInt, Math.pow(2, 20).toInt)
      .withSupervisionStrategy(decider)
  )

  val mets = Metrics.getMetricRegistry
  log.debug(s"Measured Metrics: ${mets.getMetrics.toString}")

  override def receive: Receive = {
    case Init => {
      // Line Crossing Events Graph
      val linecGraph = new TrafficEventsGraph(system, traffic_linec_topic, traffic_linec_replay_topic) with TrafficGraphStages
      linecGraph.trafficGraph.run()

      // Object Entering Events Graph
      val objentGraph = new TrafficEventsGraph(system, traffic_objent_topic, traffic_objent_replay_topic) with TrafficGraphStages
      objentGraph.trafficGraph.run()

      // Object Leaving Events Graph
      val objlevGraph = new TrafficEventsGraph(system, traffic_objlev_topic, traffic_objlev_replay_topic) with TrafficGraphStages
      objlevGraph.trafficGraph.run()

      // Object Dwel Events Graph
      val objdwlGraph = new TrafficEventsGraph(system, traffic_objdwl_topic, traffic_objdwl_replay_topic) with TrafficGraphStages
      objdwlGraph.trafficGraph.run()

    }
  }

}

object TrafficService {
  val props = Props[TrafficService]
  case object Init
}