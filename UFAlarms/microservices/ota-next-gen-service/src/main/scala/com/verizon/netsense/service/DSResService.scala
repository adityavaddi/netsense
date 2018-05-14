package com.verizon.netsense.service

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import com.verizon.netsense.utils.Logging
import com.verizon.netsense.database.CassandraConnector.connector
import scala.concurrent.ExecutionContext
import com.verizon.netsense.graph.DSResGraph

object DSResService
  extends Logging {

  implicit val system = ActorSystem.create("Device-Service-Res-system")
  implicit val materializer = ActorMaterializer(ActorMaterializerSettings(system))
  implicit val ec = ExecutionContext.Implicits.global

  def init: Unit = {
    DSResGraph(connector).graph.run()
    log.info("DeviceService started")
  }
}
