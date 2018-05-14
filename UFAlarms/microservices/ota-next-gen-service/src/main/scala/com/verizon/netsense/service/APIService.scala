package com.verizon.netsense.service

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import com.verizon.netsense.graph.APIGraph
import com.verizon.netsense.helper.OTAFirmwareImpl
import com.verizon.netsense.utils.Logging
import scala.concurrent.ExecutionContext
import com.verizon.netsense.database.CassandraConnector.connector

object APIService
  extends Logging {

  implicit val system = ActorSystem.create("API-Service-system")
  implicit val materializer = ActorMaterializer(ActorMaterializerSettings(system))
  implicit val ec = ExecutionContext.Implicits.global

  def init: Unit = {
    APIGraph(OTAFirmwareImpl, connector).graph.run()
    log.info("APIService started")
  }
}
