package com.verizon.netsense.service

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import com.verizon.netsense.graph.JobGraph
import com.verizon.netsense.helper.OTAFirmwareImpl
import com.verizon.netsense.database.CassandraConnector.connector
import com.verizon.netsense.utils.Logging

object JobService
  extends Logging {

  implicit val system = ActorSystem.create("Job-Service-system")
  implicit val materializer = ActorMaterializer(ActorMaterializerSettings(system))

  def init: Unit = {
    JobGraph(OTAFirmwareImpl, connector).graph.run()
    log.info("JobService started")
  }
}
