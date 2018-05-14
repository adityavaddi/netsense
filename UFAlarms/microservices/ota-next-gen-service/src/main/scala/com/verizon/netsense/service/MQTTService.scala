package com.verizon.netsense.service

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import com.verizon.netsense.graph.MQTTMessagesGraph
import com.verizon.netsense.utils.Logging
import com.verizon.netsense.database.CassandraConnector.connector
import com.verizon.netsense.helper.OTAFirmwareImpl

object MQTTService
  extends Logging {

  implicit val system = ActorSystem.create("MQTT-Service-system")
  implicit val materializer = ActorMaterializer(ActorMaterializerSettings(system))

  def init: Unit = {
    MQTTMessagesGraph(OTAFirmwareImpl, connector).graph.run()
    log.info("JobService started")
  }
}
