package com.verizon.netsense.services

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.verizon.netsense.connector.KafkaConnection
import com.verizon.netsense.utils.{ConfigLoader, HttpServices}

object DeviceService extends App {

  implicit val system = ActorSystem("device_service")
  implicit val mat    = ActorMaterializer()

  //DeviceConnectionStatusDbService.start()

  Thread.sleep(30000)

  val kafkaConnection = new KafkaConnection(system)

  val deviceLoginService = new DeviceLoginKafkaService(kafkaConnection)(system)

//  val isConsumerActor     = new ISConsumerActor(kafkaConnection)(system)
//  val isPublisherActor    = system.actorOf(ISPublisherActor.props(kafkaConnection))
  //deviceLoginService.runnableGraphCommit.run()
  /**
   * Binding host and port with mat
   */
  Http().bindAndHandle(HttpServices.route,
                       ConfigLoader.config.getString("httpHealth.host"),
                       ConfigLoader.config.getString("httpHealth.port").toInt)
}
