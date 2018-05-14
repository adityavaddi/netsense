package com.vz.ns.ts.service

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.vz.ns.ts.service.config.ConfigLoader._
import com.vz.ns.ts.service.service.{EventSimulator, HttpServices, Logging}

/**
 * Created by donthgo on 5/2/17.
 */
object TsEventSimulator extends App with Logging {

  implicit val system = ActorSystem.create("TsAdapterSimulator")
  implicit val ec     = system.dispatcher
  implicit val mat    = ActorMaterializer()

  /**
   * Binding host and port with mat
   */
  Http().bindAndHandle(HttpServices.routes, httpHost, httpPort)

  /**
   * Publish events to RabbitMQ
   */
  log.info("###Publishing Messages")
  EventSimulator.eventPublisher.run()

}
