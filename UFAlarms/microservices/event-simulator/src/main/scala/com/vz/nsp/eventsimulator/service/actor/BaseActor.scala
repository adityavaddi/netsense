package com.vz.nsp.eventsimulator.service.actor

import akka.actor._
import akka.stream.ActorMaterializer

/**
 * Created by Jittara on 4/17/17.
 * For the implicit values
 */
abstract class BaseActor extends Actor {
  implicit val system = ActorSystem.create("NorthboundSimulator-system")
  implicit val ec     = system.dispatcher
  implicit val mat    = ActorMaterializer()
}
