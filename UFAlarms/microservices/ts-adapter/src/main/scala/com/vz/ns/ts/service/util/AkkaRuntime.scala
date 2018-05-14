package com.vz.ns.ts.service.util

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Materializer, Supervision}
import com.vz.ns.ts.service.service.Logging

import scala.concurrent.ExecutionContextExecutor
import scala.util.control.NonFatal

class AkkaRuntime(_actorSystem: ActorSystem) extends Logging {
  implicit val actorSystem: ActorSystem     = _actorSystem
  implicit val ec: ExecutionContextExecutor = actorSystem.dispatcher
  implicit val mat: Materializer = ActorMaterializer(
    ActorMaterializerSettings(actorSystem).withSupervisionStrategy(supervisor)
  )

  lazy val supervisor: Supervision.Decider = {
    case NonFatal(ex) => {
      log.error(s"Exception in the stream: ${ex.getMessage}")
      Supervision.Restart
    }
  }
}

object AkkaRuntime {
  def apply(_actorSystem: ActorSystem): AkkaRuntime = new AkkaRuntime(_actorSystem)
}
