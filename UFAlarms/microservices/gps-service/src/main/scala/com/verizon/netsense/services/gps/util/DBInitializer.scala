package com.verizon.netsense.services.gps.util

import com.verizon.netsense.services.gps.dbLayer.DbLayer
import com.verizon.netsense.utils.Logging

import scala.concurrent.ExecutionContext

object DBInitializer extends Logging {

  implicit val ec = ExecutionContext.Implicits.global

  sealed trait Application

  case object GpsPhantom extends Application

  /**
    * This will initiliaze the phantom service so that the request should wait for long timer or end up in timeouts
    */
  def phantomInitializer(application: Application,dbLayer: DbLayer) = {
    log.info("Initializing database phantom layer ")
    application match {
      case GpsPhantom => dbLayer.getGpsByNodeId("nodeid","orgid", "siteid").map(r => log.info("Result should be empty -> " + r))
      case _ => log.error("Unknown service")
    }
  }
}
