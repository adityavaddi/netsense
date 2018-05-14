package com.vz.ns.ts.service.service

import akka.http.scaladsl.server.Directives._

/**
 * Created by donthgo on 4/19/17.
 */
object HttpServices extends Logging {

  val routes =
    get {
      path("status") {
        log.info("entered HttpServices::status[GET]")
        complete("OK")
      }
    }
}
