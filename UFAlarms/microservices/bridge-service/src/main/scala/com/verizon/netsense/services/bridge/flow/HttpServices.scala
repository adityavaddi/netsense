package com.verizon.netsense.services.bridge.flow

import akka.http.scaladsl.server.Directives.{complete, get, path}
import com.verizon.netsense.services.bridge.utils.Logging

/**
 * Created by thimija on 8/9/17.
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
