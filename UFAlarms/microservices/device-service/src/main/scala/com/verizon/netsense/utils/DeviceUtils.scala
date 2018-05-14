package com.verizon.netsense.utils

import akka.http.scaladsl.server.Route
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._

object HttpServices {
  val route: Route =
    get {
      path("status") {
        complete(HttpEntity(ContentTypes.`application/json`, "{\"status\":\"ok\"}"))
      }
    }
}
