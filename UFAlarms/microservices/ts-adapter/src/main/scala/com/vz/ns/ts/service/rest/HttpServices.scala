package com.vz.ns.ts.service.rest

import akka.http.scaladsl.server.Directives._

object HttpServices {

  val devProvRoute = new DeviceProvisioningService()

  val routes =
    pathPrefix("sensity") {
      pathPrefix("v1") {
        devProvRoute.route
      }
    }
}
