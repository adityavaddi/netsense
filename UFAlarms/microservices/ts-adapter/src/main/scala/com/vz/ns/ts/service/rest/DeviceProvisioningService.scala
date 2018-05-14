package com.vz.ns.ts.service.rest

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.PathMatchers.Segment
import com.vz.ns.ts.service.model.ServiceJsonProtocol.customerProtocol
import com.vz.ns.ts.service.model.TsDevice
import com.vz.ns.ts.service.service.Logging

class DeviceProvisioningService extends Logging {

  val route =
  pathPrefix("device") {
    pathPrefix(Segment) { id: String =>
      pathPrefix("actions") {
        path("register") {
          post {
            log.info("entered DeviceProvisioningService::deviceProvisioning[POST]")
            entity(as[TsDevice]) { tsdevice =>
              complete {
                tsdevice.state = Some("update")
                tsdevice
              }
            }
          }
        }
      }
    }
  } ~
  get {
    path("sample") {
      log.info("entered DeviceProvisioningService::sample[GET]")
      complete("hello")
    }
  }
}
