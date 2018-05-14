package com.vz.ns.ts.service.rest

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model.ContentTypes._
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.vz.ns.ts.service.model.ServiceJsonProtocol.customerProtocol
import com.vz.ns.ts.service.model.TsDevice
import org.scalatest.{FlatSpec, _}

class DeviceProvisioningServiceTest extends FlatSpec with Matchers with ScalatestRouteTest {

  val tsDeviceInput =
    TsDevice(Option("tsdeviceid"), None, None, None, None, None, None, None, None, None, None, None, None, None)
  val tsDeviceOutput = TsDevice(Option("tsdeviceid"),
                                None,
                                None,
                                None,
                                None,
                                None,
                                None,
                                None,
                                None,
                                None,
                                None,
                                None,
                                Option("update"),
                                None)

  val dev = new DeviceProvisioningService
  "GET call" should "return string hello " in {
    Get(s"/sample") ~> dev.route ~> check {
      status shouldBe OK
      responseAs[String] shouldBe "hello"
    }
  }

  "POST call" should "take device input and updates state to string update" in {
    Post(s"/device/123/actions/register", tsDeviceInput) ~> dev.route ~> check {
      status shouldBe OK
      contentType shouldBe `application/json`
      responseAs[TsDevice] shouldBe tsDeviceOutput
    }
  }
}
