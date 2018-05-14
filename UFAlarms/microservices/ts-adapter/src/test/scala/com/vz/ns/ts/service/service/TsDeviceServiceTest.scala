package com.vz.ns.ts.service.service

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{HttpMethods, HttpRequest, HttpResponse, StatusCodes}
import com.vz.ns.ts.service.config.ConfigLoader._
import com.vz.ns.ts.service.model.TsDeviceInfo
import com.vz.ns.ts.service.util.{AkkaRuntime, HttpRequestService}
import org.mockito.ArgumentCaptor.forClass
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FunSuite, Matchers}

import scala.concurrent.Future
import scala.concurrent.Future.successful

class TsDeviceServiceTest extends FunSuite with Matchers with MockitoSugar {

  test("should create a provider") {
    val httpRequestService = mock[HttpRequestService]
    val runtime            = AkkaRuntime(ActorSystem.create("ts-adapter-test"))
    val argumentCaptor     = forClass(classOf[HttpRequest])

    val tsDeviceService = TsDeviceService(httpRequestService, runtime)

    when(httpRequestService.makeRequest(any(classOf[HttpRequest])))
      .thenReturn(successful(HttpResponse(StatusCodes.OK)))

    val tsDeviceInfo = TsDeviceInfo(
      Some(devicekind),
      Some(deviceversion),
      Some("dummyname"),
      Some(providerid),
      Some("node1"),
      Some("dummySensorType"),
      deviceState,
      "id"
    )

    tsDeviceService.createDevice(tsDeviceInfo)

    verify(httpRequestService).makeRequest(argumentCaptor.capture())
    val httpRequestParams: HttpRequest = argumentCaptor.getValue

    (httpRequestParams.method) shouldEqual HttpMethods.POST
    (httpRequestParams.uri.path.toString()) shouldEqual "/south/v2/devices"
    val result =
      "{\"kind\":\"ts.device.light.sensity\",\"version\":\"1.0\",\"name\":\"dummyname\",\"providerid\":\"d8fd9f7f-081b-4f37-a3b8-1650821b69f2\",\"refid\":\"node1\",\"qrcode\":\"dummySensorType\",\"state\":\"update\",\"foreignid\":\"id\"}"
    (httpRequestParams.entity.toString) shouldEqual s"HttpEntity.Strict(application/json,$result)"
  }

  test("should get device id using ref id") {
    val httpRequestService = mock[HttpRequestService]
    val runtime            = AkkaRuntime(ActorSystem.create("ts-adapter-test"))
    val argumentCaptor     = forClass(classOf[HttpRequest])

    val tsDeviceService = TsDeviceService(httpRequestService, runtime)

    when(httpRequestService.makeRequest(any(classOf[HttpRequest])))
      .thenReturn(successful(HttpResponse(StatusCodes.OK)))

    val id: Future[Option[String]] = tsDeviceService.getTsDeviceByRefId("node1")

    verify(httpRequestService).makeRequest(argumentCaptor.capture())
    val httpRequestParams: HttpRequest = argumentCaptor.getValue

    (httpRequestParams.method) shouldEqual HttpMethods.POST
    (httpRequestParams.uri.path.toString()) shouldEqual "/south/v2/devices/actions/$query"
    val result = "{\"$filter\" : {\"refid\" : \"node1\"}}"
    (httpRequestParams.entity.toString) shouldEqual s"HttpEntity.Strict(application/json,$result)"
  }
}
