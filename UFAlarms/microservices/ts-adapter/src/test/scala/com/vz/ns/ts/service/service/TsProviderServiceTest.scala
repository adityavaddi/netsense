package com.vz.ns.ts.service.service

import akka.actor.ActorSystem
import akka.http.scaladsl.model._
import com.vz.ns.ts.service.util.{AkkaRuntime, HttpRequestService}
import org.mockito.ArgumentCaptor.forClass
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FunSuite, Matchers}

import scala.concurrent.Future.successful

class TsProviderServiceTest extends FunSuite with Matchers with MockitoSugar {
  test("should create a provider") {
    val httpRequestService = mock[HttpRequestService]
    val runtime            = AkkaRuntime(ActorSystem.create("ts-adapter-test"))
    val argumentCaptor     = forClass(classOf[HttpRequest])

    val tsProviderService = TsProviderService(httpRequestService, runtime)

    when(httpRequestService.makeRequest(any(classOf[HttpRequest])))
      .thenReturn(successful(HttpResponse(StatusCodes.OK)))

    tsProviderService.createProvider

    verify(httpRequestService).makeRequest(argumentCaptor.capture())
    val httpRequestParams: HttpRequest = argumentCaptor.getValue
    (httpRequestParams.method) shouldEqual HttpMethods.PUT
    (httpRequestParams.uri.path.toString()) shouldEqual "/south/v2/providers/d8fd9f7f-081b-4f37-a3b8-1650821b69f2"
    val result =
      "{\"id\":\"d8fd9f7f-081b-4f37-a3b8-1650821b69f2\",\"devicekind\":\"ts.device.light.sensity\",\"kind\":\"ts.provider\",\"version\":\"1.0\",\"name\":\"sensityProvider\",\"description\":\"sensity Provider hhh\",\"inventory\":{\"alias\":\"ts.device\",\"basePath\":\"/sensity/v1\",\"relativePath\":\"device\",\"hostAndPort\":\"http://198.23.5.50:8083\"},\"sink\":null,\"source\":null}"
    (httpRequestParams.entity.toString) shouldEqual s"HttpEntity.Strict(application/json,$result)"
  }
}
