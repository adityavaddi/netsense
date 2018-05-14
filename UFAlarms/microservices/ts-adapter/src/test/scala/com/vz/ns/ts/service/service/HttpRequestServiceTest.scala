package com.vz.ns.ts.service.service

import akka.actor.ActorSystem
import akka.http.scaladsl.HttpExt
import akka.http.scaladsl.model._
import com.vz.ns.ts.service.util.{AkkaRuntime, HttpRequestService}
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FunSuite, Matchers}

import scala.concurrent.Future

class HttpRequestServiceTest extends FunSuite with Matchers with MockitoSugar {

  test("should create a provider") {
    val runtime            = AkkaRuntime(ActorSystem.create("ts-adapter-test"))
    val httpRequestService = HttpRequestService(mock[HttpExt], runtime)

    val response: Future[HttpResponse] = httpRequestService.makeRequest(
      HttpRequest(method = HttpMethods.POST, uri = "/test")
        .withEntity(ContentTypes.`application/json`, "{some response}")
    )
    response shouldEqual null
  }
}
