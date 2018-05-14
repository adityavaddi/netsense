package com.vz.ns.ts.service.util

import akka.http.scaladsl.HttpExt
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}

import scala.concurrent.Future

class HttpRequestService(httpExt: HttpExt, _akkaRuntime: AkkaRuntime) {

  import _akkaRuntime._

  def makeRequest(httpRequest: HttpRequest): Future[HttpResponse] =
    httpExt.singleRequest(httpRequest)
}

object HttpRequestService {
  def apply(httpExt: HttpExt, akkaRuntime: AkkaRuntime): HttpRequestService =
    new HttpRequestService(httpExt, akkaRuntime)
}
