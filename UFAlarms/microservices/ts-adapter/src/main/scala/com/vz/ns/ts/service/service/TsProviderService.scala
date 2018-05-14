package com.vz.ns.ts.service.service

import akka.http.scaladsl.model.{ContentTypes, HttpMethods, HttpRequest}
import com.vz.ns.ts.service.config.ConfigLoader._
import com.vz.ns.ts.service.util.{AkkaRuntime, HttpRequestService, ObjectMapperUtil}

class TsProviderService(httpRequestService: HttpRequestService, _akkaRuntime: AkkaRuntime) extends Logging {

  import _akkaRuntime._

  def createProvider: Unit = {
    log.info("Entering TsProviderUtil::createProvider")
    log.debug("PROVIDER URL: " + s"${tsSouthApi}/south/v2/providers/${providerInfo.id.get}")

    val response = httpRequestService.makeRequest(
      HttpRequest(method = HttpMethods.PUT, uri = s"${tsSouthApi}/south/v2/providers/${providerInfo.id.get}")
        .withEntity(ContentTypes.`application/json`, ObjectMapperUtil.toJson(providerInfo))
    )
    response.foreach(r => log.info("Create provider status: " + r.status))
  }
}

object TsProviderService {
  def apply(httpRequestService: HttpRequestService, _akkaRuntime: AkkaRuntime): TsProviderService =
    new TsProviderService(httpRequestService, _akkaRuntime)
}
