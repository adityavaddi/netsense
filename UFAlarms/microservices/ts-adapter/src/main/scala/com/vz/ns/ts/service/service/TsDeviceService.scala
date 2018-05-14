package com.vz.ns.ts.service.service

import akka.http.scaladsl.model.{ContentTypes, HttpMethods, HttpRequest, HttpResponse}
import akka.http.scaladsl.unmarshalling.Unmarshal
import com.verizon.netsense.metrics.Instrumented
import com.vz.ns.ts.service.config.ConfigLoader._
import com.vz.ns.ts.service.model.{TsDevice, TsDeviceInfo}
import com.vz.ns.ts.service.util.{AkkaRuntime, HttpRequestService, ObjectMapperUtil}
import de.heikoseeberger.akkahttpjackson.JacksonSupport._
import nl.grons.metrics.scala.Timer

import scala.concurrent.Future

class TsDeviceService(httpRequestService: HttpRequestService, _akkaRuntime: AkkaRuntime)
    extends Logging
    with Instrumented {

  import _akkaRuntime._

  private[this] val RegisteredDevicesCount: Timer = metrics.timer("RegisteredDevices-Count")

  def createDevice(device: TsDeviceInfo): Future[TsDevice] = {
    log.debug("Entering createDevice")
    RegisteredDevicesCount.time()
    val response: Future[HttpResponse] = httpRequestService.makeRequest(
      HttpRequest(method = HttpMethods.POST, uri = s"${tsSouthApi}/south/v2/devices")
        .withEntity(ContentTypes.`application/json`, ObjectMapperUtil.toJson(device))
    )
    response.foreach(r => log.info("response after creating device " + r.status))
    response.flatMap(x => (Unmarshal(x.entity).to[TsDevice]))
  }

  def getTsDeviceByRefId(refId: String): Future[Option[String]] = {
    log.debug("Entering getTsDeviceByRefId")
    log.debug("refid: " + refId)

    val data = Map(
      "$filter" -> Map(
        "refid" -> refId
      )
    )

    val response: Future[HttpResponse] = httpRequestService.makeRequest(
      HttpRequest(method = HttpMethods.POST, uri = s"${tsSouthApi}/south/v2/devices/actions/$queryForFilter")
        .withEntity(ContentTypes.`application/json`, ObjectMapperUtil.toJson(data))
    )

    val map: Future[Option[String]] = response.flatMap(x => {
      val tsDevices = Unmarshal(x.entity).to[Array[TsDevice]]
      tsDevices.map(x => {
        if (x.length == 0) None
        else Some(x(0).id.get)
      })
    })
    map
  }
}

object TsDeviceService {
  def apply(httpRequestService: HttpRequestService, _akkaRuntime: AkkaRuntime): TsDeviceService =
    new TsDeviceService(httpRequestService, _akkaRuntime)
}
