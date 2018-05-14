package com.vz.ns.ts.service.model

import spray.json.DefaultJsonProtocol

case class TsDeviceInfo(kind: Option[String],
                        version: Option[String],
                        name: Option[String],
                        providerid: Option[String],
                        refid: Option[String],
                        qrcode: Option[String],
                        state: String,
                        foreignid: String)

object TsDeviceJsonProtocol extends DefaultJsonProtocol {
  implicit val deviceProtocol = jsonFormat8(TsDeviceInfo)
}
