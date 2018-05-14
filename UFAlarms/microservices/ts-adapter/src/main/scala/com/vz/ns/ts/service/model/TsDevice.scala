package com.vz.ns.ts.service.model

import spray.json.DefaultJsonProtocol

case class TsDevice(id: Option[String],
                    kind: Option[String],
                    var version: Option[String],
                    var versionid: Option[String],
                    createdon: Option[String],
                    lastupdated: Option[String],
                    refid: Option[String],
                    name: Option[String],
                    description: Option[String],
                    foreignid: Option[String],
                    providerid: Option[String],
                    qrcode: Option[String],
                    var state: Option[String],
                    tagids: Option[Array[String]])

object ServiceJsonProtocol extends DefaultJsonProtocol {
  implicit val customerProtocol = jsonFormat14(TsDevice)
}
