package com.vz.ns.ts.service.model

import spray.json.DefaultJsonProtocol

case class TsAccount(id: Option[String],
                     kind: Option[String],
                     version: Option[String],
                     versionid: Option[String],
                     createdon: Option[String],
                     lastupdated: Option[String])

object TsAccountJsonProtocol extends DefaultJsonProtocol {
  implicit val accountProtocol = jsonFormat6(TsAccount)
}
