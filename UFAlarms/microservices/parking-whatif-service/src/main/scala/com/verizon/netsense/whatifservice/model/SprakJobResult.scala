package com.verizon.netsense.whatifservice.model

import spray.json.DefaultJsonProtocol

/**
  * Created by maleva on 3/17/18.
  */

case class SparkJobResult(id: Int, state:String,appId:Option[String], appInfo: AppInfo,log:Array[String])
case class AppInfo(driverLogUrl:Option[String],sparkUiUrl: Option[String])

object SparkJsonProtocol extends DefaultJsonProtocol {
  implicit val appInfoProtocal = jsonFormat2(AppInfo)
  implicit val sparkJobResultProtocal = jsonFormat5(SparkJobResult)
}
