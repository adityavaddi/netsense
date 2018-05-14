package com.verizon.netsense.entity

import java.util.{Date, UUID}
import com.fasterxml.jackson.annotation.JsonProperty
import org.joda.time.{DateTime, DateTimeZone}

case class MQTTReq(a: String,
                   p: String,
                   sid: String = "OTAService",
                   f: String = "",
                   d: String = new DateTime(new Date()).withZone(DateTimeZone.UTC).toString(),
                   uuid: String = UUID.randomUUID().toString,
                   l: Array[Byte]) extends Entity

case class MQTTRes(a: String,
                   p: String,
                   sid: String,
                   e: String,
                   d: String,
                   s: Int,
                   uuid: String,
                   @JsonProperty l: Map[String, Any]) extends Entity


object MQTTMessages extends Constants {
  def extractNodeIDFromTopic(topic: String): String = topic.split("/")(1)

  def isFinalStatus(status: String): Boolean = {
    status match {
      case INSTALL_SUCCESSFUL => true
      case INSTALL_FAILED => true
      case INSTALL_INVALID => true
      case INSTALL_ERROR => true
      case DOWNLOAD_FAILED => true
      case DOWNLOAD_ERROR => true
      case DOWNLOAD_INVALID => true
      case FW_NOT_FOUND => true
      case _ => false
    }
  }
}