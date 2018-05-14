package com.vz.nsp.trigger.helper

import java.util.UUID

import com.vz.nsp.trigger.model.casel._
import com.verizon.netsense.utils.Logging

object CommonHelper extends Logging {

  def isEmpty(x: String): Boolean = x == null || x.trim.isEmpty

  def generateAppRequestForValidationError(msgid: String, responsetopic: String, errormsg: String): AppRequest =
    AppRequest(
      msgid,
      responsetopic,
      RequestBody(
        null,
        null,
        null,
        `type` = "validationfailed",
        model = null,
        null,
        None,
        null,
        null,
        null
      )
    )

  def generateUUID: String = UUID.randomUUID().toString

  def getCurrentTime:Long = System.currentTimeMillis()

}

