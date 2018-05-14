package com.verizon.netsense.entity

import com.fasterxml.jackson.annotation.JsonProperty
/**
  * Created by davor on 5/18/17.
  */

trait ISPayload

trait ISResponsePayload extends Entity {
  val requestid: String
  val success: Boolean
  val timestamp: String
}


trait ISResponseMessage  extends Entity {
  val messageid: String
}

case class ISMessageError(messageid: String, @JsonProperty response: ISResponseError) extends ISResponseMessage
case class ISMessageSuccess(messageid: String, @JsonProperty response: ISResponseSuccess) extends ISResponseMessage



case class ISResponseError(requestid: String,
                           success: Boolean,
                           timestamp: String,
                           error: String,
                           status: Int) extends ISResponsePayload


case class ISResponseSuccess(requestid: String,
                             success: Boolean,
                             timestamp: String,
                             message: String) extends ISResponsePayload
