package com.verizon.netsense.entity

/**
 * Created by davor on 7/11/17.
 */
trait ISResponseEvent

case class ISResponse(messageId: String, requestId: String, replyTo: String, model: String, body: String)
    extends ISResponseEvent
