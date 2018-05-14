package com.verizon.netsense.entity

import com.fasterxml.jackson.annotation.JsonProperty
import com.verizon.netsense.utils.Deserializer

/**
 * Created by brefsdal on 3/8/17.
 */
case class LoginReply(t: Long) extends Entity

object LoginReply {

  def toMsgPack(lr: LoginReply): Array[Byte] =
    Deserializer.msgpackMapper.writeValueAsBytes(lr)

  def fromMsgPack(msg: Array[Byte]): LoginReply = Deserializer.msgpackMapper.readValue(msg, classOf[LoginReply])

  def toJson(lr: LoginReply): String = Deserializer.jsonMapper.writeValueAsString(lr)

  def fromJson(msg: Array[Byte]): LoginReply = Deserializer.jsonMapper.readValue(msg, classOf[LoginReply])
}
