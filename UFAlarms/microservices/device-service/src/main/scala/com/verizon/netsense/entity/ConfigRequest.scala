package com.verizon.netsense.entity

import com.verizon.netsense.utils.Deserializer

/**
 * Created by davor on 3/23/2017.
 */
trait ConfigPublisherEvent

case class ConfigRequest(n: String, t: String, m: String) extends Entity with ConfigPublisherEvent

object ConfigRequest {
  def fromMsgPack(data: Array[Byte]): ConfigRequest = {
    val hasmap = Deserializer.msgpackMapper.readValue(data, classOf[Map[String, Object]])
    ConfigRequest(hasmap("n").toString, hasmap("t").toString, hasmap("m").toString)
  }
  def toMsgPack(cr: ConfigRequest): Array[Byte] = Deserializer.msgpackMapper.writeValueAsBytes(cr)

  def fromJson(data: Array[Byte]): ConfigRequest = {
    val hasmap = Deserializer.jsonMapper.readValue(data, classOf[Map[String, Object]])
    ConfigRequest(hasmap("n").toString, hasmap("t").toString, hasmap("m").toString)
  }
  def toJson(cr: ConfigRequest): Array[Byte] = Deserializer.jsonMapper.writeValueAsBytes(cr)
}
