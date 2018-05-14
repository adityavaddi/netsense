package com.verizon.netsense.entity

import com.verizon.netsense.utils.Deserializer

/**
 * Created by davor on 3/22/2017.
 */
trait Entity {
  def toMsgPack: Array[Byte] = Deserializer.msgpackMapper.writeValueAsBytes(this)
  def toJSON: String         = Deserializer.jsonMapper.writeValueAsString(this)
  def toJsonArray: Array[Byte] = Deserializer.jsonMapper.writeValueAsBytes(this)
}
