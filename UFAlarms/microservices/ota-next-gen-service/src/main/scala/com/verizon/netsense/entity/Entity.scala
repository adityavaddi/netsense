package com.verizon.netsense.entity

import com.verizon.netsense.utils.Deserializer

trait Entity {
  def toJSON: String = Deserializer.jsonMapper.writeValueAsString(this)
  def toJSONArray: Array[Byte] = Deserializer.jsonMapper.writeValueAsBytes(this)
  def toMsgPack: Array[Byte] = Deserializer.msgpackMapper.writeValueAsBytes(this)
}
