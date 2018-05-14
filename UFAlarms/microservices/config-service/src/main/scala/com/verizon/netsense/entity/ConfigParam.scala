package com.verizon.netsense.entity

import com.fasterxml.jackson.annotation.JsonProperty
import com.verizon.netsense.utils.Deserializer

/**
 * Created by davor on 3/22/2017.
 */
case class ConfigParam(@JsonProperty("db") db: String, @JsonProperty("k") k: String, @JsonProperty("v") v: Array[Byte])
    extends Entity

object ConfigParam {
  def toMsgPack(cp: ConfigParam): Array[Byte] =
    Deserializer.msgpackMapper.writeValueAsBytes(cp)

  def fromMsgPack(msg: Array[Byte]): ConfigParam = {
    val hashmap = Deserializer.msgpackMapper.readValue(msg, classOf[Map[String, Object]])
    ConfigParam(
      hashmap("db").toString,
      hashmap("k").toString,
      hashmap("v").asInstanceOf[Array[Byte]]
    )
  }

  def fromJSON(msg: String): ConfigParam =
    Deserializer.jsonMapper.readValue(msg, classOf[ConfigParam])
}

case class DeviceConfig(db: String, t: String, l: Array[Byte]) extends Entity

object DeviceConfig {
  def toMsgPack(c: DeviceConfig): Array[Byte] = Deserializer.msgpackMapper.writeValueAsBytes(c)
  def fromMsgPack(msg: Array[Byte]): DeviceConfig = {
    val hashMap = Deserializer.msgpackMapper.readValue(msg, classOf[Map[String, Object]])

    DeviceConfig(
      hashMap("db").toString,
      hashMap("t").toString,
      hashMap("l").asInstanceOf[Array[Byte]]
    )
  }
  def fromJSON(msg: String): DeviceConfig = Deserializer.jsonMapper.readValue(msg, classOf[DeviceConfig])
}

case class ConfigBulkKVP(db: String, t: String, l: Array[Byte]) extends Entity

object ConfigBulkKVP {
  def toMsgPack(c: ConfigBulkKVP): Array[Byte] = Deserializer.msgpackMapper.writeValueAsBytes(c)

  def fromMsgPack(msg: Array[Byte]): ConfigBulkKVP =
    Deserializer.msgpackMapper.readValue(msg, classOf[ConfigBulkKVP])
  def fromJSON(msg: String): ConfigBulkKVP =
    Deserializer.jsonMapper.readValue(msg, classOf[ConfigBulkKVP])
}

//case class ConfigKVP(t: String, db: String, k: String, v: String)
//
//object ConfigKVP {
//  def fromMsgPack(msg: Array[Byte]): ConfigKVP =
//    Deserializer.msgpackMapper.readValue(msg, classOf[ConfigKVP])
//  def fromJSON(msg: String): ConfigKVP =
//    Deserializer.jsonMapper.readValue(msg, classOf[ConfigKVP])
//}

case class SingleParamReq(k: String, db: String) extends Entity

object SingleParamReq {
  def toMsgPack(c: SingleParamReq): Array[Byte] = Deserializer.msgpackMapper.writeValueAsBytes(c)
  def fromMsgPack(data: Array[Byte]): SingleParamReq =
    Deserializer.msgpackMapper.readValue(data, classOf[SingleParamReq])
}

case class ConfigListReq(db: String) extends Entity

object ConfigListReq {
  def fromMsgPack(data: Array[Byte]): ConfigListReq =
    Deserializer.msgpackMapper.readValue(data, classOf[ConfigListReq])
}
