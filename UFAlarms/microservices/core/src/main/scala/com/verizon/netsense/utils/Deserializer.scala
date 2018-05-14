package com.verizon.netsense.utils

import com.fasterxml.jackson.annotation.JsonInclude.Include
import com.fasterxml.jackson.core.JsonFactory
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import org.msgpack.jackson.dataformat.MessagePackFactory

import scala.collection.immutable.HashMap

object Deserializer {

  val baseMapper = new ObjectMapper()
  baseMapper.registerModule(DefaultScalaModule)

  val msgpackMapper = new ObjectMapper(new MessagePackFactory)

  msgpackMapper.registerModule(DefaultScalaModule)

  def unpack(msg: IndexedSeq[Byte]): Unit = {
    val preamble = msgpackMapper.readValue(msg.toArray, classOf[HashMap[String, Object]])
  }

  val jsonMapper = new ObjectMapper(new JsonFactory)
  jsonMapper.setSerializationInclusion(Include.NON_ABSENT)

  jsonMapper.registerModule(DefaultScalaModule)

  def unpackJson(msg: IndexedSeq[Byte]): HashMap[String, Any] =
    jsonMapper.readValue(msg.toArray, classOf[HashMap[String, Any]])

}
