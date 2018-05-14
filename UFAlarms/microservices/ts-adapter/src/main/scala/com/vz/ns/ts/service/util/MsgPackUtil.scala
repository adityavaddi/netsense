package com.vz.ns.ts.service.util

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import org.msgpack.jackson.dataformat.MessagePackFactory

import scala.concurrent.{ExecutionContext, Future}

object MsgPackUtil {

  val mapper = new ObjectMapper(new MessagePackFactory).registerModule(DefaultScalaModule)

  def unPacker(msg: Array[Byte])(implicit ec: ExecutionContext) = Future {
    Option(mapper.readValue(msg, classOf[Object]))
  }
}
