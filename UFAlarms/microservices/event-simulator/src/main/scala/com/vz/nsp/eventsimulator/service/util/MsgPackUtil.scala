package com.vz.nsp.eventsimulator.service.util

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule

import org.msgpack.jackson.dataformat.MessagePackFactory

import scala.concurrent.{ExecutionContext, Future}

/**
 * Created by subrsi9 on 4/10/17.
 * Packing and unpacking the messages
 */
object MsgPackUtil {

  val mapper = new ObjectMapper(new MessagePackFactory)
  mapper.registerModule(DefaultScalaModule)
  //Packing the message
  def packer(msg: AnyRef): Array[Byte] =
    mapper.writeValueAsBytes(msg)

  def fromBytes(x: Array[Byte]) = new String(x, "UTF-8")
}
