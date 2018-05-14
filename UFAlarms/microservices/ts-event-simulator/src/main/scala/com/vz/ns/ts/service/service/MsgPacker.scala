package com.vz.ns.ts.service.service

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import org.msgpack.jackson.dataformat.MessagePackFactory

import scala.concurrent.{ExecutionContext, Future}

/**
 * Created by lakssr6
 */
object MsgPacker {

  val mapper = new ObjectMapper(new MessagePackFactory)
  mapper.registerModule(DefaultScalaModule)

  def packerAsync(msg: AnyRef)(implicit ec: ExecutionContext): Future[Array[Byte]] = Future {

    mapper.writeValueAsBytes(msg)

  }
}
