package com.verizon.netsense.utils

import msgpack4z._

import scalaz.{-\/, \/-}

/**
 * Created by wswaney on 5/16/17.
 */
object BaseJSONUtils {

  private val codec = JawnMsgpack.jValueCodec(JawnUnpackOptions.default)

  def json2bytes(jsonString: String): Array[Byte] = {
    val json = jawn.Parser.parseUnsafe[jawn.ast.JValue](jsonString)
    codec.toBytes(json, new MsgpackJavaPacker)
  }

  def msgPack2json(msg: Array[Byte]): String = {
    val unpacker: MsgUnpacker = MsgpackJavaUnpacker.defaultUnpacker(msg)
    codec.unpackAndClose(unpacker) match {
      case \/-(a) => a.toString
      case -\/(a) => a.toString
    }
  }

}
