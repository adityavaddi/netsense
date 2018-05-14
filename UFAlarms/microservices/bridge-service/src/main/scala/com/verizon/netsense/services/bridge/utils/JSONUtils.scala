package com.verizon.netsense.services.bridge.utils

import msgpack4z.{JawnMsgpack, JawnUnpackOptions, MsgpackJavaPacker, MsgpackJavaUnpacker}

import scalaz.{-\/, \/-}

/**
 * Created by wswaney on 5/16/17.
 */
object JSONUtils {

  private val codec = JawnMsgpack.jValueCodec(JawnUnpackOptions.default)

  def json2bytes(jsonString: String): Array[Byte] = {
    val json = jawn.Parser.parseUnsafe[jawn.ast.JValue](jsonString)
    codec.toBytes(json, new MsgpackJavaPacker)
  }

  def msgPack2json(msg: Array[Byte]): Array[Byte] = {
    val unpacker = MsgpackJavaUnpacker.defaultUnpacker(msg)
    codec.unpackAndClose(unpacker) match {
      case \/-(a) => a.toString.getBytes
      case -\/(a) => a.toString.getBytes
    }
  }

}
