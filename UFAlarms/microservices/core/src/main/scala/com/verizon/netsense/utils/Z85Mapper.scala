package com.verizon.netsense.utils

import java.nio.ByteBuffer
import java.util.UUID

/**
 * Created by brefsdal on 4/12/17.
 */
object Z85Mapper {

  def encode(uuidStr: String): String = {

    //val components = uuidStr.split("-")
    //if (components.length != 5) throw new IllegalArgumentException("Invalid UUID string: " + uuidStr)
    //val hexArray = components.map(token => s"0x$token")

    //val uuidStr = "dff67a8b-c7d7-4290-892c-f2b909516c31"
    //uuidStr.replace("-","").grouped(2).map(token => s"0x$token")

    val buffer: Array[Byte] =
      uuidStr.replace("-", "").grouped(2).map(token => java.lang.Integer.parseInt(token, 16).toByte).toArray

//    val uuid = UUID.fromString(uuidStr)
//
//    val most = uuid.getMostSignificantBits
//    val least = uuid.getLeastSignificantBits
//
//    val buf = ByteBuffer.allocate(16)
//    buf.putLong(most)
//    buf.putLong(least)
//
//    val buffer = buf.array()
//
//    arr.foreach { byte =>
//      print(s"$byte, ")
//    }
//    println("")

//    val buffer:Array[Byte] = Array[Byte](
//      ((most >>> 48) & 0xFF).toByte,
//      ((most >>> 32) & 0xFF).toByte,
//      ((most >>> 16) & 0xFF).toByte,
//      (most & 0xFF).toByte,
//      ((least >>> 48) & 0xFF).toByte,
//      ((least >>> 32) & 0xFF).toByte,
//      ((least >>> 16) & 0xFF).toByte,
//      (least & 0xFF).toByte
//    )

//    val buffer:Array[Byte] = Array[Byte](
//      (most & 0xFF).toByte,
//      ((most >>> 16) & 0xFF).toByte,
//      ((most >>> 32) & 0xFF).toByte,
//      ((most >>> 48) & 0xFF).toByte,
//      (least & 0xFF).toByte,
//      ((least >>> 16) & 0xFF).toByte,
//      ((least >>> 32) & 0xFF).toByte,
//      ((least >>> 48) & 0xFF).toByte
//    )

//    buffer.foreach { byte =>
//      print(s"$byte, ")
//    }
//    println("")

    Z85.z85Encode(buffer, buffer.length)
    //Z85.z85Encode(buffer.array(), buffer.array().length)
  }

  def decode(str: String): String = {
    val decoded   = Z85.z85Decode(str)
    val hexString = decoded.map("%02x" format _).mkString
    hexString.substring(0, 8) + "-" +
    hexString.substring(8, 12) + "-" +
    hexString.substring(12, 16) + "-" +
    hexString.substring(16, 20) + "-" +
    hexString.substring(20, 32)
  }

}
