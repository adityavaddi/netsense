package com.verizon.netsense.Utils

import com.verizon.netsense.utils.{Z85, Z85Mapper}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by brefsdal on 4/12/17.
 */
class Z85MapperSpec extends FlatSpec with Matchers {

  val helloWorldBytes: Array[Byte] =
    Array[Byte](0x86.toByte, 0x4F.toByte, 0xD2.toByte, 0x6F.toByte, 0xB5.toByte, 0x59.toByte, 0xF7.toByte, 0x5B.toByte)

  val helloWorld = "HelloWorld"

  "Z85Mapper" should "pass the reference implementation encode test" in {
    Z85.z85Encode(helloWorldBytes, helloWorldBytes.length) should be(helloWorld)
  }

  "Z85Mapper" should "pass the reference implementation decode test" in {
    Z85.z85Decode(helloWorld) should be(helloWorldBytes)
  }

  "Z85Mapper" should "decode Z85 strings" in {
    val z85Str = "?$zwO:jAUtI7Fkd2#LH["
    val expected: Array[Byte] = Array[Byte](
      0xDF.toByte,
      0xF6.toByte,
      0x7A.toByte,
      0x8B.toByte,
      0xC7.toByte,
      0xD7.toByte,
      0x42.toByte,
      0x90.toByte,
      0x89.toByte,
      0x2C.toByte,
      0xF2.toByte,
      0xB9.toByte,
      0x9.toByte,
      0x51.toByte,
      0x6C.toByte,
      0x31.toByte
    )
    Z85.z85Decode(z85Str) should be(expected)

    val expectedUuid = "dff67a8b-c7d7-4290-892c-f2b909516c31"
    Z85Mapper.decode(z85Str) should be(expectedUuid)
  }

  "Z85Mapper" should "encode X85 strings" in {
    val uuid     = "dff67a8b-c7d7-4290-892c-f2b909516c31"
    val expected = "?$zwO:jAUtI7Fkd2#LH["
    Z85Mapper.encode(uuid) should be(expected)
  }

}
