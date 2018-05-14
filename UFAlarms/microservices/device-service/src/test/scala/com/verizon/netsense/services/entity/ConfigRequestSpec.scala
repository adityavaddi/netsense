package com.verizon.netsense.services.entity

import com.verizon.netsense.entity.ConfigRequest
import org.scalatest.{FlatSpec, Matchers}

class ConfigRequestSpec extends FlatSpec with Matchers  {
  val request = ConfigRequest("N02c01089", "12345", "falcon-q")

  "ConfigRequest" should "encode and decode msgpack" in {

    val decoded = ConfigRequest.toMsgPack(request)

    val encoded = ConfigRequest.fromMsgPack(decoded)

    encoded.n shouldBe(request.n)
    encoded.t shouldBe(request.t)
  }

  "it" should "encode and decode json" in {

    val decoded = ConfigRequest.toJson(request)

    val encoded = ConfigRequest.fromJson(decoded)

    encoded.n shouldBe(request.n)
    encoded.t shouldBe(request.t)
  }

}
