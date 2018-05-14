package com.verizon.netsense.services.entity

import com.verizon.netsense.entity._
import org.scalatest.{FlatSpec, Matchers}

class ConfigParamSpec  extends FlatSpec with Matchers  {

  val configKvp = ConfigBulkKVP("prov","123", "123".getBytes)
  val deviceConfig = DeviceConfig("prov","123", "123".getBytes)
  val singleParamReq = SingleParamReq("server", "prov")
  val configReq = ConfigRequest("N02c01089", "12345", "falcon-q")
  val configParamReq = ConfigParam("prov", "token", "123345".getBytes)

  "ConfigBulkKvp" should "write encode and decode message pack" in {
      val msgPack = ConfigBulkKVP.toMsgPack(configKvp)

    val decoded = ConfigBulkKVP.fromMsgPack(msgPack)

    decoded.t shouldBe(configKvp.t)
  }

  it should "encode and decode json" in {
    val json = configKvp.toJSON

    val decoded = ConfigBulkKVP.fromJSON(json)
    decoded.t shouldBe(configKvp.t)
  }

  "DeviceConfig" should "write encode and decode message pack" in {
    val msgPack = DeviceConfig.toMsgPack(deviceConfig)

    val decoded = DeviceConfig.fromMsgPack(msgPack)

    decoded.t shouldBe(configKvp.t)
  }

  it should "encode and decode json" in {
    val json = deviceConfig.toJSON

    val decoded = DeviceConfig.fromJSON(json)
    decoded.t shouldBe(configKvp.t)
  }

  "SingleParamReq" should "encode and decode msgpack" in {
    val paramReq = SingleParamReq.toMsgPack(singleParamReq)

    val decoded = SingleParamReq.fromMsgPack(paramReq)

    decoded.k shouldBe(singleParamReq.k)
    decoded.db shouldBe(singleParamReq.db)
  }

  "ConfigRequest" should "encode and decode msgpack" in {
    val paramReq = ConfigRequest.toMsgPack(configReq)

    val decoded = ConfigRequest.fromMsgPack(paramReq)

    decoded.n shouldBe(configReq.n)
    decoded.t shouldBe(configReq.t)
  }

  "ConfigParam" should "encode and decode msgpack" in {
    val paramReq = ConfigParam.toMsgPack(configParamReq)

    val decoded = ConfigParam.fromMsgPack(paramReq)

    decoded.db shouldBe(configParamReq.db)
    decoded.k shouldBe(configParamReq.k)
  }

}
