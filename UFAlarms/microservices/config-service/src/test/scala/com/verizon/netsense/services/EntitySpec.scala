package com.verizon.netsense.services

import java.util.Base64

import com.typesafe.config.ConfigList
import com.verizon.netsense.entity.{ConfigListReq, ConfigParam, DeviceConfig}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by davor on 4/12/17.
 */
class EntitySpec extends FlatSpec with Matchers {

  val configParam = ConfigParam("prov", "server", "server.sensity.com".getBytes)

  val encodedConfigParam = "g6JkYqRwcm92oWumc2VydmVyoXbEEnNlcnZlci5zZW5zaXR5LmNvbQ=="

  val configParamBytes = Base64.getDecoder.decode(encodedConfigParam)

  val falconConfig = """{  "network.wlan-x.security.psk": "netsense",
                           "vaevent.bucket_interval": "1",
                           "network.nowifi": "false"
                        }""";

  val configList = DeviceConfig("prov", "", falconConfig.getBytes)

  val configListBytes = configList.toMsgPack
  val configListReq   = ConfigListReq("prov")

  val encodedConfigListReq = "gaJkYqRwcm92"

  val configListReqBytes = Base64.getDecoder.decode(encodedConfigListReq)

  "EntitySpec" should "decode msgpack" ignore {
    ConfigParam.fromMsgPack(configParamBytes).db should be(configParam.db)
    ConfigParam.fromMsgPack(configParamBytes).v should be(configParam.v)
    ConfigParam.fromMsgPack(configParamBytes).k should be(configParam.k)
    DeviceConfig.fromMsgPack(configListBytes).l should be(configList.l)
    DeviceConfig.fromMsgPack(configListBytes).db should be(configList.db)
    ConfigListReq.fromMsgPack(configListReqBytes) should be(configListReq)
  }
}
