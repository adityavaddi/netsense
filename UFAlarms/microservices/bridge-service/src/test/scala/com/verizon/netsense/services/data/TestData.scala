package com.verizon.netsense.services.data

import com.verizon.netsense.services.bridge.utils.JSONUtils
import org.velvia.{MsgPack}

/**
 * Created by subrsi9 on 7/13/17.
 */
trait TestData {

  /**
   * Login Request payload JSON string (which has nested object (map)
   */
  val loginReqPayloadJson: String =
    "{\"uuid\":\"c2057c82-36a8-4df9-8172-a850ccdb01f4\", \"f\":\"\" , \"a\":\"POST\",\"l\": {\"mac\":\"7e:40:5c:81:55:b2\", \"ip\":\"192.168.66.145\",\"t\":\"1499277104341141861\",\"ssid\":\"SensitySim\",\"bssid\":\"ca:51:ca:af:47:0a\", \"ch\":\"120\", \"sec\":\"WPA2_PSK\", \"cid\":\"a3441a938\",\"dev\":\"unode-v7\",\"tok\":\"9a5f862456\",\"nid\": \"JSV7_4\"},\"p\":\"v1/JSV7_4/out/POST/loginReq\",\"h\":\"localhost\"}"

  /**
   * Sensor sample paylaod JSON
   */
  val sensorSamplePayload: String =
    "{\"uuid\": \"fd519636-8ccf-4e0c-85a7-0c95e508d2b5\",\"a\": \"UNSOL\",\"p\": \"/all/out/sensor/falcon\",\"?u\": \"true\", \"l\":{\"n\":\"N12121\",\"m\":\"SensorSample\",\"s\":\"pc\",\"t\": \"1495572600\",\"u\": 2,\"v\": \"100.1\"}}"

  val sensorSampleMap: Map[Any, Any] = Map(
    "uuid" -> "fd519636-8ccf-4e0c-85a7-0c95e508d2b5",
    "a"    -> "UNSOL",
    "l"    -> Map("s" -> "pc", "n" -> "N12121", "t" -> "1495572600", "u" -> 2, "m" -> "SensorSample", "v" -> "100.1"),
    "p"    -> "/all/out/sensor/falcon",
    "?u"   -> "true"
  )

  /**
   * Inner (child) map of login payload
   */
  val childPayloadMap: Map[Any, Any] = Map(
    "mac"   -> "7e:40:5c:81:55:b2",
    "ip"    -> "192.168.66.145",
    "t"     -> "1499277104341141861",
    "ssid"  -> "SensitySim",
    "bssid" -> "ca:51:ca:af:47:0a",
    "ch"    -> "120",
    "sec"   -> "WPA2_PSK",
    "cid"   -> "a3441a938",
    "dev"   -> "unode-v7",
    "tok"   -> "9a5f862456",
    "nid"   -> "JSV7_4"
  )

  /**
   * Root map of login payload
   */
  val loginReqPayloadMap: Map[Any, Any] = Map("uuid" -> "c2057c82-36a8-4df9-8172-a850ccdb01f4",
                                              "f" -> "",
                                              "a" -> "POST",
                                              "l" -> childPayloadMap,
                                              "p" -> "v1/JSV7_4/out/POST/loginReq",
                                              "h" -> "localhost")

  val loginReqPayloadJsonByte = JSONUtils.json2bytes(loginReqPayloadJson)

  /**
   * create child json payload of login payload
   */
  val childMapJson: String =
    "{\"mac\":\"7e:40:5c:81:55:b2\", \"ip\":\"192.168.66.145\",\"t\":\"1499277104341141861\",\"ssid\":\"SensitySim\",\"bssid\":\"ca:51:ca:af:47:0a\", \"ch\":\"120\", \"sec\":\"WPA2_PSK\", \"cid\":\"a3441a938\",\"dev\":\"unode-v7\",\"tok\":\"9a5f862456\",\"nid\": \"JSV7_4\"}"

  val childMapPack = JSONUtils.json2bytes(childMapJson)

  /**
   * Payload map with child map as byte string
   */
  val payLoadMapwithChildMap: Map[Any, Any] = Map("uuid" -> "c2057c82-36a8-4df9-8172-a850ccdb01f4",
                                                  "f" -> "",
                                                  "a" -> "POST",
                                                  "l" -> childMapPack,
                                                  "p" -> "v1/JSV7_4/out/POST/loginReq",
                                                  "h" -> "localhost")

  val jsonPaylodwithChildByte = MsgPack.pack(payLoadMapwithChildMap)

}
