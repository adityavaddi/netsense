package com.verizon.netsense.services.utils

import com.verizon.netsense.services.data.TestData
import org.velvia.MsgPackUtils

/**
 * Created by subrsi9 on 7/13/17.
 */
class MsgPackUtilsSpec extends BaseSpec with TestData {

  //Note: Please refer the TestData for test payloads

  //SCENARIO: Input has nested map with root level serialization
  it should "Recursively Unpack the payload message to Map object" in {
    val unPackedMap = MsgPackUtils.unpackMap(loginReqPayloadJsonByte.asInstanceOf[Array[Byte]])
    unPackedMap mustBe loginReqPayloadMap

  }

  //SCENARIO: Input has nested map with root level serialization and child level serialization
  //ISSUE: The nested map should be done only at the root level not child level. But now it is twice.
  it should "Unpack the Root payload and then child map by given key" in {

    val payLoadMapwithChildMap = MsgPackUtils.unpackMap(jsonPaylodwithChildByte)

    val unPackedMap = MsgPackUtils.unpackMap(payLoadMapwithChildMap("l").asInstanceOf[Array[Byte]])

    unPackedMap mustBe childPayloadMap
  }

}
