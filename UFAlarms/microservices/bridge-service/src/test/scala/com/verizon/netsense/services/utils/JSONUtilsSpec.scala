package com.verizon.netsense.services.utils

import com.verizon.netsense.services.bridge.utils.JSONUtils
import com.verizon.netsense.services.data.TestData
import org.velvia.MsgPackUtils

/**
 * Created by thimija on 7/5/17.
 */
class JSONUtilsSpec extends BaseSpec with TestData {

  /**
   * Test JSON string to Bytes (Packing the message)
   */
  it should "Convert JSON String to Message Pack" in {

    val loginReqPayloadJsonBytes = JSONUtils.json2bytes(loginReqPayloadJson)
    val unPackedMap              = MsgPackUtils.unpackMap(loginReqPayloadJsonByte.asInstanceOf[Array[Byte]])
    unPackedMap mustBe loginReqPayloadMap

  }
}
