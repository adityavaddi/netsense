package com.vz.ns.ts.service.util

class MsgPackUtilTest extends BaseSpec {

  val msgPackUtil = MsgPackUtil

  "ObjectMapper" should "Create as MessagePackFactory" in {
    msgPackUtil.mapper mustNot equal(null)
  }
}
