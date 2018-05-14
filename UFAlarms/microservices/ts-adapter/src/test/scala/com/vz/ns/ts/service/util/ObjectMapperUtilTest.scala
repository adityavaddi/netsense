package com.vz.ns.ts.service.util

/**
 * Test class for testing the ObjectMapperUtil object
 */
class ObjectMapperUtilTest extends BaseSpec {

  val objMapUtil        = ObjectMapperUtil
  val inputMap          = Map(("x", 24), ("y", 25), ("z", 26))
  val jsonOutput        = "{\"x\" : 24, \"y\" : 25, \"z\" : 26}"
  val invalidJsonOutput = "{\"x\" : 24, y\" : 25, \"z\" : 26}"

  "any value" should "convert to string" in {
    objMapUtil.toJson(12) === "12"
  }

  "map" should "convert to json" in {
    objMapUtil.toJson(inputMap) === jsonOutput
  }

  "map" should "fail while converting to json" in {
    objMapUtil.toJson(inputMap) != invalidJsonOutput
  }
}
