package com.verizon.netsense.helper

import com.verizon.netsense.config.TestSuiteConfig
import com.verizon.netsense.data.TestData
import com.verizon.netsense.model._
import com.verizon.netsense.util.BaseSpec
import com.verizon.netsense.utils.{SensorValueConverter, TimeConverter}

/**
  * Created by nalamte on 9/25/17.
  */

class CommonSpec extends BaseSpec
                          with Common
                          with TestSuiteConfig
                          with TestData
                          with TimeConverter {

  "CommonSpec" should "Convert CASEL to MsgPack for Code Node" in {

   val actual=  convertCASELtoMsgPackForCoreNode(requestQueryWithHeaders.request.nodeprops.nodeid,
                                                 requestQueryWithHeaders.request.extprops.sensorid)
   val expected: SensorSampleCoreRequest = SensorSampleCoreRequest(Array.apply(nodeId),"SensorSampleReq", sensorId)

   actual.asInstanceOf[SensorSampleCoreRequest].sensor mustBe expected.sensor
   actual.asInstanceOf[SensorSampleCoreRequest].nodeid(0) mustBe expected.nodeid(0)
  }

  it should "Convert CASEL to RESTPACK for video " in {
     val actual = convertCASELtoRestPackForVideoNode(requestQueryWithHeaders.request.requestid,
                                                     requestQueryWithHeaders.request.nodeprops.nodeid,
                                                     requestQueryWithHeaders.request.timestamp,
                                                     requestQueryWithHeaders.request.extprops.sensorid)
     val expected = SensorSampleRequest(requestId,
                      nodeId, "GET",
                      sensorId,
                      SensorValueConverter.constructRestPathForDeviceRequest(nodeId, sensorId),
                      requestQueryWithHeaders.request.timestamp,
                      SensorSampleProps(sensorId,
                      convertISOToMicros(requestQueryWithHeaders.request.timestamp)).toMsgPack)
    val sensorSampleRequest = actual.asInstanceOf[SensorSampleRequest]
    sensorSampleRequest.p mustBe expected.p
    sensorSampleRequest.sid mustBe expected.sid
  }
}
