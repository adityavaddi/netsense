package com.verizon.netsense.helper

import com.verizon.netsense.constants.SensorSampleConstants
import com.verizon.netsense.data.TestData
import com.verizon.netsense.model._
import com.verizon.netsense.constants.SensorSampleConstants._
import com.verizon.netsense.util.BaseSpec
import com.verizon.netsense.utils.SensorValueConverter

/**
 * Created by maidapr on 6/26/17.
 */
class SensorValueConverterSpec extends BaseSpec with TestData {

  "SensorValueConverter" should "decode bR and produce multiple values" in {

    val expectedDecodedValues = List(sensorSampleEventDecoded(bRL, 8.0), sensorSampleEventDecoded(bRA, 1.0))
    val simulatedEvent = SensorSampleEvent(uuid = requestId,
      sid = nodeId,
      a = SensorSampleConstants.UNSOL,
      f = bRSensorType,
      SensorValueConverter.constructRestPathForEventPersist(nodeId, sensorId),
      l = SensorPayload(n = nodeId, u = sensorUnits, s = bRSensorType, v = bRValue, t = starttimeInMicros))
    val decodedSensorValues = SensorValueConverter.convertSensorValues(simulatedEvent)
    decodedSensorValues mustBe expectedDecodedValues
  }

  it should "decode the jt values" in {

    val expectedDecodedValues = List(sensorSampleEventDecoded(jtx, -16244.0),
      sensorSampleEventDecoded(jty, 2752.0),
      sensorSampleEventDecoded(jtz, 2352.0),
      sensorSampleEventDecoded(jtm, 16642.0))
    val simulatedEvent = SensorSampleEvent(requestId,
                                           nodeId,
                                           SensorSampleConstants.UNSOL,
                                           jtSensorType,
                                           "v1/",
                                           l = SensorPayload(n = nodeId, u = sensorUnits, s = jtSensorType, v = jtValue, t = starttimeInMicros))
    val decodedSensorValues = SensorValueConverter.convertSensorValues(simulatedEvent)
    decodedSensorValues mustBe expectedDecodedValues
  }

  it should "decode the rf values" in {

    val expectedDecodedValues = List(sensorSampleEventDecoded(RF, -80.0), sensorSampleEventDecoded(sn, -100.0))
    val simulatedEvent = SensorSampleEvent(requestId,
                                           nodeId,
                                           SensorSampleConstants.UNSOL,
                                           rfSensorType,
                                           "v1/",
                                           l = SensorPayload(n = nodeId, u = sensorUnits, s = rfSensorType, v = rfValue, t = starttimeInMicros))
    val decodedSensorValues = SensorValueConverter.convertSensorValues(simulatedEvent)
    decodedSensorValues mustBe expectedDecodedValues
  }

  it should "decode the l values" in {

    val sensorType            = "l"
    val sensorValue           = 123.123
    val expectedDecodedValues = List(sensorSampleEventDecoded(sensorType, 0.123123))
    val simulatedEvent = SensorSampleEvent(requestId,
                                           nodeId,
                                           SensorSampleConstants.UNSOL,
                                           sensorType,
                                           "v1/",
                                           l = SensorPayload(n = nodeId, u = sensorUnits, s = sensorType, v = sensorValue, t = starttimeInMicros))
    val decodedSensorValues = SensorValueConverter.convertSensorValues(simulatedEvent)
    decodedSensorValues mustBe expectedDecodedValues
  }

}
