package com.verizon.netsense.sse.specs.helper

import com.verizon.netsense.constants.SensorSampleConstants._
import com.verizon.netsense.model.{SensorPayload, SensorSampleEvent}
import com.verizon.netsense.sse.specs.data.TestData
import com.verizon.netsense.utils.SensorValueConverter
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

/**
 * Created by muppasw on 9/14/17.
 */
class SensorValueConvertorSpec
    extends FlatSpecLike
    with TestData
    with MustMatchers
    with Inspectors
    with ScalaFutures
    with OptionValues
    with BeforeAndAfterAll {

  private implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  "SensorSample" should "decode the jt values" in {

    lazy val jtSensorType = jt
    lazy val jtValue      = BigDecimal(4684316664362221708L)

    val expectedDecodedValues = List(
      sensorSampleEventDecoded(jtx, -16244.0),
      sensorSampleEventDecoded(jty, 2752.0),
      sensorSampleEventDecoded(jtz, 2352.0),
      sensorSampleEventDecoded(jtm, 16642.0)
    )
    println("ExpectedDecodedValue-->" + expectedDecodedValues)
    val simulatedEvent =
      SensorSampleEvent(uuid1,
                        sid,
                        a1,
                        f,
                        p,
                        l = SensorPayload(n = nodeId, u = sensorUnits, s = jtSensorType, v = jtValue, t = t))
    val decodedSensorValues = SensorValueConverter.convertSensorValues(simulatedEvent)
    println("DecodedSensorValues-->" + decodedSensorValues)
    decodedSensorValues mustBe expectedDecodedValues

  }

  "SensorSamplePayload" should "decode the l values" in {

    val sensorType            = "l"
    val sensorValue           = 123.123
    val expectedDecodedValues = List(sensorSampleEventDecoded(sensorType, 0.123123))
    val simulatedEvent =
      SensorSampleEvent(uuid1,
                        sid,
                        a1,
                        f,
                        p,
                        l = SensorPayload(n = nodeId, u = sensorUnits, s = sensorType, v = sensorValue, t = t))
    val decodedSensorValues = SensorValueConverter.convertSensorValues(simulatedEvent)
    decodedSensorValues mustBe expectedDecodedValues
  }

}
