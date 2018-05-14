package com.verizon.netsense.utils

import com.verizon.netsense.constants.SensorSampleConstants._
import com.verizon.netsense.model.{SensorPayload, SensorSampleEvent}

import scala.collection.mutable.ListBuffer

/**
  * Created by maidapr on 6/8/17.
  */
object SensorValueConverter {

  /**
    * helper to convert the encoded sensor values
    *
    * @param ss    -  SensorSampleEvent
    * @return      -  List of SensorSampleEvent after decoding
    */
  def convertSensorValues(ss: SensorSampleEvent): List[SensorSampleEvent] = ss.l.s match {
    case `jt` => combinedValuesConverter(bitOperateJtValues(ss.l), ss)
    case `rf` => combinedValuesConverter(bitOperateRfValues(ss.l), ss)
    case `bR` => combinedValuesConverter(calculateBrValues(ss.l), ss)
    case _    => combinedValuesConverter(List(ss.l), ss)
  }

  def constructRestPathForEventPersist(nodeId: String = "unspecified", sensorType: String = "unspecified"): String =
    s"v1/$nodeId/out/UNSOL/sensor/$sensorType"

  def constructRestPathForDeviceRequest(nodeId: String = "unspecified", sensorType: String = "unspecified"): String =
    s"v1/$nodeId/in/GET/sensors/$sensorType"


  private def convertToActualValues(sP: SensorPayload): SensorPayload =
    sP.s match {
      case "l" | "l-i" | "lIR" | "T" | "v" | "vp" | "mi" | "mip" | "i" | "ai" | "aip" | "mP" | "aP" =>
        sP.copy(v = sP.v / 1000)
      case "w" | "mw" | "aw"             => sP.copy(v = sP.v / (1000 * 3600))
      case "PF" | "mPF" | "aPF"          => sP.copy(v = sP.v / pow2_30)
      case "jtx" | "jty" | "jtz"         => sP.copy(v = sP.v)
      case "jtm"                         => sP.copy(v = sP.v)
      case "t"                           => sP.copy(v = sP.v * 0.01007203 - 40)
      case _                             => sP
    }

  private def combinedValuesConverter(spL: List[SensorPayload], event: SensorSampleEvent): List[SensorSampleEvent] =
    spL.foldLeft(List[SensorSampleEvent]()) { (a, c) =>
      a.:+(
        event.copy(f = c.s, p = constructRestPathForEventPersist(event.sid, c.s), l = convertToActualValues(c))
      )
    }

  private def calculateJoltValue(nth: Int, sP: SensorPayload): Long =
    (sP.v.toLong >>> 16 * nth).toShort.toLong

  private def bitOperateJtValues(sP: SensorPayload): List[SensorPayload] = {
    val jtxValue = (sP.v.toLong & 0xFFFF).toShort.toLong
    val jtyValue = calculateJoltValue(1, sP)
    val jtzValue = calculateJoltValue(2, sP)
    val jtmValue = calculateJoltValue(3, sP)
    val jtList   = new ListBuffer[SensorPayload]()
    jtList += sP.copy(s = jtx, v = jtxValue)
    jtList += sP.copy(s = jty, v = jtyValue)
    jtList += sP.copy(s = jtz, v = jtzValue)
    jtList += sP.copy(s = jtm, v = jtmValue)
    jtList.toList
  }

  private def bitOperateRfValues(sP: SensorPayload): List[SensorPayload] = {
    val RFValue = sP.copy(s = RF, v = (sP.v.toLong & 0xFFFF).toShort.toLong)
    val snValue = sP.copy(s = sn, v = ((sP.v.toLong >> 16) & 0xFFFF).toShort.toLong)
    List(RFValue, snValue)
  }

  private def calculateBrValues(sP: SensorPayload): List[SensorPayload] = {
    val bRLValue = sP.copy(s = bRL, v = (sP.v % 256).toInt)
    val bRAValue = sP.copy(s = bRA, v = (sP.v / 256).toInt)
    List(bRLValue, bRAValue)
  }

  private lazy val pow2_30: Double = Math.pow(2, 30)
  private lazy val pow2_14: Double = Math.pow(2, 14)

}
