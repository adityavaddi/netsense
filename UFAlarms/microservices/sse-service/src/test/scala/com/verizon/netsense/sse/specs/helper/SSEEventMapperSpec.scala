package com.verizon.netsense.sse.specs.helper

import com.verizon.netsense.sse.model.{SSEEvent, SSELoginEvent, _}
import com.verizon.netsense.sse.specs.data.TestData
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, MustMatchers}

/**
 * Created by muppasw on 9/1/17.
 */
class SSEEventMapperSpec extends FlatSpecLike with MustMatchers with BeforeAndAfterAll with TestData {

  "Sensor SSEEvent" should "be wrapped as an instance of SensorSampleEvent" in {
    val a                           = sensorSampleEvent
    val event: SSESensorSampleEvent = SSESensorSampleEvent.apply(a)
    event.sid mustBe sensorSampleEvent.sid
  }

  "Login SSEEvent" should "be wrapped as an instance of SSELoginEvent" in {
    val a: SSEEvent          = loginReqPayEvent
    val event: SSELoginEvent = a.asInstanceOf[SSELoginEvent]
    event.a mustBe loginReqPayEvent.a
  }

  "Connection SSEEvent" should "be wrapped as an instance of SSEConnectionEvent" in {
    val a: SSEEvent               = connectionEvent
    val event: SSEConnectionEvent = a.asInstanceOf[SSEConnectionEvent]
    event.a mustBe connectionEvent.a
  }

  "Device Alarm SSEEvent" should "be wrapped as an instance of SSEAlarmEvent" in {
    val a: SSEEvent          = alarmEvent
    val event: SSEAlarmEvent = a.asInstanceOf[SSEAlarmEvent]
    event mustBe
    SSEAlarmEvent(alertId,
                  msg,
                  severity,
                  `type`,
                  nodehw,
                  orgname,
                  sitename,
                  nodename,
                  created,
                  updated,
                  `type`,
                  msg,
                  false,
                  false)
    event.severity mustBe alarmEvent.severity
  }

  "BusinessAlert SSEEvent" should "be wrapped as an instance of SSEBusinessAlertEvent" in {
    val a: SSEEvent                  = businessAlertEvent
    val event: SSEBusinessAlertEvent = a.asInstanceOf[SSEBusinessAlertEvent]
    event.triggerId mustBe businessAlertEvent.triggerId
    event.triggerCategory mustBe businessAlertEvent.triggerCategory
  }

  "GPS Sample SSEEvent" should "be wrapped as an instance of SSEGpsEvent" in {
    val a: SSEEvent        = gpsEvent
    val event: SSEGpsEvent = a.asInstanceOf[SSEGpsEvent]
    event mustBe
    SSEGpsEvent(lat, lon, latUserAdded, lonUserAdded, created1, updated1)
    event.name = gpsEvent.name
  }

  "Login data" should "be wrapped as an instance of LoginReq" in {
    val a: LoginReq     = loginReqOuput
    val event: LoginReq = a.asInstanceOf[LoginReq]
    a mustBe loginReqOuput
  }

  "Sensor data" should "be wrapped as an instance of SensorSample" in {
    val a: SensorSample     = sensorsampleOutput
    val event: SensorSample = a.asInstanceOf[SensorSample]
    a mustBe sensorsampleOutput

  }
}
