package com.verizon.netsense.sse.specs.data

import java.util.UUID

import scala.util.Random

/**
 * Created by sundach on 6/7/17.
 */
trait SSEGenData {
  val r         = Random
  val random    = UUID.randomUUID().toString
  val t         = 1504209564836699580L
  def r(i: Int) = Random.nextInt(i)
  def node      = "JSV7_" + Random.nextInt(10)
  /*
   * Sample data
   */
  def constructInput(payload: String): String =
    s"""{"a":"UNSOL","p":"()","sid":"JSV7_0","uuid":"$random","f":"205","d":"2017-06-27T13:50:05Z","l":$payload}"""

  def deviceAlarm(i: Int): String =
    s"""{"alertid":"706952e6-3ed9-41c9-92df-6b19842adbaf","msg":"Random Message","name":"DeviceAlarm","nodeid":"JSV7_$i","orgid":"O$i","severity":"Clear","siteid":"S$i","type":"HWFail_EEPROM","nodehw":"nodehw1","orgname":"orgname1","sitename":"sitename1","nodename":"nodename1","created":"2017-06-27T13:50:05","updated":"2017-06-27T13:50:05"}"""

  def connectionStatus(i: Int): String =
    s"""{"nodeid":"JSV7_$i","status":false}"""

  def gpsSample(i: Int): String =
    s"""{
    "nodeid":"JSV7_$i",
    "name":"GpsSample",
    "orgid":"O$i",
    "siteid":"S$i",
    "latitude":42.614722,
    "longitude":-71.324722345,
    "latUserAdded":42.23455876788,
    "lonUserAdded":-71.6888768812,
    "created":1520958011523,
    "updated":1520958601779
  }""".stripMargin

  def businessAlert(i: Int): String =
    s"""{
        "businessAlertId":"5fc6cc67-aaf7-4063-864f-c5467551ac46",
        "triggerName":"average,occupancy,15,>,80",
        "triggerId":"trigger123",
        "triggerCategory":"parking",
        "triggerSubcategory":"group",
        "triggerUserId":"a8f6cc67-aaf7-4063-864f-c5467551ac45",
        "siteid":"S$i",
        "orgid":"O$i",
        "resourceId":"3881f299-2450-4f2c-a2d6-03635938b836",
        "resourceName":"Marietta",
        "active":true,
        "createdOn":"2017-12-14T01:00:00.000Z",
        "lastUpdated":"2018-01-12T10:00:00.000Z",
        "lastClearedAt":"2018-01-16T11:00:00.000Z",
        "lastClearedBy":"8991f299-2450-4f2c-a2d6-03635938b836",
        "message":"The average occupancy percentage of the Marietta over 15 minutes is > 80%",
        "severity":"Major"
        }""".stripMargin

  def loginReq(i: Int): String =
    s"""{"ch": ${r(999)},"sec":"WPA_PSK","bssid":"13:bb:05:83:ec:1d","dev":"unode-v$i","tok":"${r(300000000)}","ip":"${r(
      300
    )}.${r(300)}.${r(300)}.${r(300)}","mac":"${r(99)}:${r(99)}:${r(99)}:${r(99)}:${r(99)}:${r(99)}","ssid":"SensityCorp","nid":"JSV7_$i","cid": "c2097abdf","t": $t,"profile":"","protoc":0,"subtype":"","modemRevEd":""}"""

  def sensorSample(i: Int): String =
    s"""{"u":"degC","s":"jt","t":$t,"v":${r(99)}}"""

  def sensorSubscribe(r: Int) =
    s"""{"messageid":"$random",
        |"responsetopic":"ISTopic",
        |"request":{
        |"instanceid":"$random",
        |"requestid":"$random",
        |"timestamp":"2016-06-03T22:54:46.670Z",
        |"type":"subscribe",
        |"model":"SSEModel",
        |"user":"d48dcd46-da6e-49b0-b76e-e3cbc79c4565",
        |"orgprops":{"orgid":"O$r"},
        |"siteprops":{"siteid":"S$r"},
        |"nodeprops":{"nodeid":"JSV7_$r"},
        |"extprops":{"eventtype":"SensorSample"}}}""".stripMargin

  def alarmSubscribe(r: Int) =
    s"""{"messageid":"$random",
        |"responsetopic":"ISTopic",
        |"request":{
        |"instanceid":"$random",
        |"requestid":"$random",
        |"timestamp":"2016-06-03T22:54:46.670Z",
        |"type":"subscribe",
        |"model":"SSEModel",
        |"user":"d48dcd46-da6e-49b0-b76e-e3cbc79c4565",
        |"orgprops":{"orgid":"O$r"},
        |"siteprops":{"siteid":"S$r"},
        |"nodeprops":{"nodeid":"JSV7_$r"},
        |"extprops":{"eventtype":"DeviceAlarm"}}}""".stripMargin

  def loginSubscribe(r: Int) =
    s"""{"messageid":"$random",
        |"responsetopic":"ISTopic",
        |"request":{
        |"instanceid":"$random",
        |"requestid":"$random",
        |"timestamp":"2016-06-03T22:54:46.670Z",
        |"type":"subscribe",
        |"model":"SSEModel",
        |"user":"d48dcd46-da6e-49b0-b76e-e3cbc79c4565",
        |"orgprops":{"orgid":"O$r"},
        |"siteprops":{"siteid":"S$r"},
        |"nodeprops":{"nodeid":"JSV7_$r"},
        |"extprops":{"eventtype":"LoginReq"}}}""".stripMargin

  def connectionSubscribe(r: Int) =
    s"""{"messageid":"$random",
        |"responsetopic":"ISTopic",
        |"request":{
        |"instanceid":"$random",
        |"requestid":"$random",
        |"timestamp":"2016-06-03T22:54:46.670Z",
        |"type":"subscribe",
        |"model":"SSEModel",
        |"user":"d48dcd46-da6e-49b0-b76e-e3cbc79c4565",
        |"orgprops":{"orgid":"O$r"},
        |"siteprops":{"siteid":"S$r"},
        |"nodeprops":{"nodeid":"JSV7_$r"},
        |"extprops":{"eventtype":"ConnectionStatus"}}}""".stripMargin

  def next(i: Int) = {
    var n = 0
    if (i < 4)
      n = n + 1
    else n = 0
    n
  }

  val deviceAlarmJson =
    """{"alarmSeverity":"Clear","alarmType":"HWFail_EEPROM","alertid":"706952e6-3ed9-41c9-92df-6b19842adbaf","msg":"","name":"DeviceAlarm","nodeid":"JSV7_0","orgid":"O0","severity":"Clear","siteid":"S0","type":"HWFail_EEPROM"}"""

  val loginreqJson =
    s"""{"ch":20,"sec":"WPA_PSK","bssid":"13:bb:05:83:ec:1d","dev":"unode-v","tok":"${r(300000000)}","ip":"${r(300)}.${r(
      300
    )}.${r(300)}.${r(300)}","mac":"${r(99)}:${r(99)}:${r(99)}:${r(99)}:${r(99)}:${r(99)}","ssid":"SensityCorp","nid":"JSV7_0","cid": "c2097abdf","t": $t}"""

  val sensorsampleJson =
    """{"s":"t","t":1498585214112607,"u":"degC","v":25}"""

}
