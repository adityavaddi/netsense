package com.verizon.netsense.sse.simulator.data

import java.time.LocalDateTime
import java.util.UUID

import com.verizon.netsense.sse.simulator.config.ConfigLoader._

import scala.util.Random

/**
 * Created by subrsi9 on 11/20/17.
 */
trait SSEEventData {

  lazy val random    = UUID.randomUUID().toString
  lazy val time      = System.currentTimeMillis()
  lazy val date      = LocalDateTime.now
  lazy val nodeId    = nodeIdPrefix
  lazy val triggerId = triggerIdPrefix

  def r(i: Int) = Random.nextInt(i)

  def constructLoginReq(nodeSuffix: Int): String =
    s"""{
       "a"   : "UNSOL",
       "p"   : "()",
       "sid" : "$nodeId$nodeSuffix",
       "uuid": "$random",
       "f"   : "205",
       "d"   : "$date",
       "l"   : {"ch": "${r(999)}","sec":"WPA_PSK","bssid":"13:bb:05:83:ec:1d","dev":"$nodeName",
        "tok":"${r(300000000)}","ip":"${r(300)}.${r(300)}.${r(300)}.${r(300)}","mac":"${r(99)}:${r(99)}:${r(99)}:${r(
         99
       )}:${r(99)}:${r(99)}",
    "ssid":"SensityCorp","nid":"$nodeId$nodeSuffix","cid": "c2097abdf","t": "$time","profile":"","protoc":0,"subtype":"","modemRevEd":""}}""".stripMargin

  def constructSensor(nodeSuffix: Int): String =
    s"""{
       "a"   : "UNSOL",
       "p"   : "()",
       "sid" : "$nodeId$nodeSuffix",
       "uuid": "$random",
       "f"   : "205",
       "d"   : "$date",
       "l"   : {"n" : "JSV7_1","u":"degC","s":"jt","t":"$time","v":${r(99)}}}""".stripMargin

  def constructCorenodeSensor(nodeSuffix: Int): String =
    s"""{
     "name":"SensorSample",
     "nodeid":"$nodeId$nodeSuffix",
     "sensor":"jt",
     "time":"$time",
     "units":"waM",
     "value":${r(99)}
     }""".stripMargin

  def constructGpsSample(nodeSuffix: Int): String =
    s"""{
    "nodeid":"N06232eb5",
    "name":"GpsSample",
    "orgid":"$orgId",
    "siteid":"$siteId",
    "latitude":42.614722,
    "longitude":-71.324722345,
    "latUserAdded":42.23455876788,
    "lonUserAdded":-71.6888768812,
    "created":1520958011523,
    "updated":1520958601779
  }""".stripMargin

  def constructDeviceAlarm(nodeSuffix: Int): String =
    s"""{"alertid":"$random",
       "msg":"Simulator Random Message","name":"DeviceAlarm","nodeid":"JSV7_1","orgid":"$orgId","severity":"Clear",
       "siteid":"$siteId","type":"HWFail_EEPROM","nodehw":"nodehw1","orgname":"$orgName","sitename":"$siteName",
       "nodename":"$nodeName","created":"$date","updated":"$date"}""".stripMargin

  def constructBusinessAlert(nodeSuffix: Int): String =
    s"""{
        "businessAlertId":"5fc6cc67-aaf7-4063-864f-c5467551ac46",
        "triggerName":"average,occupancy,15,>,80",
        "triggerId":"$triggerId$nodeSuffix",
        "triggerCategory":"parking",
        "triggerSubcategory":"group",
        "triggerUserId":"a8f6cc67-aaf7-4063-864f-c5467551ac45",
        "siteid":"$siteId",
        "orgid":"$orgId",
        "resourceId":"3881f299-2450-4f2c-a2d6-03635938b836",
        "resourceName":"Marietta",
        "active":true,
        "createdOn":"$date",
        "lastUpdated":"$date",
        "lastClearedAt":"2018-01-16T11:00:00.000Z",
        "lastClearedBy":"8991f299-2450-4f2c-a2d6-03635938b836",
        "message":"The average occupancy percentage of the Marietta over 15 minutes is > 80%",
        "severity":"Major"
        }""".stripMargin

  def constructConStatus(nodeSuffix: Int): String =
    s"""{
       "a"   : "UNSOL",
       "p"   : "()",
       "sid" : "device-service",
       "uuid": "$random",
       "f"   : "",
       "d"   : "$date",
       "l"   : {"nodeid":"$nodeId$nodeSuffix","status":false}}""".stripMargin

  def constructSubscribe(subscribeType: String): String =
    s"""{"messageid":"$random",
         "responsetopic":"$sseSubscribeReplyTopic",
         "request":{
         "instanceid":"$random",
         "requestid":"$random",
         "timestamp":"$date",
         "type":"$subscribeType",
         "model":"SSEModel",
         "user":"d48dcd46-da6e-49b0-b76e-e3cbc79c4565",
         "orgprops":{"orgid":"$orgId"},
         "siteprops":{"siteid":"$siteId"},
         "nodeprops":{"nodeid":"+"},
         "extprops":{"eventtype":"+"}}}""".stripMargin

  def constructbusinessAlertSubscribe(subscribeType: String): String =
    s"""{
             "messageid": "$random",
             "responsetopic": "$sseSubscribeReplyTopic",
             "request": {
             "instanceid": "$random",
             "requestid": "$random",
             "timestamp": "$date",
             "type": "$subscribeType",
             "model": "SSEBusinessAlertModel",
             "user": "d48dcd46-da6e-49b0-b76e-e3cbc79c4565",
             "orgprops": {"orgid": "$orgId"},
             "siteprops": {"siteid": "$siteId"},
             "businessalertprops":{
             "application": "+",
             "triggerid": "+"
                }
               }
              }""".stripMargin
}
