package com.verizon.netsense.sse.specs.data

import java.time.format.DateTimeFormatter
import java.util.{Calendar, UUID}

import com.verizon.netsense.constants.SensorSampleConstants._
import com.verizon.netsense.model.{SensorPayload, SensorSampleEvent}
import com.verizon.netsense.sse.model._
import com.verizon.netsense.sse.util.ObjectMapperUtil
import com.verizon.netsense.utils.{Logging, SensorValueConverter}

trait TestData extends SSEGenData with Logging {

  val messageId: String                        = UUID.randomUUID().toString
  val requestId: String                        = UUID.randomUUID().toString
  lazy val instanceId: String                  = UUID.randomUUID().toString
  lazy val orgId: String                       = "O0"
  lazy val siteId: String                      = "S0"
  lazy val userId: String                      = UUID.randomUUID().toString
  lazy val nodeId: String                      = "JSV7_0"
  lazy val corenodeId: String                  = "JSV4_0"
  override val t                               = 1504209564836699580L
  lazy val timestamp: String                   = "1504209564836699580"
  lazy val u: String                           = "degC"
  val responsetopicInRequest: String           = "ISTopic"
  val stype                                    = "subscribe"
  val htype                                    = "heartbeat"
  val dtype                                    = "disconnect"
  val ftype                                    = "InvalidType"
  lazy val model                               = "SSEModel"
  lazy val topicNames: Set[String]             = Set("login,sensor,alert,connectionstatus")
  lazy val businessAlerttopicName: Set[String] = Set("businessalert")
  lazy val sseStreamTTL: Int                   = 3600

  lazy val ch: Int                   = 20
  lazy val sec: String               = "WPA_PSK"
  lazy val bssid: String             = "13:bb:05:83:ec:1d"
  lazy val dev: String               = "unode-v4"
  lazy val tok: String               = "94109817"
  lazy val ip: String                = "190.195.74.78"
  lazy val mac: String               = "23:43:53:83:41:8"
  lazy val ssid: String              = "SensityCorp"
  lazy val nid: String               = "JSV7_1"
  lazy val cid: String               = "c2097abdf"
  lazy val d: String                 = Calendar.getInstance().toInstant.toString
  lazy val profile: String           = ""
  lazy val protoc: Int               = 0
  lazy val subtype: String           = ""
  lazy val modemRevEd: String        = ""
  lazy val voltageType: String       = ""
  lazy val loginname: String         = "LoginReq"
  lazy val sensorname: String        = "SensorSample"
  lazy val connectionname: String    = "ConnectionStatus"
  lazy val sensorTopicname: String   = s"$orgId/$siteId/$nodeId/$sensorname"
  lazy val loginTopicname: String    = s"$orgId/$siteId/$nodeId/$loginname"
  lazy val businessTopicname: String = s"/streamv1/$orgId/$siteId/businessalert/$triggerCategory/$triggerId"
  lazy val StreamTtl: Int            = 30
  lazy val count: Int                = 1

  lazy val sensorId                = "pc"
  lazy val sensorValue             = 123.111
  lazy val sensorUnits             = "degC"
  lazy val starttimeInMillis: Long = System.currentTimeMillis()
  lazy val starttimeInNanos: Long  = starttimeInMillis * 1000000
  lazy val ISODateFormat           = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
  lazy val status: Boolean         = true
  lazy val cstatus: String         = "connected"
  lazy val scode: String           = "200"
  lazy val fcode: String           = "400"
  lazy val smsg: String            = s"$stype: Success"
  lazy val hmsg: String            = s"$htype: Success"
  lazy val dmsg: String            = s"$dtype: Success"
  lazy val fmsg: String            = s"$ftype: Bad Request"

  lazy val a1    = "UNSOL"
  lazy val p     = s"v1/$nodeId/out/sensor/$jtSensorType"
  lazy val sid   = "JSV7_0"
  lazy val uuid1 = "cbc15788-d572-4d89-a0f0-0917b6043d6d"
  lazy val f     = "t"

  /**
   * ObjectMapper mocks
   */
  lazy val unsolEventType = "UNSOL"
  lazy val eventPath      = s"v1/$nodeId/out/sensor/$sensorId"
  lazy val eventName      = "LoginReq"
  lazy val service        = "sensor"

  /**
   * device alarm
   */
  lazy val alertId  = "706952e6-3ed9-41c9-92df-6b19842adbaf"
  lazy val msg      = "Random Message"
  lazy val name     = "DeviceAlarm"
  lazy val severity = "Clear"
  lazy val `type`   = "HWFail_EEPROM"
  lazy val nodehw   = "nodehw1"
  lazy val orgname  = "orgname1"
  lazy val sitename = "sitename1"
  lazy val nodename = "nodename1"
  lazy val created  = "2017-06-27T13:50:05"
  lazy val updated  = "2017-06-27T13:50:05"

  /**
   * gps sample
   */
  lazy val gpsname                      = "GpsSample"
  lazy val lat: Option[Double]          = Some(42.614722)
  lazy val lon: Option[Double]          = Some(-71.324722345)
  lazy val latUserAdded: Option[Double] = Some(42.23455876788)
  lazy val lonUserAdded: Option[Double] = Some(-71.6888768812)
  lazy val created1: Option[Long]       = Some(1520958011523L)
  lazy val updated1: Option[Long]       = Some(1520958601779L)

  /**
   * business alert
   */
  lazy val application: String = "parking"
  lazy val businessAlertId     = "5fc6cc67-aaf7-4063-864f-c5467551ac46"
  lazy val triggerName         = "average,occupancy,15,>,80"
  lazy val triggerId           = "trigger0"
  lazy val triggerCategory     = "parking"
  lazy val triggerSubcategory  = "group"
  lazy val triggerUserId       = "a8f6cc67-aaf7-4063-864f-c5467551ac45"
  lazy val siteid              = "S0"
  lazy val orgid               = "O0"
  lazy val resourceId          = "3881f299-2450-4f2c-a2d6-03635938b836"
  lazy val resourceName        = "Marietta"
  lazy val active              = true
  lazy val createdOn           = "2017-12-14T01:00:00.000Z"
  lazy val lastUpdated         = "2018-01-12T10:00:00.000Z"
  lazy val lastClearedAt       = "2018-01-16T11:00:00.000Z"
  lazy val lastClearedBy       = "8991f299-2450-4f2c-a2d6-03635938b836"
  lazy val message             = "The average occupancy percentage of the Marietta over 15 minutes is > 80%"

  /**
   * Sensor Sample
   */
  lazy val sensorSamplePayload: SensorPayload =
    SensorPayload(nodeId, sensorUnits, sensorId, sensorValue, starttimeInNanos)

  lazy val sensorSampleEvent =
    SensorSampleEvent(uuid1, sid, a1, f, p, sensorSamplePayload)

  /**
   * Login Req
   */
  lazy val loginReqPayload: LoginReqPayload =
    LoginReqPayload(ch, sec, bssid, dev, tok, ip, mac, ssid, nid, cid, t, profile, protoc, subtype, modemRevEd)

  lazy val loginReqPayEvent: SSELoginEvent =
    SSELoginEvent(UUID.randomUUID().toString, nodeId, unsolEventType, sensorId, eventPath, d, loginReqPayload)

  /**
   * Connection Status
   */
  lazy val connectionStatusPayload: ConnectionStatusPayload = ConnectionStatusPayload(nodeId, status)

  lazy val connectionEvent: SSEConnectionEvent =
    SSEConnectionEvent(UUID.randomUUID().toString,
                       nodeId,
                       unsolEventType,
                       sensorId,
                       eventPath,
                       d,
                       connectionStatusPayload)

  lazy val alarmEvent: SSEAlarmEvent =
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

  lazy val gpsEvent: SSEGpsEvent =
    SSEGpsEvent(lat, lon, latUserAdded, lonUserAdded, created1, updated1)

  lazy val businessAlertEvent: SSEBusinessAlertEvent =
    SSEBusinessAlertEvent(
      businessAlertId,
      triggerName,
      triggerSubcategory,
      triggerUserId,
      resourceId,
      resourceName,
      active,
      createdOn,
      lastUpdated,
      lastClearedAt,
      lastClearedBy,
      message,
      severity
    )

  val loginReqOuput: LoginReq =
    LoginReq(ch,
             sec,
             bssid,
             dev,
             tok,
             ip,
             mac,
             loginname,
             ssid,
             nodeId,
             orgId,
             siteId,
             cid,
             t,
             profile,
             protoc,
             subtype,
             modemRevEd,
             voltageType)

  val sensorsampleOutput: SensorSample =
    SensorSample(sensorname, nodeId, orgId, sensorId, siteId, t, sensorUnits, sensorValue)

  val corenodesensorsampleOutput: SensorSample =
    SensorSample(sensorname, corenodeId, orgId, sensorId, siteId, t, sensorUnits, sensorValue)

  val connectionstatusOutput: ConnectionStatus =
    ConnectionStatus(connectionname, nodeId, orgId, siteId, cstatus)

  val gpsSampleOutput: GpsSample =
    GpsSample(nodeId, gpsname, orgId, siteId, lat, lon, latUserAdded, lonUserAdded, created1, updated1)

  val deviceAlarmOutput: DeviceAlarm =
    DeviceAlarm(alertId,
                msg,
                name,
                nodeId,
                orgId,
                siteId,
                severity,
                `type`,
                nodehw,
                `type`,
                msg,
                orgname,
                sitename,
                nodename,
                created,
                updated,
                false,
                false)

  val businessAlertOutput: BusinessAlert =
    BusinessAlert(
      businessAlertId,
      triggerName,
      triggerId,
      triggerCategory,
      triggerSubcategory,
      triggerUserId,
      siteid,
      orgid,
      resourceId,
      resourceName,
      active,
      createdOn,
      lastUpdated,
      lastClearedAt,
      lastClearedBy,
      message,
      severity
    )

  lazy val jtSensorType = jt

  def sensorSampleEventDecoded(sensorId: String, value: Double) =
    SensorSampleEvent(
      uuid1,
      sid,
      a1,
      sensorId,
      SensorValueConverter.constructRestPathForEventPersist(nodeId, sensorId),
      SensorPayload(nodeId, sensorUnits, sensorId, value, t)
    )

  def generateValidSubscribeRequest(subscribeType: String): SSESubscribeRequest = {
    val orgProps  = OrgProps(orgId)
    val nodeProps = NodeProps(nodeId)
    val siteProps = SiteProps(siteId)
    val extProps  = ExtProps(eventName)
    val requestQuery: SSERequest =
      SSERequest(instanceId,
                 requestId,
                 timestamp,
                 subscribeType,
                 model,
                 userId,
                 orgProps,
                 siteProps,
                 nodeProps,
                 extProps)
    SSESubscribeRequest(messageId, responsetopicInRequest, requestQuery)
  }

  def generateValidBusinessAlertSubscribeRequest(subscribeType: String): SSESubscribeBusinessAlertRequest = {
    val orgProps      = OrgProps(orgId)
    val siteProps     = SiteProps(siteId)
    val businessProps = BusinessAlertProps(application, triggerId)
    val requestQuery: SSEBusinessRequest =
      SSEBusinessRequest(instanceId,
                         requestId,
                         timestamp,
                         subscribeType,
                         model,
                         userId,
                         orgProps,
                         siteProps,
                         businessProps)
    SSESubscribeBusinessAlertRequest(messageId, responsetopicInRequest, requestQuery)
  }

  def generateValidSensorRequest: SSESubscribeRequest = {
    val orgProps  = OrgProps(orgId)
    val nodeProps = NodeProps(nodeId)
    val siteProps = SiteProps(siteId)
    val extProps  = ExtProps(sensorname)
    val requestQuery: SSERequest =
      SSERequest(instanceId, requestId, timestamp, stype, model, userId, orgProps, siteProps, nodeProps, extProps)
    SSESubscribeRequest(messageId, responsetopicInRequest, requestQuery)
  }

  def generateSubscribeResponse(subscribeType: String, statusCode: String, statusMsg: String): SSESubscribeResponse = {
    val orgProps    = OrgProps(orgId)
    val nodeProps   = NodeProps(nodeId)
    val siteProps   = SiteProps(siteId)
    val extProps    = ExtProps(eventName)
    val statusProps = StatusProps(statusCode, statusMsg)
    val responseQuery: SSEResponse =
      SSEResponse(instanceId,
                  requestId,
                  timestamp,
                  subscribeType,
                  model,
                  userId,
                  orgProps,
                  siteProps,
                  nodeProps,
                  extProps,
                  statusProps)
    SSESubscribeResponse(messageId, responsetopicInRequest, responseQuery)
  }

  def generateValidBusinessAlertSubscribeResponse(subscribeType: String,
                                                  statusCode: String,
                                                  statusMsg: String): SSESubscribeBusinessAlertResponse = {
    val orgProps      = OrgProps(orgId)
    val siteProps     = SiteProps(siteId)
    val businessProps = BusinessAlertProps(application, triggerId)
    val statusProps   = StatusProps(statusCode, statusMsg)
    val requestQuery: SSEBusinessResponse =
      SSEBusinessResponse(instanceId,
                          requestId,
                          timestamp,
                          subscribeType,
                          model,
                          userId,
                          orgProps,
                          siteProps,
                          businessProps,
                          statusProps)
    SSESubscribeBusinessAlertResponse(messageId, responsetopicInRequest, requestQuery)
  }

  val orgEvent: OrgEvent = OrgEvent(
    orgId,
    siteId,
    nodeId
  )
  val loginReqRaw =
    """{"a":"UNSOL","p":"()","sid":"JSV7_0","uuid":"cbc15788-d572-4d89-a0f0-0917b6043d6d","f":"205","d":"2017-06-27T13:50:05Z","l":{"ch": 20,"sec":"WPA_PSK","bssid":"13:bb:05:83:ec:1d","dev":"unode-v4","tok":"94109817","ip":"190.195.74.78","mac":"23:43:53:83:41:8","ssid":"SensityCorp","nid":"JSV7_0","cid":"c2097abdf","t":1504209564836699580,"profile":"","protoc":0,"subtype":"","modemRevEd":""}}"""

  val loginEventObj = loginReqOuput
  log.info("loginReqObj data -->" + loginEventObj)
  loginEventObj.nodeid = "JSV7_0"
  loginEventObj.orgid = "O0"
  loginEventObj.siteid = "S0"
  loginEventObj.name = "LoginReq"
  val loginReqBytes = loginReqRaw.getBytes
  val loginReqStr   = ObjectMapperUtil.toJson(loginEventObj)
  log.info("loginReqStr-->" + loginReqStr)

  val sensorSampleRaw =
    """{"a":"UNSOL","p":"()","sid":"JSV7_0","uuid":"cbc15788-d572-4d89-a0f0-0917b6043d6d","f":"205","d":"2017-06-27T13:50:05Z","l":{"s":"pc","t":1504209564836699580,"u":"degC","v":123.111}}"""
  val sensorSampleObj = sensorsampleOutput
  log.info("SensorSampleobj data -->" + sensorSampleObj)
  sensorSampleObj.nodeid = "JSV7_0"
  sensorSampleObj.orgid = "O0"
  sensorSampleObj.siteid = "S0"
  sensorSampleObj.name = "SensorSample"
  val sensorSampleBytes = sensorSampleRaw.getBytes
  val sensorSampleStr   = ObjectMapperUtil.toJson(sensorSampleObj)
  log.info("sensorSampleStr -->" + sensorSampleStr)

  val connectionStatusRaw =
    """{"a":"UNSOL","p":"()","sid":"device-service","uuid":"cbc15788-d572-4d89-a0f0-0917b6043d6d","f":"205","d":"2017-06-27T13:50:05Z","l":{"nodeid":"JSV7_0","status":true}}"""
  val connectionStatusObj = connectionstatusOutput
  log.info("ConnectionStatusobj data -->" + connectionStatusObj)
  connectionStatusObj.nodeid = "JSV7_0"
  connectionStatusObj.orgid = "O0"
  connectionStatusObj.siteid = "S0"
  connectionStatusObj.name = "ConnectionStatus"
  val connectionStatusBytes = connectionStatusRaw.getBytes
  val connectionStatusStr   = ObjectMapperUtil.toJson(connectionStatusObj)
  log.info("connectionStatusStr -->" + connectionStatusStr)

  val gpsSampleRaw =
    s"""{
    "nodeid":"$nodeId",
    "name":"GpsSample",
    "orgid":"$orgId",
    "siteid":"$siteId",
    "latitude":42.614722,
    "longitude":-71.324722345,
    "latUserAdded":42.23455876788,
    "lonUserAdded":-71.6888768812,
    "created":1520958011523,
    "updated":1520958601779
  }"""
  val gpsSampleObj = gpsSampleOutput
  log.info("GpsSampleobj data -->" + gpsSampleObj)
  val gpsSampleBytes = gpsSampleRaw.getBytes
  val gpsSampleStr   = ObjectMapperUtil.toJson(gpsSampleObj)
  log.info("GpsSampleStr -->" + gpsSampleStr)

  val corenodeSensorRaw =
    s"""{
      "name":"SensorSample",
      "nodeid":"JSV4_0",
      "sensor":"jt",
      "time":"1504209564836699580",
      "units":"waM",
      "value": 123
    }""".stripMargin
  val corenodeSensorRawObj = corenodesensorsampleOutput
  log.info("corenodeSensorRawobj data -->" + corenodeSensorRawObj)
  val corenodeSensorRawBytes = corenodeSensorRaw.getBytes
  val corenodeSensorRawStr   = ObjectMapperUtil.toJson(corenodeSensorRawObj)
  log.info("corenodeSensorRawStr -->" + corenodeSensorRawStr)

  val devicealarmRaw =
    s"""{"alertid":"706952e6-3ed9-41c9-92df-6b19842adbaf",
       "msg":"Random Message",
       "name":"DeviceAlarm",
       "nodeid":"JSV7_0",
       "orgid":"O0",
       "siteid":"S0",
       "severity":"Clear",
       "type":"HWFail_EEPROM",
       "nodehw":"nodehw1",
       "ufname":"HWFail_EEPROM",
       "description":"Random Message",
       "displaytopartner": false,
       |"displaytocustomer": false,
       "orgname":"orgname1",
       "sitename":"sitename1",
       "nodename":"nodename1",
       "created":"2017-06-27T13:50:05",
       "updated":"2017-06-27T13:50:05"}""".stripMargin
  val devicealarmObj = deviceAlarmOutput
  log.info("devicealarmobj data -->" + devicealarmObj)
  val devicealarmBytes = devicealarmRaw.getBytes
  val devicealarmStr   = ObjectMapperUtil.toJson(devicealarmObj)
  log.info("devicealarmStr -->" + devicealarmStr)

  val businessAlertRaw =
    """{
      "businessAlertId":"5fc6cc67-aaf7-4063-864f-c5467551ac46",
      "triggerName":"average,occupancy,15,>,80",
      "triggerId":"trigger0",
      "triggerCategory":"parking",
      "triggerSubcategory":"group",
       "triggerUserId":"a8f6cc67-aaf7-4063-864f-c5467551ac45",
      "siteid":"S0",
      "orgid":"O0",
      "resourceId":"3881f299-2450-4f2c-a2d6-03635938b836",
      "resourceName":"Marietta",
      "active":true,
      "createdOn":"2017-12-14T01:00:00.000Z",
       "lastUpdated":"2018-01-12T10:00:00.000Z",
       "lastClearedAt":"2018-01-16T11:00:00.000Z",
      "lastClearedBy":"8991f299-2450-4f2c-a2d6-03635938b836",
      "message":"The average occupancy percentage of the Marietta over 15 minutes is > 80%",
      "severity":"Clear"
    }"""

  val businessAlertObj = businessAlertOutput
  log.info("businessAlertobj data -->" + businessAlertObj)
  val businessAlertBytes = businessAlertRaw.getBytes
  val businessAlertStr   = ObjectMapperUtil.toJson(businessAlertObj)
  log.info("businessAlertStr -->" + businessAlertStr)
}
