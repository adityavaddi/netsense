package com.verizon.netsense.data

import java.time.format.DateTimeFormatter
import java.util.{Calendar, UUID}

import com.verizon.netsense.constants.SensorSampleConstants
import com.verizon.netsense.constants.SensorSampleConstants._
import com.verizon.netsense.model._
import com.verizon.netsense.utils.{SensorValueConverter, TimeConverter}

/**
  * Created by subrsi9 on 6/29/17.
  */
trait TestData extends TimeConverter {

  /*
    Sensor pay load
    https://xeranet.atlassian.net/wiki/display/NSN/RestPackMQ+Format+for+Sensor+Service
   */

  val messageId: String              = UUID.randomUUID().toString
  val requestId: String              = UUID.randomUUID().toString
  lazy val instanceId: String             = UUID.randomUUID().toString
  lazy val orgId: String                  = UUID.randomUUID().toString
  val siteId: String                 = "4aacfab0-4cc8-11e7-a01d-1bee4134e9b6"
  lazy val userId: String                 = UUID.randomUUID().toString
  lazy val nodeId: String                 = UUID.randomUUID().toString
  lazy val sensorUnits: String            = "degC"
  lazy val coreNodeId: String             = UUID.randomUUID().toString
  lazy val timestamp: String              = Calendar.getInstance().toInstant.toString
  val responsetopicInRequest: String = UUID.randomUUID().toString

  val date1: String                   = convertMicrosToISO(starttimeInMicros - 10L)
  val date2: String                   = convertMicrosToISO(starttimeInMicros + 10L)
  lazy val sensorId                   = "pc"
  lazy val sensorIdSingle             = null
  lazy val callbackaddress            = "callbackaddress"
  lazy val limit                      = 120
  val `type`                          = "getHistoricalDataForSensor"
  lazy val model                      = "NodeModel"
  lazy val action                     = "CAN_READ"
  lazy val sensorValue                = 123.111
  lazy val starttimeInMillis: Long    = System.currentTimeMillis()
  lazy val starttimeInMicros: Long    = starttimeInMillis * 1000
  lazy val startTimeInISO             = convertMicrosToISO(starttimeInMillis)
  lazy val startTimeInISO1            = convertMicrosToISO(starttimeInMicros)

  lazy val energySensorId             = "energy"
  lazy val aggregationtype            = "15min"

  lazy val legacy_energy_consumption  = 100.1
  lazy val led_energy_consumption     = 100.2
  lazy val actual_energy_consumption  = 100.3
  lazy val savings_legacy_vs_led      = 100.4
  lazy val savings_legacy_vs_actual   = 100.5

  lazy val startdt                    = "6jun"
  lazy val starthr                    = "3hr"
  lazy val enddt                      = "9jun"
  lazy val endtime                    = 67583940
  lazy val ts                         = 109875786

  /**
    * Sensor Converter mock values
    */
  lazy val bRSensorType  = bR
  lazy val jtSensorType  = jt
  lazy val rfSensorType  = rf
  lazy val jtValue       = BigDecimal(4684316664362221708L)
  lazy val bRValue       = 264
  lazy val rfValue       = java.lang.Long.parseUnsignedLong("18446744073703063472")

  override lazy val ISODateFormat     = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")

  /**
    * ObjectMapper mocks
    */
  lazy val unsolEventType             = "UNSOL"
  lazy val eventPath                  = s"v1/$nodeId/out/sensor/$sensorId"
  lazy val eventName                  = "SensorSample"
  lazy val service                    = "sensor"

  lazy val extProps                   = ExtProps(sensorId, date1, date1, Some(date2), limit = limit, Some(aggregationtype))
  lazy val nodeProps                  = NodeProps(nodeId)
  lazy val siteProps                  = SiteProps(siteId)
  lazy val orgProps                   = OrgProps(orgId)

  /**
    * Device Sensor Sample
    */
  lazy val sensorSamplePayload: SensorPayload = SensorPayload(n = nodeId, u = sensorUnits, s = sensorId, v = sensorValue, t = starttimeInMicros)

  def constructSensorSamplePayload(time: Long): SensorPayload = SensorPayload(n = nodeId, u = sensorUnits, s = sensorId, v = sensorValue, t = time)

  lazy val sensorSamplePayloadTuple = (starttimeInMicros, sensorValue)

  lazy val sensorSampleResponsePayload = SensorSamplePayload(sensorId, startTimeInISO1, sensorValue)
  lazy val sensorSampleResponsePayloadSingleSensor = SensorSamplePayload(sensorIdSingle, startTimeInISO1, sensorValue)


  lazy val sensorSampleEvent: SensorSampleEvent = SensorSampleEvent(UUID.randomUUID().toString, nodeId, unsolEventType, sensorId, eventPath, sensorSamplePayload)

  def constructSensorSampleEvent(time:Long): SensorSampleEvent = SensorSampleEvent(UUID.randomUUID().toString, nodeId, unsolEventType, sensorId, eventPath, constructSensorSamplePayload(time))
  def constructSensorSampleEventFixture(time:Long): SensorSampleEvent = SensorSampleEvent(UUID.randomUUID().toString, nodeId, unsolEventType, sensorIdFixture, eventPath, constructSensorSamplePayload(time))

  lazy val sensorSamplePayloadList = Vector(sensorSampleResponsePayload)
  lazy val sensorSamplePayloadListSingleSensor = Vector(sensorSampleResponsePayloadSingleSensor)

  lazy val SampleQueryResponse: SensorSampleQueryPayload =
    SensorSampleQueryPayload(requestId, true, timestamp, null, 200, sensorId, items = sensorSamplePayloadList)
  lazy val SampleQueryResponseSingleSensor: SensorSampleQueryPayload =
    SensorSampleQueryPayload(requestId, true, timestamp, null, 200, sensorId, items = sensorSamplePayloadListSingleSensor)




  lazy val sensorSampleQueryResponse = SensorSampleQueryResponse(messageId, SampleQueryResponse)

  /**
    * Energy Savings Node
    */
  lazy val sEnergySavingNodePayloadItem = EnergySavingsNodePayloadItem(siteId,
    nodeId,
    startdt,
    legacy_energy_consumption,
    led_energy_consumption,
    actual_energy_consumption,
    savings_legacy_vs_led,
    savings_legacy_vs_actual)

  lazy val sEnergySavingNodePayloadList = List(sEnergySavingNodePayloadItem)

  lazy val energySavingsNodeModel: EnergySavingsNodeModel =
    EnergySavingsNodeModel(
      siteid = siteId,
      nodeId,
      aggregationtype,
      starttimeInMicros,
      actual_energy_consumption,
      enddt,
      endtime,
      led_energy_consumption,
      legacy_energy_consumption,
      savings_legacy_vs_actual,
      savings_legacy_vs_led,
      startdt,
      startdt,
      starthr,
      ts
    )

  lazy val energyNodeResponsePayload: SensorEnergyNodePayload =
    SensorEnergyNodePayload(requestId, true, timestamp, sEnergySavingNodePayloadList)

  lazy val sensorEnergyNodeResponse: SensorEnergyNodeResponse =
    SensorEnergyNodeResponse(messageId, energyNodeResponsePayload)

  /**
    * Energy Savings Site
    */
  lazy val energySavingsSiteModel: EnergySavingsSiteModel =
    EnergySavingsSiteModel(
      siteId,
      aggregationtype,
      starttimeInMicros,
      startdt,
      actual_energy_consumption,
      led_energy_consumption,
      legacy_energy_consumption,
      savings_legacy_vs_actual,
      savings_legacy_vs_led
    )

  lazy val sEnergySavingSitePayloadItem = EnergySavingsSitePayloadItem(siteId,
    startdt,
    legacy_energy_consumption,
    led_energy_consumption,
    actual_energy_consumption,
    savings_legacy_vs_led,
    savings_legacy_vs_actual)

  lazy val sEnergySavingSitePayloadList = List(sEnergySavingSitePayloadItem)

  lazy val EnergySiteResponsePayload: SensorEnergySitePayload =
    SensorEnergySitePayload(requestId, true, timestamp, sEnergySavingSitePayloadList)

  lazy val sensorEnergySiteResponse: SensorEnergySiteResponse =
    SensorEnergySiteResponse(messageId, EnergySiteResponsePayload)

  /**
    * Request Headers IS -> SS
    *
    * @param _nodeId   - either nodeId or "all"
    * @param _sensorId - Type of sensor or "energy"
    * @return - SensorQueryEnvelope
    */
  def generateQueryWithHeaders(_nodeId: String = nodeId, _sensorId: String = sensorId): SensorQueryEnvelope = {

    val orgProps  = OrgProps(orgId)
    val nodeProps = NodeProps(_nodeId)
    val siteProps = SiteProps(siteId)
    val extProps  = ExtProps(_sensorId, date1, date1, Option(date2), limit)
    val requestQuery: SensorQueryPayload = SensorQueryPayload(requestId,
      `type`,
      model,
      action,
      instanceId,
      timestamp,
      userId,
      service,
      nodeProps,
      siteProps,
      orgProps,
      extProps)
    SensorQueryEnvelope(messageId, responsetopicInRequest, requestQuery)
  }

  lazy val requestQueryWithHeaders: SensorQueryEnvelope = generateQueryWithHeaders()
  lazy val requestQueryWithHeadersIngest: SensorSampleEvent = sensorSampleEvent

  /**
    * For Sensor Value conversion Path construction
    *
    * @param sensorId - Type of the Sensor
    * @param value    - Sensor Value BigDecimal
    * @return
    */

  def sensorSampleEventDecoded(sensorId: String, value: Double) =
    SensorSampleEvent(
      uuid = requestId,
      sid = nodeId,
      a = SensorSampleConstants.UNSOL,
      f = sensorId,
      p = SensorValueConverter.constructRestPathForEventPersist(nodeId, sensorId),
      l = SensorPayload(n = nodeId, u = sensorUnits, s = sensorId, v = value, t = starttimeInMicros)
    )
  def sensorSampleEventDecoded1(sensorId: String, value: Double) =
    SensorSampleEvent(
      uuid =requestId,
      sid = nodeId,
      a = `type`,
      f = null,
      p = SensorValueConverter.constructRestPathForEventPersist(nodeId, sensorId),
      l = SensorPayload(n = nodeId, u = sensorUnits, s = null, v = null, t = starttimeInMicros)
    )

  def buildSensorPayload(sensorTypeForTest: String): SensorSampleEvent = {
    val sensorSamplePayloadForSensor: SensorPayload =
      SensorPayload(nodeId,"", sensorTypeForTest, sensorValue, starttimeInMicros)

    SensorSampleEvent(UUID.randomUUID().toString,
      nodeId,
      unsolEventType,
      sensorTypeForTest,
      eventPath,
      sensorSamplePayloadForSensor)
  }

  def generateCoreNodeSensorSample(nodeId: String = coreNodeId,
                                   sensorId: String = "p",
                                   time: Long = System.currentTimeMillis(), value: Double) =
    CoreNodeRawSensorSample(
      "SensorSample",
      coreNodeId,
      sensorId,
      time,
      "?u",
      value)

  // Fixture test data

 lazy val fixturenodeid = "uberNodeFixture"
 lazy val sensorIdFixture = "p"
 lazy val sensorModel = "unode-v7"
 lazy val fixtureUnits = "mW"
 lazy val fixtureSensorValue = 200
 lazy val fixtureSensorValueOverPower = 200
 lazy val fixtureSensorValueUnderPower = 19
 lazy val fixtureSensorValueClearPower = 100
 lazy val driverLevel = 9.9
 lazy val deviceAlarm = "DeviceAlarm"
 lazy val underPower = "UnderPower"
 lazy val overPower = "OverPower"
  val street1 = "900 Chelmsford street"
  val city1 = "Lowell"
  val state1 = "MA"
  val zipcode1 = "01867"
  val country1 = "USA"
  val latitude1 = "42.614685"
  val longitude1 = "-71.324845"
  val timezone1 = "America/New_York"
  val nodetype1 = "unode-v7"
  val nodename1 = "Sample Node"

  // creating node
//  val createNode = s"MERGE (node:Node:Active {nodeid: '$fixturenodeid'})"
  val createNode = s"MERGE (site)-[:HAS]->(node:Node:Active {nodeid: '$fixturenodeid', name: '$nodename1'," +
  s" building: '2', longitude: $longitude1, latitude: $latitude1, level: 3," +
  s" time_zone: '$timezone1', model: '$nodetype1'})-[:BELONGS_TO]->(site)"
  // creating cypher for fixture data in neo4j
  val createFixture = s"MERGE (n:Node:Active {nodeid:'$fixturenodeid'})-[:BELONGS_TO]->(f: Fixture) " +
    s"SET n.nodeid = '$fixturenodeid', n.model = '$nodetype1'," +
    s" f.MinPower0 = 10," +
    s" f.MinPower10 = 30," +
    s" f.MinPower50 = 50, " +
    s" f.MinPower100 = 70," +
    s" f.MaxPower0 = 120, " +
    s" f.MaxPower10 = 140," +
    s" f.MaxPower50 = 160," +
    s" f.MaxPower100 = 180"

// sensor sample event, sensor sample payload and fixture data
  val FixturesensorSamplePayload: SensorPayload = SensorPayload(n = fixturenodeid,
    u = fixtureUnits, s = sensorIdFixture, v = fixtureSensorValue, t = starttimeInMicros)

  val FixturesensorSamplePayloadOverPower: SensorPayload = SensorPayload(n = fixturenodeid,
    u = fixtureUnits, s = sensorIdFixture, v = fixtureSensorValueOverPower, t = starttimeInMicros)

  val FixturesensorSamplePayloadUnderPower: SensorPayload = SensorPayload(n = fixturenodeid,
    u = fixtureUnits, s = sensorIdFixture, v = fixtureSensorValueUnderPower, t = starttimeInMicros)

  val FixturesensorSamplePayloadClearPower: SensorPayload = SensorPayload(n = fixturenodeid,
    u = fixtureUnits, s = sensorIdFixture, v = fixtureSensorValueClearPower, t = starttimeInMicros)

  val FixturesensorSamplePayloadLT: SensorPayload = SensorPayload(n = fixturenodeid,
    u = fixtureUnits, s = "lt", v = fixtureSensorValue, t = starttimeInMicros)

  val FixturesensorSampleEvent: SensorSampleEvent = SensorSampleEvent(UUID.randomUUID().toString,
    fixturenodeid, unsolEventType, sensorIdFixture, eventPath, FixturesensorSamplePayload)

  val FixturesensorSampleEventOverPower: SensorSampleEvent = SensorSampleEvent(UUID.randomUUID().toString,
    fixturenodeid, unsolEventType, sensorIdFixture, eventPath, FixturesensorSamplePayloadOverPower)

  val FixturesensorSampleEventUnderPower: SensorSampleEvent = SensorSampleEvent(UUID.randomUUID().toString,
    fixturenodeid, unsolEventType, sensorIdFixture, eventPath, FixturesensorSamplePayloadUnderPower)

  val FixturesensorSampleEventClear: SensorSampleEvent = SensorSampleEvent(UUID.randomUUID().toString,
    fixturenodeid, unsolEventType, sensorIdFixture, eventPath, FixturesensorSamplePayloadClearPower)

  val FixturesensorSampleEventLT: SensorSampleEvent = SensorSampleEvent(UUID.randomUUID().toString,
    fixturenodeid, unsolEventType, "lt", eventPath, FixturesensorSamplePayloadLT)

  val fixture:Fixture = Fixture(nodeid = fixturenodeid,
                                nodeType = Some(sensorModel),
                                minPower0 = Some("10"),
                                minPower10 = Some("30"),
                                minPower50 = Some("50"),
                                minPower100 = Some("70"),
                                maxPower0 = Some("120"),
                                maxPower10 = Some("140"),
                                maxPower50 = Some("160"),
                                maxPower100 = Some("180"))

}