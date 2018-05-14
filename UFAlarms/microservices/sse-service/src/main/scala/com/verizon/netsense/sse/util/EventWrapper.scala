package com.verizon.netsense.sse.util

import com.verizon.netsense.sse.model._

/**
 * Created by muppasw on 8/29/17.
 */
object EventWrapper {

  /*
   *  Json format SensorSample response publishes to mqtt
   */
  def buildSensorResponse(sensorSampleEvent: SSESensorSampleEvent): SensorSample =
    SensorSample(
      nodeid = sensorSampleEvent.sid,
      orgid = sensorSampleEvent.orgid,
      sensor = sensorSampleEvent.l.s,
      siteid = sensorSampleEvent.siteid,
      // time = TimeConverter.convertNanosToISO(sensorSampleEvent.l.t),
      time = sensorSampleEvent.l.t,
      units = sensorSampleEvent.l.u,
      value = sensorSampleEvent.l.v
    )

  /*
   *  Json format LoginReq response publishes to mqtt
   */
  def buildLoginResponse(sSELoginEvent: SSELoginEvent): LoginReq =
    LoginReq(
      assocChannel = sSELoginEvent.l.ch,
      auth = sSELoginEvent.l.sec,
      bssid = sSELoginEvent.l.bssid,
      clientType = sSELoginEvent.l.dev,
      configToken = sSELoginEvent.l.tok,
      localIP = sSELoginEvent.l.ip,
      mac = sSELoginEvent.l.mac,
      netName = sSELoginEvent.l.ssid,
      nodeid = sSELoginEvent.sid,
      orgid = sSELoginEvent.orgid,
      siteid = sSELoginEvent.siteid,
      swVerId = sSELoginEvent.l.cid,
      time = sSELoginEvent.l.t,
      profileName = sSELoginEvent.l.profile,
      protocolVersion = sSELoginEvent.l.protoc,
      voltageType = "",
      subType = sSELoginEvent.l.subtype,
      modemRevEd = sSELoginEvent.l.modemRevEd
    )

  /*
   *  Json format DeviceAlarm response publishes to mqtt
   */
  def buildAlertResponse(sSEAlarmEvent: SSEAlarmEvent): DeviceAlarm =
    DeviceAlarm(
      alertid = sSEAlarmEvent.alertid,
      msg = sSEAlarmEvent.msg,
      nodeid = sSEAlarmEvent.nodeid,
      orgid = sSEAlarmEvent.orgid,
      siteid = sSEAlarmEvent.siteid,
      severity = sSEAlarmEvent.severity,
      `type` = sSEAlarmEvent.`type`,
      nodehw = sSEAlarmEvent.nodehw,
      ufname = sSEAlarmEvent.ufname,
      description = sSEAlarmEvent.description,
      orgname = sSEAlarmEvent.orgname,
      sitename = sSEAlarmEvent.sitename,
      nodename = sSEAlarmEvent.nodename,
      created = sSEAlarmEvent.created,
      updated = sSEAlarmEvent.updated,
      displaytopartner = sSEAlarmEvent.displaytopartner,
      displaytocustomer = sSEAlarmEvent.displaytocustomer
    )

  def buildBusinessAlertResponse(sSEBusinessAlarmEvent: SSEBusinessAlertEvent): BusinessAlert =
    BusinessAlert(
      businessAlertId = sSEBusinessAlarmEvent.businessAlertId,
      triggerName = sSEBusinessAlarmEvent.triggerName,
      triggerId = sSEBusinessAlarmEvent.triggerId,
      triggerCategory = sSEBusinessAlarmEvent.triggerCategory,
      triggerSubcategory = sSEBusinessAlarmEvent.triggerSubcategory,
      triggerUserId = sSEBusinessAlarmEvent.triggerUserId,
      siteid = sSEBusinessAlarmEvent.siteid,
      orgid = sSEBusinessAlarmEvent.orgid,
      resourceId = sSEBusinessAlarmEvent.resourceId,
      resourceName = sSEBusinessAlarmEvent.resourceName,
      active = sSEBusinessAlarmEvent.active,
      createdOn = sSEBusinessAlarmEvent.createdOn,
      lastUpdated = sSEBusinessAlarmEvent.lastUpdated,
      lastClearedAt = sSEBusinessAlarmEvent.lastClearedAt,
      lastClearedBy = sSEBusinessAlarmEvent.lastClearedBy,
      message = sSEBusinessAlarmEvent.message,
      severity = sSEBusinessAlarmEvent.severity
    )

  /*
   *  Json format ConnectionStatus response publishes to mqtt
   */

  def buildConnectionResponse(sSEConnectionEvent: SSEConnectionEvent): ConnectionStatus = {

    val status = sSEConnectionEvent.l.status match {
      case true => ConnStatus.`true`
      case _    => ConnStatus.`false`
    }
    ConnectionStatus(
      nodeid = sSEConnectionEvent.l.nodeid,
      orgid = sSEConnectionEvent.orgid,
      siteid = sSEConnectionEvent.siteid,
      status = status.toString
    )
  }

  /*
   *  Json format GPS response publishes to mqtt
   */
  def buildGpsResponse(sSEGpsEvent: SSEGpsEvent): GpsSample =
    GpsSample(
      nodeid = sSEGpsEvent.nodeid,
      name = sSEGpsEvent.name,
      orgid = sSEGpsEvent.orgid,
      siteid = sSEGpsEvent.siteid,
      latitude = sSEGpsEvent.latitude,
      longitude = sSEGpsEvent.longitude,
      latUserAdded = sSEGpsEvent.latUserAdded,
      lonUserAdded = sSEGpsEvent.lonUserAdded,
      created = sSEGpsEvent.created,
      updated = sSEGpsEvent.updated
    )

}
