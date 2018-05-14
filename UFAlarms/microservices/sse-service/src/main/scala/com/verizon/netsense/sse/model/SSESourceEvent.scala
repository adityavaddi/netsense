package com.verizon.netsense.sse.model

import java.util.Calendar

import com.fasterxml.jackson.annotation.JsonProperty
import com.verizon.netsense.entity.Entity
import com.verizon.netsense.model.SensorSampleEvent

/**
 * Created by muppasw on 8/29/17.
 */
case class SSESensorSampleEvent(a: String,
                                p: String,
                                f: String,
                                sid: String,
                                uuid: String,
                                @JsonProperty l: SSESensorSamplePayload)
    extends SSEEvent
    with Entity
    with DeviceEvent {
  override var nodeid: String = sid
  override var orgid: String  = _
  override var siteid: String = _
  override var name: String   = _
}

object SSESensorSampleEvent {

  def apply(s: SensorSampleEvent): SSESensorSampleEvent =
    SSESensorSampleEvent(s.a, s.p, s.f, s.sid, s.uuid, SSESensorSamplePayload(s.l.u, s.l.s, s.l.v, s.l.t))
}

case class SSESensorSamplePayload(u: String, s: String, v: BigDecimal, t: Long)

case class SSELoginEvent(a: String,
                         p: String,
                         f: String,
                         sid: String,
                         uuid: String,
                         d: String = Calendar.getInstance().toInstant.toString,
                         @JsonProperty l: LoginReqPayload)
    extends SSEEvent
    with Entity
    with DeviceEvent {
  override var nodeid: String = _
  override var orgid: String  = _
  override var siteid: String = _
  override var name: String   = _
}

case class LoginReqPayload(ch: Int,
                           sec: String,
                           bssid: String,
                           dev: String,
                           tok: String,
                           ip: String,
                           mac: String,
                           ssid: String,
                           nid: String,
                           cid: String,
                           t: Long,
                           subtype: String,
                           protoc: Int,
                           profile: String,
                           modemRevEd: String)
    extends SSEEvent
    with Entity
    with DeviceEvent {
  override var nodeid: String = _
  override var orgid: String  = _
  override var siteid: String = _
  override var name: String   = _
}

case class SSEAlarmEvent(alertid: String,
                         msg: String,
                         severity: String,
                         `type`: String,
                         nodehw: String,
                         orgname: String,
                         sitename: String,
                         nodename: String,
                         created: String,
                         updated: String,
                         ufname: String,
                         description: String,
                         displaytopartner: Boolean,
                         displaytocustomer: Boolean)
    extends SSEEvent
    with Entity
    with DeviceEvent {
  override var nodeid: String = _
  override var orgid: String  = _
  override var siteid: String = _
  override var name: String   = _
}

case class SSEGpsEvent(latitude: Option[Double],
                       longitude: Option[Double],
                       latUserAdded: Option[Double],
                       lonUserAdded: Option[Double],
                       created: Option[Long],
                       updated: Option[Long])
    extends SSEEvent
    with Entity
    with DeviceEvent {
  override var nodeid: String = _
  override var orgid: String  = _
  override var siteid: String = _
  override var name: String   = _
}

case class SSEBusinessAlertEvent(
    businessAlertId: String,
    triggerName: String,
    triggerSubcategory: String,
    triggerUserId: String,
    resourceId: String,
    resourceName: String,
    active: Boolean,
    createdOn: String,
    lastUpdated: String,
    lastClearedAt: String,
    lastClearedBy: String,
    message: String,
    severity: String
) extends SSEEvent
    with Entity
    with BusinessAlertEvent {
  override var triggerId: String       = _
  override var siteid: String          = _
  override var orgid: String           = _
  override var triggerCategory: String = _
}

case class SSEConnectionEvent(a: String,
                              p: String,
                              f: String,
                              sid: String,
                              uuid: String,
                              d: String = Calendar.getInstance().toInstant.toString,
                              l: ConnectionStatusPayload)
    extends SSEEvent
    with Entity
    with DeviceEvent {
  override var nodeid: String = _
  override var orgid: String  = _
  override var siteid: String = _
  override var name: String   = _
}

case class ConnectionStatusPayload(override var nodeid: String, status: Boolean)
    extends SSEEvent
    with Entity
    with DeviceEvent {
  override var orgid: String  = _
  override var siteid: String = _
  override var name: String   = _
}
