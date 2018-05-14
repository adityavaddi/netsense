package com.verizon.netsense.services.alert.model

import java.util.UUID

import com.fasterxml.jackson.annotation.JsonProperty
import com.verizon.netsense.entity.Entity
import com.verizon.netsense.services.alert.customexceptions.OrgDataNotFoundException

/**
 * Created by vermri5 on 7/12/17.
 */
case class Alert(@JsonProperty("alertid") alertId: String,
                 name: Option[String] = Some("DeviceAlarm"),
                 msg: Option[String] = None,
                 @JsonProperty("nodeid") nodeId: String,
                 @JsonProperty("orgid") orgId: String,
                 severity: Option[String] = None,
                 `type`: String,
                 category: Option[String] = None,
                 created: Option[String] = None,
                 updated: Option[String] = None,
                 @JsonProperty("sitename") siteName: Option[String] = None,
                 @JsonProperty("nodename") nodeName: Option[String] = None,
                 @JsonProperty("orgname") orgName: Option[String] = None,
                 @JsonProperty("siteaddress") siteAddress: Option[String] = None,
                 @JsonProperty("siteid") siteId: String,
                 @JsonProperty("bssid") bssId: Option[String] = None,
                 @JsonProperty("nodehw") nodeHw: Option[String] = None,
                 active: Boolean)
    extends Entity

object Alert {

  def apply(alarmEvent: AlarmEvent, orgHrchy: OrgHierarchy): Alert = {
    val alarmType = if (alarmEvent.l.a.equalsIgnoreCase("lwt")) "Disconnect" else alarmEvent.l.a
    Alert(
      UUID.randomUUID().toString,
      Some("DeviceAlarm"),
      Some(alarmEvent.l.m),
      alarmEvent.sid,
      orgHrchy.orgId.getOrElse(throw new OrgDataNotFoundException),
      Some(AlarmSeverity(alarmEvent.l.s).toString),
      alarmType,
      Some(AlarmCategory(alarmEvent.l.c).toString),
      Some(alarmEvent.d),
      Some(alarmEvent.d),
      orgHrchy.siteName,
      orgHrchy.nodeName,
      orgHrchy.orgName,
      orgHrchy.siteAddress,
      orgHrchy.siteId.getOrElse(throw new OrgDataNotFoundException),
      orgHrchy.bssId,
      orgHrchy.nodeHw,
      true
    )
  }
}

case class AlertResponse(@JsonProperty("alertid") alertId: String,
                         name: Option[String] = Some("DeviceAlarm"),
                         msg: Option[String] = None,
                         @JsonProperty("nodeid") nodeId: String,
                         @JsonProperty("orgid") orgId: String,
                         severity: Option[String] = None,
                         `type`: String,
                         category: Option[String] = None,
                         created: Option[String] = None,
                         updated: Option[String] = None,
                         @JsonProperty("sitename") siteName: Option[String] = None,
                         @JsonProperty("nodename") nodeName: Option[String] = None,
                         @JsonProperty("orgname") orgName: Option[String] = None,
                         @JsonProperty("siteaddress") siteAddress: Option[String] = None,
                         @JsonProperty("siteid") siteId: String,
                         @JsonProperty("bssid") bssId: Option[String] = None,
                         @JsonProperty("nodehw") nodeHw: Option[String] = None,
                         active: Boolean,
                         @JsonProperty("ufname") ufName: String,
                         description: String,
                         @JsonProperty("displaytopartner") displayToPartner: Boolean = false,
                         @JsonProperty("displaytocustomer") displayToCustomer: Boolean = false)
    extends Entity

object AlertResponse {
  def apply(alert: Alert, ufAlarm: UfAlarm): AlertResponse =
    AlertResponse(
      alert.alertId,
      alert.name,
      alert.msg,
      alert.nodeId,
      alert.orgId,
      alert.severity,
      alert.`type`,
      alert.category,
      alert.created,
      alert.updated,
      alert.siteName,
      alert.nodeName,
      alert.orgName,
      alert.siteAddress,
      alert.siteId,
      alert.bssId,
      alert.nodeHw,
      alert.active,
      ufAlarm.ufName.getOrElse(alert.`type`),
      ufAlarm.description.getOrElse(alert.msg.getOrElse("")),
      ufAlarm.displayToPartner,
      ufAlarm.displayToCustomer
    )
}
