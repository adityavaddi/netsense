package com.verizon.netsense.model

import com.fasterxml.jackson.annotation.{JsonIgnore, JsonProperty}
import com.verizon.netsense.entity.Entity
import org.joda.time.DateTime

/**
 * Created by subrsi9 on 5/18/17.
 */
case class NodeProps(nodeid: String)

case class SiteProps(siteid: String)

case class OrgProps(orgid: String)

case class ExtProps(sensorid: String,
                    date: String,
                    date1: String,
                    date2: Option[String],
                    limit: Int,
                    period: Option[String] = Some("15min"))

case class SensorQueryEnvelope(messageid: String,
                               responsetopic: String,
                               @JsonProperty request: SensorQueryPayload) extends Entity

object SensorQueryEnvelope {

  def getNodeSensorProps(x: SensorQueryEnvelope): Some[(String, String)] =
    Some(x.request.nodeprops.nodeid, x.request.extprops.sensorid)

  def getNodeId(x: SensorQueryEnvelope): String = x.request.nodeprops.nodeid

  def getSiteId(x: SensorQueryEnvelope): String = x.request.siteprops.siteid
}

case class SensorQueryPayload(requestid: String,
                              `type`: String,
                              model: String,
                              action: String,
                              instanceid: String,
                              timestamp: String,
                              user: String,
                              service: String = "unspecified",
                              @JsonProperty nodeprops: NodeProps,
                              @JsonProperty siteprops: SiteProps,
                              @JsonProperty orgprops: OrgProps,
                              @JsonProperty extprops: ExtProps)

case class EnergySavingsNodeModel(siteid: String,
                                  nodeid: String,
                                  aggregation_type: String,
                                  starttime: Long,
                                  actual_energy_consumption: Double,
                                  enddt: String,
                                  endtime: Long,
                                  led_energy_consumption: Double,
                                  legacy_energy_consumption: Double,
                                  savings_legacy_vs_actual: Double,
                                  savings_legacy_vs_led: Double,
                                  startday: String,
                                  startdt: String,
                                  starthr: String,
                                  ts: Long)

case class EnergySavingsSiteModel(siteid: String,
                                  aggregation_type: String,
                                  starttime: Long,
                                  startdt: String,
                                  actual_energy_consumption: Double,
                                  led_energy_consumption: Double,
                                  legacy_energy_consumption: Double,
                                  savings_legacy_vs_actual: Double,
                                  savings_legacy_vs_led: Double)

case class SensorSampleQueryResponse(messageid: String,
                                     @JsonProperty response: SensorSampleQueryPayload) extends Entity

case class SensorSampleQueryPayload(requestid: String,
                                    success: Boolean = false,
                                    timestamp: String,
                                    error: String,
                                    status: Int = 200,
                                    sensorid: String = null,
                                    @JsonProperty items: Vector[SensorSamplePayload])

case class SensorEnergyNodeResponse(messageid: String, @JsonProperty response: SensorEnergyNodePayload) extends Entity

case class SensorEnergyNodePayload(requestid: String,
                                   success: Boolean,
                                   timestamp: String,
                                   @JsonProperty items: List[EnergySavingsNodePayloadItem])

case class SensorEnergySiteResponse(messageid: String, @JsonProperty response: SensorEnergySitePayload) extends Entity

/*
trait SensorResult

case class SuccessSensorResponse(requestid: String,
                                 success: Boolean,
                                 timestamp: String,
                                 @JsonProperty items: List[EnergySavingsSitePayloadItem]) extends SensorResult

case class FailureSensorResponse(requestid: String,
                                 success: Boolean,
                                 timestamp: String) extends SensorResult*/

case class SensorEnergySitePayload(requestid: String,
                                   success: Boolean,
                                   timestamp: String,
                                   @JsonProperty items: List[EnergySavingsSitePayloadItem])


case class SensorSamplePayload(sensorid: String, time: String, value: Double)

case class EnergySavingsSitePayloadItem(siteid: String,
                                        startdate: String,
                                        legacy_energy_consumption: Double,
                                        led_energy_consumption: Double,
                                        actual_energy_consumption: Double,
                                        savings_legacy_vs_led: Double,
                                        savings_legacy_vs_actual: Double)

case class EnergySavingsNodePayloadItem(siteid: String,
                                        nodeid: String,
                                        startdate: String,
                                        legacy_energy_consumption: Double,
                                        led_energy_consumption: Double,
                                        actual_energy_consumption: Double,
                                        savings_legacy_vs_led: Double,
                                        savings_legacy_vs_actual: Double)

case class Light(nodeid: String                   = "",
                 driver: Option[Int]              = None,
                 harvest_trigger: Option[Boolean] = None,
                 isscheduled: Option[Boolean]     = None,
                 policy: Option[String]           = None,
                 priority: Option[Int]            = None,
                 startdt: Option[DateTime]        = None) extends Entity

case class NodeStatus(name: String            = "",
                      nodeId: String          = "",
                      orgId: Option[String]   = None,
                      siteId: Option[String]  = None,
                      netStat: Option[Int]    = None,
                      ligStat: Option[String] = None,
                      senStat: Option[String] = None) extends Entity