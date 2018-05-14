package com.verizon.netsense.services.datamigration.model

import java.time.Instant
import java.util.UUID

import com.verizon.netsense.services.datamigration.migration.InvalidUFAlarmException


case class RawUFAlarm(mappingid: Option[String] = Some(UUID.randomUUID().toString),
                      alarmtype: Option[String] = None,
                      nodemodels: Set[String] = Set.empty,
                      ufname: Option[String] = None,
                      description: Option[String] = None,
                      displaytopartner: Option[Boolean] = Some(false),
                      displaytocustomer: Option[Boolean] = Some(false),
                      created: Option[Long] = Some(Instant.now.getEpochSecond),
                      updated: Option[Long] = Some(Instant.now.getEpochSecond))

case class UfAlarm(mappingid: String,
                   alarmtype: String,
                   nodemodels: Set[String] = Set.empty,
                   ufname: String,
                   description: String = "",
                   displaytopartner: Boolean = false,
                   displaytocustomer: Boolean = false,
                   created: Long = Instant.now.getEpochSecond,
                   updated: Long = Instant.now.getEpochSecond)

object UfAlarm {
  def apply(ufAlarm: RawUFAlarm): UfAlarm = {
    val alarmType = ufAlarm.alarmtype.getOrElse(
      throw new InvalidUFAlarmException(s"Invalid UF Alarm payload $ufAlarm"))
    new UfAlarm(
      ufAlarm.mappingid.getOrElse(UUID.randomUUID().toString),
      alarmType.trim,
      ufAlarm.nodemodels,
      ufAlarm.ufname.getOrElse(alarmType).trim,
      ufAlarm.description.getOrElse(""),
      ufAlarm.displaytopartner.getOrElse(false),
      ufAlarm.displaytocustomer.getOrElse(false),
      ufAlarm.created.getOrElse(Instant.now.getEpochSecond),
      ufAlarm.updated.getOrElse(Instant.now.getEpochSecond)
    )
  }
}
