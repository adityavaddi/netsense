package com.verizon.netsense.services.alert.helper

import com.verizon.netsense.services.alert.customexceptions._
import com.verizon.netsense.services.alert.model.{AlarmCategory, AlarmEvent, AlarmSeverity}
import com.verizon.netsense.utils.Logging

object AlarmValidator extends Logging {

  def validateEnums(a: String, s: Int, c: Int) =
    try {
      AlarmSeverity(s)
      AlarmCategory(c)
    } catch {
      case ex: Exception =>
        throw new AlarmNotCreatedException(
          "Invalid Alarm Type or Severity or Category received in the Alarm payload: " + a + ", " + s + ", " + c
        )
    }

  def validationPredicate(alarm: AlarmEvent): Boolean = {
    log.debug("AlarmEvent: " + alarm)
    if (alarm.l != null && alarm.l.a != null && alarm.l.s != null &&
        alarm.l.m != null && alarm.l.c != null && alarm.sid != null && alarm.d != null) {
      validateEnums(alarm.l.a, alarm.l.s, alarm.l.c)
      true
    } else {
      throw new AlarmNotCreatedException("Invalid alarm payload received: " + alarm.toJSON)
      false
    }
  }
}
