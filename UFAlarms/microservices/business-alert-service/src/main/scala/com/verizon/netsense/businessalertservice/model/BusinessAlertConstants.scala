package com.verizon.netsense.businessalertservice.model

sealed case class BusinessAlertConstants(value: String)

object BusinessAlertConstants {

  object getBusinessAlert extends BusinessAlertConstants("getBusinessAlert")

  object getAllBusinessAlerts extends BusinessAlertConstants("getAllBusinessAlerts")

  object dismissBusinessAlert extends BusinessAlertConstants("dismissBusinessAlert")

  object filterBusinessAlert extends BusinessAlertConstants("filterBusinessAlert")

  object getBusinessAlertSys extends BusinessAlertConstants("getBusinessAlertSys")

  object rootUser extends BusinessAlertConstants("root")

  object activeKey extends BusinessAlertConstants("active")

  object severityKey extends BusinessAlertConstants("severity")

  object triggerNameKey extends BusinessAlertConstants("triggerName")

  object resourceIdKey extends BusinessAlertConstants("resourceId")

  object ClearKey extends BusinessAlertConstants("Clear")

}
