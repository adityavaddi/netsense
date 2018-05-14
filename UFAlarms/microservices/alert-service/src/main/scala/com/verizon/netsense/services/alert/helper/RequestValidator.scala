package com.verizon.netsense.services.alert.helper

import com.verizon.netsense.services.alert.customexceptions._
import com.verizon.netsense.services.alert.model.CaselType.{GET_ALL_UF_ALARMS, GET_NOTIFICATION_TYPES}
import com.verizon.netsense.services.alert.model._
import com.verizon.netsense.services.alert.util.ObjectMapperUtil
import com.verizon.netsense.utils.Logging

object RequestValidator extends Logging {

  def validateRequestType(t: String): Unit =
    if (!CaselType.allRequestTypes.contains(t))
      throw new InvalidRequestPayloadException("Invalid API type passed in the Request payload: " + t)

  def validateProps(m: String,
                    t: String,
                    org: Option[OrgProps],
                    site: Option[SiteProps],
                    node: Option[NodeProps],
                    aprops: Option[AlertProps],
                    nprops: Option[NotificationProps],
                    ufAlarmProps: Option[UfAlarmProps]): Boolean = {
    validateRequestType(t)
    if (m.equalsIgnoreCase("UFAlarmModel")) {
      if (!List(GET_ALL_UF_ALARMS.value).contains(t))
        ufAlarmProps.getOrElse(
          throw new InvalidRequestPayloadException("UF Alarm props not found in the Request payload.")
        )
      true
    } else if (t.equalsIgnoreCase(GET_NOTIFICATION_TYPES.value)) true
    else {
      (org, site, node) match {
        case (None, _, _) =>
          throw new InvalidRequestPayloadException("Org props not found in the Request payload.")
        case (_, None, _) =>
          throw new InvalidRequestPayloadException("Site props not found in the Request payload.")
        case (_, _, None) =>
          if (t.equalsIgnoreCase(CaselType.GET_ALERTS_FOR_NODE_ID.value))
            throw new InvalidRequestPayloadException("Node props not found in the Request payload.")
        case _ => true
      }

      (t, aprops, nprops) match {
        case (CaselType.ADD_ALERT.value, None, _) | (CaselType.UPDATE_ALERT.value, None, _) |
            (CaselType.GET_ALERT_SYS.value, None, _) | (CaselType.GET_ALERT_FOR_ALERT_ID.value, None, _) |
            (CaselType.DISMISS_ALERT.value, None, _) | (CaselType.DELETE_ALERT.value, None, _) =>
          throw new InvalidRequestPayloadException("Alert props not found in the Request payload.")
        case (CaselType.ADD_NOTIFICATION.value, _, None) | (CaselType.UPDATE_NOTIFICATION.value, _, None) |
            (CaselType.NOTIFICATION_FOR_ID.value, _, None) | (CaselType.NOTIFICATION_SYS.value, _, None) |
            (CaselType.NOTIFICATION_FOR_NAME.value, _, None) | (CaselType.DELETE_NOTIFICATION.value, _, None) =>
          throw new InvalidRequestPayloadException("Notification props not found in the Request payload.")
        case _ => true
      }
    }
  }

  def validationPredicate(appRequest: AppRequest): Boolean =
    if (appRequest.responsetopic != null && appRequest.request != null
        && appRequest.request.model != null && appRequest.request.`type` != null) {
      validateProps(
        appRequest.request.model,
        appRequest.request.`type`,
        appRequest.request.orgprops,
        appRequest.request.siteprops,
        appRequest.request.nodeprops,
        appRequest.request.alertprops,
        appRequest.request.notificationprops,
        appRequest.request.ufalarmprops
      )
      true
    } else {
      throw new InvalidRequestPayloadException(
        "Invalid request payload received: " + ObjectMapperUtil.toJson(appRequest)
      )
      false
    }
}
