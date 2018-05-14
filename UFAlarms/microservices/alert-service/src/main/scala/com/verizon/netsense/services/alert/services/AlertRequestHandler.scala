package com.verizon.netsense.services.alert.services

import com.verizon.netsense.services.alert.customexceptions._
import com.verizon.netsense.services.alert.database.PhantomConnector
import com.verizon.netsense.services.alert.model.CaselType._
import com.verizon.netsense.services.alert.model._
import com.verizon.netsense.utils.Logging

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/**
 * Created by vermri5 on 8/4/17.
 */
class AlertRequestHandler(implicit ec: ExecutionContext) extends PhantomConnector with JsonUtils with Logging {

  val alertsSuccessResponse = successResponse[List[AlertResponse]]
  val alertSuccessResponse  = successResponse[AlertResponse]
  val statusResponse        = successResponse[Status]

  def getAppResponse(appRequest: AppRequest): Future[AppResponse] = {

    import appRequest.request._
    val orgId: String = orgprops match {
      case Some(x) => x.orgId
      case None    => throw new AlertIdMissingException("OrgId is missing in the Payload")
    }
    val siteId: String = siteprops match {
      case Some(x) => x.siteId
      case None    => throw new AlertIdMissingException("SiteId is missing in the Payload")
    }
    val nodeId: String = nodeprops match {
      case Some(x) => x.nodeId
      case None    => ""
    }
    val alertId: String = alertprops match {
      case Some(x) => x.alertId.getOrElse("")
      case None    => ""
    }

    `type` match {
      case ADD_ALERT.value =>
        val alertPayload = Try { alertprops.get.toAlert(orgId, siteId) }.toEither match {
          case Left(ex) => throw ex
          case Right(u) => u
        }
        lookupAndAddAlert(alertPayload).flatMap {
          case Some(alert) => getUFName(alert).map(alertSuccessResponse(_, appRequest))
          case None        => failureResponse(s"Unable to create Alert: $alertprops", appRequest).future()
        }
      case DELETE_ALERT.value =>
        deleteAlert(alertId, orgId, siteId).map {
          case true  => statusResponse(Status(true), appRequest)
          case false => failureResponse(s"Alert with $alertId not found", appRequest)
        }
      case DISMISS_ALERT.value =>
        dismissAlert(alertId, orgId, siteId).map {
          case true  => statusResponse(Status(true), appRequest)
          case false => failureResponse(s"Alert with $alertId not found", appRequest)
        }
      case GET_ALERT_FOR_ALERT_ID.value =>
        getAlertByAlertId(alertId, orgId, siteId).flatMap {
          case Some(alert) => getUFName(alert).map(alertSuccessResponse(_, appRequest))
          case None        => failureResponse(s"No Alerts found for $alertId", appRequest).future()
        }
      case GET_ALERT_SYS.value =>
        getActiveAlert(alertId, orgId, siteId).flatMap {
          case Some(alert) => getUFName(alert).map(alertSuccessResponse(_, appRequest))
          case None        => failureResponse(s"No Active Alerts found for $alertId", appRequest).future()
        }
      case GET_ALERTS_FOR_NODE_ID.value =>
        val userType: String = userprops match {
          case Some(x) => x.userType.getOrElse(throw new UserTypeMissingException)
          case None    => throw new UserTypeMissingException
        }
        getAlertByNode(orgId, siteId, nodeId).flatMap {
          filterAlerts(_, userType).map(alertsSuccessResponse(_, appRequest))
        }
      case GET_ALERTS_FOR_ORG_ID_AND_SITE_ID.value =>
        val userType: String = userprops match {
          case Some(x) => x.userType.getOrElse(throw new UserTypeMissingException)
          case None    => throw new UserTypeMissingException
        }
        getAlertByOrgAndSite(orgId, siteId).flatMap {
          filterAlerts(_, userType).map(alertsSuccessResponse(_, appRequest))
        }
      case UPDATE_ALERT.value =>
        val alertPayload = Try {
          alertprops.get.updateAlert(orgId, siteId)
        }.toEither match {
          case Left(ex) => throw ex
          case Right(u) => u
        }
        updateAlert(alertPayload).flatMap {
          case Some(alert) => getUFName(alert).map(alertSuccessResponse(_, appRequest))
          case None        => failureResponse(s"Unable to update Alert: ${alertPayload.toJSON}", appRequest).future()
        }
      case _ => throw new InvalidRequestPayloadException("Invalid AppRequest type" + appRequest.request.`type`)
    }
  }

  def getUFName(alert: Alert): Future[AlertResponse] = {
    val defaultUfAlarm = UfAlarm("", alert.`type`, ufName = Some(alert.`type`), description = alert.msg)
    get_UfAlarmByType(alert.`type`).map(_.getOrElse(defaultUfAlarm)).map {
      AlertResponse.apply(alert, _)
    }
  }

  def filterAlerts(alerts: List[Alert], userType: String): Future[List[AlertResponse]] = {
    val ufAlarmList: Future[List[UfAlarm]] = userType.toLowerCase match {
      case "partner"  => get_UfAlarmsForPartner
      case "customer" => get_UfAlarmsForCustomer
      case "sensity"  => get_AllUfAlarms
      case _          => throw new UserTypeMissingException(s"Invalid user type: $userType")
    }

    ufAlarmList.map { uf =>
      val validTypeList               = uf.map(_.alarmType)
      val validAlerts                 = alerts.filter(alert => validTypeList.contains(alert.`type`))
      val ufMap: Map[String, UfAlarm] = uf.map(ufa => (ufa.alarmType, ufa)).toMap
      for {
        alert <- validAlerts
        ufAlarm = ufMap.getOrElse(alert.`type`,
                                  UfAlarm("", alert.`type`, ufName = Some(alert.`type`), description = alert.msg))
      } yield AlertResponse(alert, ufAlarm)
    }
  }

  def lookupAndAddAlert(alert: Alert) = {
    log.debug("Add Alert for " + alert.toJSON)
    checkExistingAlert(alert.nodeId, alert.`type`, alert.orgId, alert.siteId)
      .flatMap {
        case Some(record) =>
          throw new AlertExistsException(
            s"Alert with NodeId: ${record.nodeId} and type : ${record.`type`} already exists."
          )
        case None => persistAlert(alert)
      }
  }

  def updateAlert(alert: Alert): Future[Option[Alert]] = {
    log.debug("Update Alert for " + alert.toJSON)
    update_Alert(alert)
  }

  def dismissAlert(alertId: String, orgId: String, siteId: String): Future[Boolean] = {
    log.debug("Dismiss Alert " + alertId)
    dismiss_Alert(alertId, orgId, siteId)
  }

  def deleteAlert(alertId: String, orgId: String, siteId: String): Future[Boolean] = {
    log.debug("Delete Alert " + alertId)
    delete_Alert(alertId, orgId, siteId)
  }

  def getAlertByAlertId(alertId: String, orgId: String, siteId: String): Future[Option[Alert]] = {
    log.debug("Get Alert " + alertId)
    getAlert(alertId, orgId, siteId)
  }

  def getActiveAlert(alertId: String, orgId: String, siteId: String): Future[Option[Alert]] = {
    log.debug("Get Alert For Notification " + alertId)
    getAlertSys(alertId, orgId, siteId)
  }

  def getAlertByOrgAndSite(orgId: String, siteId: String): Future[List[Alert]] = {
    log.debug("Get Alerts for Org " + orgId + " and Site " + siteId)
    getAlertsForSite(orgId, siteId)
  }

  def getAlertByNode(orgId: String, siteId: String, nodeId: String): Future[List[Alert]] = {
    log.debug("Get Alerts for Node " + nodeId)
    getAlertsForNode(orgId, siteId, nodeId)
  }
}

object AlertRequestHandler {
  def apply(implicit ec: ExecutionContext): AlertRequestHandler =
    new AlertRequestHandler()(ec)
}
