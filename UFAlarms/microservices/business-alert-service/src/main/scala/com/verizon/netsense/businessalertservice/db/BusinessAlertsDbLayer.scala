package com.verizon.netsense.businessalertservice.db

import com.verizon.netsense.utils.Logging
import com.verizon.netsense.businessalertservice.model.{BusinessAlert, BusinessAlertHistory, DBTrigger, TriggerResource}
import com.verizon.netsense.businessalertservice.model.BusinessAlertConstants._
import com.verizon.netsense.businessalertservice.util.ObjectMapperUtil

import scala.concurrent.{ExecutionContext, Future}

/**
 * Business Alert Dblayer
 * Created by Rajitha on 15/02/18.
 */
class BusinessAlertsDbLayer(phantomService: BAPhantomService) extends Logging {
  implicit val ec = ExecutionContext.Implicits.global

  def getAllBusinessAlerts(orgid: String, siteid: String): Future[List[BusinessAlert]] = {
    val triggers = phantomService.getAllBusinessTriggersInOrgSite(orgid, siteid)
    val alerts   = phantomService.getAllBusinessAlerts(orgid, siteid)

    val result: Future[(List[DBTrigger], List[BusinessAlert])] = for {
      triggerResult <- triggers
      alertResult   <- alerts
    } yield (triggerResult, alertResult)

    result.map(triggerAlertTuple => {
      val liveTriggers             = triggerAlertTuple._1.filterNot(_.isdeleted)
      val possibleTriggerResources = buildAllTriggerResourceTuples(liveTriggers)
      val bAlerts = possibleTriggerResources.foldLeft(List[BusinessAlert]())((acc, tup) => {
        filterAndFindValidAlert(tup, triggerAlertTuple._2) match {
          case Some(alert) => acc :+ alert
          case None        => acc
        }
      })
      bAlerts.sortWith(_.lastUpdated.getOrElse(0l) > _.lastUpdated.getOrElse(0l))
    })
  }

  private[this] def filterAndFindValidAlert(triggerResourceTuple: (String, String),
                                            alerts: List[BusinessAlert]): Option[BusinessAlert] = {
    val filteredAlerts = alerts.filter(
      alert =>
        alert.triggerId.getOrElse("") == triggerResourceTuple._1 && alert.resourceId
          .getOrElse("") == triggerResourceTuple._2
    )
    filteredAlerts.sortWith(_.lastUpdated.getOrElse(0l) > _.lastUpdated.getOrElse(0l)).headOption
  }

  private[this] def buildAllTriggerResourceTuples(dBTrigger: List[DBTrigger]): List[(String, String)] =
    dBTrigger.flatMap(trigger => buildTriggerResourceTuples(trigger))

  private[this] def buildTriggerResourceTuples(dBTrigger: DBTrigger): List[(String, String)] =
    convertStringToResourceList(dBTrigger.resourcelistjson, dBTrigger.triggerid)
      .map(resource => (dBTrigger.triggerid, resource.resourceId))

  private[this] val isEmpty: String => Boolean = x => x == null || x.trim.isEmpty

  private[this] def convertStringToResourceList(resourceJson: String, triggerid: String): List[TriggerResource] =
    try {
      if (isEmpty(resourceJson)) List[TriggerResource]()
      else ObjectMapperUtil.fromJson[List[TriggerResource]](resourceJson)
    } catch {
      case ex: Exception =>
        log.error(s"Unable to unmarshall resourceJson $resourceJson for triggerid $triggerid")
        List[TriggerResource]()
    }

  def dismissByBusinesssAlertId(orgid: String,
                                siteid: String,
                                businessAlertId: String,
                                severity: String,
                                userId: String,
                                lastClearedAt: Long): Future[Boolean] =
    phantomService
      .dismissByBusinessAlertId(orgid, siteid, businessAlertId, severity, userId, lastClearedAt)
      .map(
        x => x.wasApplied()
      )

  def storeBusinessAlertHistory(businessAlert: BusinessAlertHistory): Future[Boolean] =
    phantomService
      .storeBusinessAlertHistory(businessAlert)
      .map(
        x => x.wasApplied()
      )

  def getBusinessAlertById(orgid: String, siteid: String, businessAlertId: String): Future[Option[BusinessAlert]] =
    phantomService
      .getBusinessAlertById(orgid, siteid, businessAlertId)

  def getBusinessAlertByIdSys(orgid: String, siteid: String, businessAlertId: String): Future[Option[BusinessAlert]] =
    phantomService
      .getBusinessAlertById(orgid, siteid, businessAlertId)
      .flatMap {
        case Some(alert) =>
          validateTriggerAndReturnAlert(orgid, siteid, alert)
        case None => Future { None }
      }

  private[this] def validateTriggerAndReturnAlert(orgid: String,
                                                  siteid: String,
                                                  alert: BusinessAlert): Future[Option[BusinessAlert]] =
    phantomService
      .getBusinessTriggerByIdInOrgSite(orgid, siteid, alert.triggerId.getOrElse(""))
      .map {
        case Some(trigger) => if (trigger.isdeleted) None else Some(alert)
        case None          => None
      }

  def searchBusinessAlert(orgid: String, siteid: String, key: String, value: String): Future[List[BusinessAlert]] =
    key match {
      case severityKey.value =>
        getAllBusinessAlerts(orgid, siteid).map(
          ba => ba.filter(businessAlert => businessAlert.severity.getOrElse("").equalsIgnoreCase(value))
        )
      case triggerNameKey.value =>
        getAllBusinessAlerts(orgid, siteid).map(
          ba => ba.filter(businessAlert => businessAlert.triggerName.getOrElse("").equalsIgnoreCase(value))
        )
      case resourceIdKey.value =>
        getAllBusinessAlerts(orgid, siteid).map(
          ba => ba.filter(businessAlert => businessAlert.resourceId.getOrElse("").equalsIgnoreCase(value))
        )
      case activeKey.value =>
        getAllBusinessAlerts(orgid, siteid).map(
          ba => ba.filter(businessAlert => businessAlert.active.getOrElse(false) == value.trim.toBoolean)
        )

    }
}

object BusinessAlertsDbLayer {
  def apply(): BusinessAlertsDbLayer = new BusinessAlertsDbLayer(new BAPhantomService with ProductionDatabase {})
}
