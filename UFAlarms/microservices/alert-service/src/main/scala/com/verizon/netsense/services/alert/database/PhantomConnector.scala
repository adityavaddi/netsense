package com.verizon.netsense.services.alert.database

import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.services.alert.customexceptions._
import com.verizon.netsense.services.alert.model._
import com.verizon.netsense.utils.Logging
import nl.grons.metrics.scala.Timer

import scala.concurrent.{ExecutionContext, Future}

trait PhantomConnector extends ProductionDatabase with Instrumented with Logging {

  private[this] val persistAlarmTimer: Timer        = metrics.timer("persistAlarm-timer")
  private[this] val lookupAlertTimer: Timer         = metrics.timer("lookupAlert-timer")
  private[this] val persistAlertTimer: Timer        = metrics.timer("persistAlert-timer")
  private[this] val persistAlertByNodeTimer: Timer  = metrics.timer("persistAlertByNode-timer")
  private[this] val lookupOrgTimer: Timer           = metrics.timer("lookupOrg-timer")
  private[this] val lookupAlertByNodeIdTimer: Timer = metrics.timer("lookupAlertByNode-timer")
  private[this] val alertUpdateTimer: Timer         = metrics.timer("updateAlert-timer")
  private[this] val alertUpdateByNodeIdTimer: Timer = metrics.timer("updateAlertByNode-timer")
  private[this] val alertLookupTimer: Timer         = metrics.timer("getAlert-timer")
  private[this] val alertLookupSysTimer: Timer      = metrics.timer("getAlertSys-timer")
  private[this] val alertDeleteTimer: Timer         = metrics.timer("deleteAlert-timer")
  private[this] val alertDismissTimer: Timer        = metrics.timer("dismissAlert-timer")
  private[this] val alertDeleteByNodeTimer: Timer   = metrics.timer("deleteAlertByNode-timer")
  private[this] val alertDismissByNodeTimer: Timer  = metrics.timer("dismissAlertByNode-timer")
  private[this] val alertLookupBySiteTimer: Timer   = metrics.timer("getAlertBySite-timer")
  private[this] val alertLookupByNodeTimer: Timer   = metrics.timer("getAlertByNode-timer")

  private[this] val notificationPersistTimer: Timer      = metrics.timer("createNotification-timer")
  private[this] val notificationLookupTimer: Timer       = metrics.timer("getNotification-timer")
  private[this] val notificationLookupBySiteTimer: Timer = metrics.timer("getNotificationBySite-timer")
  private[this] val notificationLookupByNameTimer: Timer = metrics.timer("getNotificationByName-timer")
  private[this] val notificationDeleteTimer: Timer       = metrics.timer("deleteNotification-timer")
  private[this] val notificationUpdateTimer: Timer       = metrics.timer("updateNotification-timer")
  private[this] val notificationActivateTimer: Timer     = metrics.timer("activateNotification-timer")
  private[this] val notificationDeActivateTimer: Timer   = metrics.timer("deActivateNotification-timer")

  private[this] val ufAlarmPersistTimer: Timer       = metrics.timer("createUFAlarm-timer")
  private[this] val ufAlarmPersistByIdTimer: Timer   = metrics.timer("createUFAlarmById-timer")
  private[this] val ufAlarmLookupTimer: Timer        = metrics.timer("getAllUFAlarms-timer")
  private[this] val ufAlarmLookupByIdTimer: Timer    = metrics.timer("getUFAlarm-timer")
  private[this] val ufAlarmLookupByTypeTimer: Timer  = metrics.timer("getUFAlarmByType-timer")
  private[this] val ufAlarmForCustomerTimer: Timer   = metrics.timer("getUFAlarmForCustomer-timer")
  private[this] val ufAlarmForPartnerTimer: Timer    = metrics.timer("getUFAlarmForPartner-timer")
  private[this] val getNotificationTypesTimer: Timer = metrics.timer("getNotificationTypes-timer")
  private[this] val ufAlarmDeleteTimer: Timer        = metrics.timer("deleteUFAlarm-timer")
  private[this] val ufAlarmDeleteByIdTimer: Timer    = metrics.timer("deleteUFAlarmById-timer")
  private[this] val ufAlarmUpdateTimer: Timer        = metrics.timer("updateUFAlarm-timer")
  private[this] val ufAlarmUpdateByIdTimer: Timer    = metrics.timer("updateUFAlarmById-timer")

  def checkExistingAlert(nodeId: String, alarmType: String, orgId: String, siteId: String)(
      implicit ec: ExecutionContext
  ): Future[Option[Alert]] =
    lookupAlertTimer
      .time(database.AlertsByNode.getByNodeIdAndSiteId(nodeId, alarmType, orgId, siteId))
      .recover {
        case ex: Exception =>
          throw new CassandraDBException("Failed to get Alert from Mappings table for node: "
                                         + nodeId + ", type: " + alarmType + ex.getMessage,
                                         ex.getCause)
      }

  def checkExistingAlertsForNode(nodeId: String,
                                 alarmType: String)(implicit ec: ExecutionContext): Future[List[Alert]] =
    lookupAlertByNodeIdTimer
      .time(database.AlertsByNode.getByNodeId(nodeId))
      .recover {
        case ex: Exception =>
          throw new CassandraDBException("Failed to check Mappings table for node: "
                                         + nodeId + ", type: " + alarmType + ex.getMessage,
                                         ex.getCause)
      }
      .map {
        _.filter(_.`type`.equalsIgnoreCase(alarmType))
      }

  def persistAlarm(alarm: ApAlarm)(implicit ec: ExecutionContext) =
    persistAlarmTimer.time(CassandraDB.DeviceAlarms.store(alarm)).recover {
      case ex: Exception =>
        throw new CassandraDBException("Failed to persist the alarm " + alarm.toJSON + ", with error " +
                                       ex.getMessage,
                                       ex.getCause)
    }

  def getOrgHierarchy(nodeId: String)(implicit ec: ExecutionContext): Future[Option[OrgHierarchy]] =
    lookupOrgTimer.time(database.OrgHierarchyByNode.getById(nodeId)).recover {
      case ex: Exception =>
        log.error("Unable to get Org details from OrgHierarchy_by_nodeid table for node: "
                  + nodeId + " with error: " + ex.getMessage,
                  ex)
        throw throw new CassandraDBException(ex.getMessage)
    }

  def persistAlertById(alert: Alert)(implicit ec: ExecutionContext): Future[Boolean] =
    persistAlertTimer
      .time(database.DeviceAlerts.store(alert))
      .recover {
        case ex: Exception =>
          log.error("Unable to persist alert: " + alert.toJSON + " with error: " + ex.getMessage, ex)
          throw new CassandraDBException(ex.getMessage)
      }
      .map(_.wasApplied())

  def persistAlertByNodeId(alert: Alert)(implicit ec: ExecutionContext): Future[Boolean] =
    persistAlertByNodeTimer
      .time(database.AlertsByNode.storeByNodeId(alert))
      .recover {
        case ex: Exception =>
          log.error("Unable to persist alert: " + alert.toJSON + " with error: " + ex.getMessage, ex)
          throw new CassandraDBException(ex.getMessage)
      }
      .map(_.wasApplied())

  def persistAlert(alert: Alert)(implicit ec: ExecutionContext): Future[Option[Alert]] =
    persistAlertByNodeId(alert).flatMap(_ => persistAlertById(alert)).map { result =>
      if (result)
        Some(alert)
      else
        None
    }

  def updateAlertByNodeId(alert: Alert)(implicit ec: ExecutionContext): Future[Boolean] =
    alertUpdateByNodeIdTimer
      .time(database.AlertsByNode.updateAlertByNodeId(alert))
      .recover {
        case ex: Exception =>
          log.error("Unable to update the alert: " + alert.toJSON + " with error: " + ex.getMessage, ex)
          throw new CassandraDBException(ex.getMessage)
      }
      .map(_.wasApplied())

  def updateAlertById(alert: Alert)(implicit ec: ExecutionContext): Future[Boolean] =
    alertUpdateTimer
      .time(database.DeviceAlerts.updateAlert(alert))
      .recover {
        case ex: Exception =>
          log.error("Unable to update the alert: " + alert.toJSON + " with error: " + ex.getMessage, ex)
          throw new CassandraDBException(ex.getMessage)
      }
      .map(_.wasApplied())

  def update_Alert(alert: Alert)(implicit ec: ExecutionContext): Future[Option[Alert]] =
    getAlert(alert.alertId, alert.orgId, alert.siteId).flatMap {
      case Some(result: Alert) =>
        val updatedAlert = alert.copy(nodeId = result.nodeId, `type` = result.`type`, created = result.created)
        updateAlertByNodeId(updatedAlert).flatMap(_ => updateAlertById(updatedAlert)).map { result =>
          if (result)
            Some(updatedAlert)
          else None
        }
      case None => Future(None)
    }

  def dismissAlertByNodeId(nodeId: String, alarmType: String, orgId: String, siteId: String)(
      implicit ec: ExecutionContext
  ): Future[Boolean] =
    alertDismissByNodeTimer
      .time(database.AlertsByNode.dismissAlertByNodeId(nodeId, alarmType, orgId, siteId))
      .recover {
        case ex: Exception =>
          log.error("Unable to dismiss the alert : " + ex.getMessage, ex)
          throw new CassandraDBException(ex.getMessage)
      }
      .map(_.wasApplied())

  def dismissAlertById(alertId: String, orgId: String, siteId: String)(implicit ec: ExecutionContext): Future[Boolean] =
    alertDismissTimer
      .time(database.DeviceAlerts.dismissAlert(alertId, orgId, siteId))
      .recover {
        case ex: Exception =>
          log.error("Unable to dismiss the alert: " + alertId + " with error: " + ex.getMessage, ex)
          throw new CassandraDBException(ex.getMessage)
      }
      .map(_.wasApplied())

  def dismiss_Alert(alertId: String, orgId: String, siteId: String)(implicit ec: ExecutionContext): Future[Boolean] =
    getAlert(alertId, orgId, siteId).flatMap {
      case Some(result: Alert) =>
        dismissAlertByNodeId(result.nodeId, result.`type`, orgId, siteId)
          .flatMap(_ => dismissAlertById(alertId, orgId, siteId))
      case None => Future(false)
    }

  def deleteAlertByNodeId(nodeId: String, alarmType: String, orgId: String, siteId: String)(
      implicit ec: ExecutionContext
  ): Future[Boolean] =
    alertDeleteByNodeTimer
      .time(database.AlertsByNode.deleteAlertByNodeId(nodeId, alarmType, orgId, siteId))
      .recover {
        case ex: Exception =>
          log.error("Unable to delete the alert : " + ex.getMessage, ex)
          throw new CassandraDBException(ex.getMessage)
      }
      .map(_.wasApplied())

  def deleteAlertById(alertId: String, orgId: String, siteId: String)(implicit ec: ExecutionContext): Future[Boolean] =
    alertDeleteTimer
      .time(database.DeviceAlerts.deleteAlert(alertId, orgId, siteId))
      .recover {
        case ex: Exception =>
          log.error("Unable to delete the alert: " + alertId + " with error: " + ex.getMessage, ex)
          throw new CassandraDBException(ex.getMessage)
      }
      .map(_.wasApplied())

  def delete_Alert(alertId: String, orgId: String, siteId: String)(implicit ec: ExecutionContext): Future[Boolean] =
    getAlert(alertId, orgId, siteId).flatMap {
      case Some(result: Alert) =>
        deleteAlertByNodeId(result.nodeId, result.`type`, orgId, siteId)
          .flatMap(_ => deleteAlertById(alertId, orgId, siteId))
      case None => Future(false)
    }

  def getAlert(alertId: String, orgId: String, siteId: String)(implicit ec: ExecutionContext): Future[Option[Alert]] =
    alertLookupTimer.time(database.DeviceAlerts.getAlertForAlertId(alertId, orgId, siteId)).recover {
      case ex: Exception =>
        log.error("Unable to get the alert: " + alertId + " with error: " + ex.getMessage, ex)
        throw new CassandraDBException(ex.getMessage);
    }

  def getAlertSys(alertId: String, orgId: String, siteId: String)(
      implicit ec: ExecutionContext
  ): Future[Option[Alert]] =
    alertLookupSysTimer
      .time {
        getAlert(alertId, orgId, siteId)
          .map {
            case None        => None
            case Some(alert) => if (alert.active) Some(alert) else None
          }
      }

  def getAlertsForSite(orgId: String, siteId: String)(implicit ec: ExecutionContext): Future[List[Alert]] =
    alertLookupBySiteTimer
      .time(database.DeviceAlerts.getAlertForOrgAndSite(orgId, siteId))
      .recover {
        case ex: Exception =>
          log.error("Unable to get alerts for org: " + orgId + " and site: " + siteId + " with error: " + ex.getMessage,
                    ex)
          throw new CassandraDBException(ex.getMessage)
      }
      .map {
        _.filter(_.active)
      }

  def getAlertsForNode(orgId: String, siteId: String, nodeId: String)(
      implicit ec: ExecutionContext
  ): Future[List[Alert]] =
    alertLookupByNodeTimer.time {
      database.DeviceAlerts
        .getAlertForOrgAndSite(orgId, siteId)
        .recover {
          case ex: Exception =>
            log.error("Unable to get alerts for node: " + nodeId + " with error: " + ex.getMessage, ex)
            throw new CassandraDBException(ex.getMessage)
        }
        .map {
          _.filter(alert => alert.nodeId.equalsIgnoreCase(nodeId) && alert.active)
        }
    }

  def persistNotification(notification: Notification)(implicit ec: ExecutionContext): Future[Option[Notification]] =
    notificationPersistTimer
      .time(database.Notifications.store(notification))
      .recover {
        case ex: Exception =>
          log.error("Unable to create notification: " + notification.toJSON + " with error: " + ex.getMessage, ex)
          throw new CassandraDBException(ex.getMessage)
      }
      .map { result =>
        if (result.wasApplied())
          Some(notification)
        else None
      }

  def update_Notification(notification: Notification)(implicit ec: ExecutionContext): Future[Option[Notification]] =
    notificationUpdateTimer.time {
      getNotification(notification.notificationId, notification.orgId, notification.siteId).flatMap {
        case Some(record) =>
          val updatedNotification = notification.copy(created = record.created)
          database.Notifications
            .modify(updatedNotification)
            .recover {
              case ex: Exception =>
                log.error(s"Unable to update notification: ${updatedNotification.toJSON} : ${ex.getMessage}", ex)
                throw new CassandraDBException(ex.getMessage)
            }
            .map { result =>
              if (result.wasApplied())
                Some(updatedNotification)
              else None
            }
        case None => Future(None)
      }
    }

  def activate_Notification(notificationId: String, orgId: String, siteId: String)(
      implicit ec: ExecutionContext
  ): Future[Boolean] =
    notificationActivateTimer.time {
      getNotification(notificationId, orgId, siteId).flatMap {
        case Some(_) =>
          database.Notifications
            .activate(notificationId, orgId, siteId)
            .recover {
              case ex: Exception =>
                log.error("Unable to activate notification: " + notificationId + " with error: " + ex.getMessage, ex)
                throw new CassandraDBException(ex.getMessage)
            }
            .map(_.wasApplied())
        case None => Future(false)
      }
    }

  def deactivate_Notification(notificationId: String, orgId: String, siteId: String)(
      implicit ec: ExecutionContext
  ): Future[Boolean] =
    notificationDeActivateTimer.time {
      getNotification(notificationId, orgId, siteId).flatMap {
        case Some(_) =>
          database.Notifications
            .deActivate(notificationId, orgId, siteId)
            .recover {
              case ex: Exception =>
                log.error("Unable to de-activate notification: " + notificationId + " with error: " + ex.getMessage, ex)
                throw new CassandraDBException(ex.getMessage)
            }
            .map(_.wasApplied())
        case None => Future(false)
      }
    }

  def delete_Notification(notificationId: String, orgId: String, siteId: String)(
      implicit ec: ExecutionContext
  ): Future[Boolean] =
    notificationDeleteTimer.time {
      getNotification(notificationId, orgId, siteId).flatMap {
        case Some(_) =>
          database.Notifications
            .deleteNotification(notificationId, orgId, siteId)
            .recover {
              case ex: Exception =>
                log.error("Unable to delete notification: " + notificationId + " with error: " + ex.getMessage, ex)
                throw new CassandraDBException(ex.getMessage)
            }
            .map(_.wasApplied())
        case None => Future(false)
      }
    }

  def getNotification(notificationId: String, orgId: String, siteId: String)(
      implicit ec: ExecutionContext
  ): Future[Option[Notification]] =
    notificationLookupTimer.time(database.Notifications.getById(notificationId, orgId, siteId)).recover {
      case ex: Exception =>
        log.error("Unable to get the notification: " + notificationId + " with error: " + ex.getMessage, ex)
        throw new CassandraDBException(ex.getMessage)
    }

  def getNotificationsForSite(orgId: String,
                              siteId: String)(implicit ec: ExecutionContext): Future[List[Notification]] =
    notificationLookupBySiteTimer.time(database.Notifications.getBySiteId(orgId, siteId)).recover {
      case ex: Exception =>
        log.error(
          "Unable to get notifications for org: " + orgId + " and site: " + siteId + " with error: " + ex.getMessage,
          ex
        )
        throw new CassandraDBException(ex.getMessage)
    }

  def getNotificationsForName(name: Option[String], orgId: String, siteId: String)(
      implicit ec: ExecutionContext
  ): Future[List[Notification]] =
    notificationLookupByNameTimer.time {
      database.Notifications
        .getBySiteId(orgId, siteId)
        .recover {
          case ex: Exception =>
            log.error("Unable to get notifications for name: " + name + " with error: " + ex.getMessage, ex)
            throw new CassandraDBException(ex.getMessage)
        }
        .map {
          _.filter(_.name.getOrElse("").equalsIgnoreCase(name.getOrElse("")))
        }
    }

  def get_AllUfAlarms()(implicit ec: ExecutionContext): Future[List[UfAlarm]] =
    ufAlarmLookupTimer
      .time(database.UFAlarmsById.getAllUfAlarms).map(_.sortBy(_.ufName))
      .recover {
        case ex: Exception =>
          log.error("Unable to get UF Alarms with error: " + ex.getMessage, ex)
          throw new CassandraDBException(ex.getMessage)
      }

  def get_UfAlarmByType(alarmType: String)(implicit ec: ExecutionContext): Future[Option[UfAlarm]] =
    ufAlarmLookupByTypeTimer.time(database.UFAlarms.getUfAlarmByType(alarmType)).recover {
      case ex: Exception =>
        log.error(s"Unable to get UF Alarms for type $alarmType : " + ex.getMessage, ex)
        throw new CassandraDBException(ex.getMessage)
    }

  def get_UfAlarmsForCustomer()(implicit ec: ExecutionContext): Future[List[UfAlarm]] =
    ufAlarmForCustomerTimer.time {
      get_AllUfAlarms.map(_.filter(_.displayToCustomer))
    }

  def get_UfAlarmsForPartner()(implicit ec: ExecutionContext): Future[List[UfAlarm]] =
    ufAlarmForPartnerTimer.time {
      get_AllUfAlarms.map(_.filter(_.displayToPartner))
    }

  def get_UfAlarmById(mappingId: String)(implicit ec: ExecutionContext): Future[Option[UfAlarm]] =
    ufAlarmLookupByIdTimer.time(database.UFAlarmsById.getUfAlarmById(mappingId)).recover {
      case ex: Exception =>
        log.error("Unable to get the UF Alarm: " + mappingId + " with error: " + ex.getMessage, ex)
        throw new CassandraDBException(ex.getMessage)
    }

  def createUfAlarmByType(ufAlarm: UfAlarm)(implicit ec: ExecutionContext): Future[Boolean] =
    ufAlarmPersistTimer
      .time(database.UFAlarms.createUfAlarmByType(ufAlarm))
      .recover {
        case ex: Exception =>
          log.error("Unable to create UF Alarm: " + ufAlarm.toJSON + " with error: " + ex.getMessage, ex)
          throw new CassandraDBException(ex.getMessage)
      }
      .map(_.wasApplied())

  def createUfAlarmById(ufAlarm: UfAlarm)(implicit ec: ExecutionContext): Future[Boolean] =
    ufAlarmPersistByIdTimer
      .time(database.UFAlarmsById.createUfAlarmById(ufAlarm))
      .recover {
        case ex: Exception =>
          log.error("Unable to create UF Alarm: " + ufAlarm.toJSON + " with error: " + ex.getMessage, ex)
          throw new CassandraDBException(ex.getMessage)
      }
      .map(_.wasApplied())

  def checkIfUfNameExists(ufAlarm: UfAlarm)(implicit ec: ExecutionContext): Future[Boolean] = {
    val ufNameList: Future[List[UfAlarm]] = get_AllUfAlarms().map {
      _.filter(_.ufName.getOrElse("").equalsIgnoreCase(ufAlarm.ufName.getOrElse("")))
    }
    ufNameList.map { ufAlarmList =>
      if (ufAlarmList.isEmpty)
        true
      else {
        val nodeModelList: Set[String] = ufAlarm.nodeModels
        val modelList: Set[String]     = ufAlarmList.flatMap(_.nodeModels).toSet
        val existingModels             = modelList.intersect(nodeModelList)
        if (existingModels.isEmpty)
          true
        else
          throw new AlarmTypeExistsException(
            s"UFAlarm exists with Model: ${existingModels.mkString(",")} and name: ${ufAlarm.ufName.mkString(",")}"
          )
      }
    }
  }

  def existingModels(newUfAlarmList: List[UfAlarm], existingUfAlarmList: List[UfAlarm]): List[(String, Set[String])] = {
    //Map of UfName and applicable node models grouped by UfName
    val existingNodeModels: Map[String, Set[String]] =
      existingUfAlarmList.groupBy(_.ufName.getOrElse("")).mapValues(x => x.flatMap(_.nodeModels).toSet)

    newUfAlarmList
      .map { item =>
        val ufName = item.ufName.getOrElse("")
        //For each entry in Map, check if Node models exist in the database for same UfName
        val models = item.nodeModels.intersect(existingNodeModels.getOrElse(item.ufName.getOrElse(""), Set()))
        (ufName, models)
      }
      .filter(_._2.nonEmpty) //Return List of NodeModels and UfNames which already exist in the Database
  }

  def checkIfUfNameExists(ufAlarmList: List[UfAlarm])(implicit ec: ExecutionContext): Future[Boolean] = {
    val typeList: List[String] = ufAlarmList.map(_.alarmType)
    val nameList: List[String] = ufAlarmList.map(_.ufName.getOrElse(""))

    get_AllUfAlarms().map { items =>
      val existingTypeList = items.map(_.alarmType)
      val existingNameList = items.map(_.ufName.getOrElse(""))
      val TypeDupsList     = existingTypeList.intersect(typeList) //Check if Type Exists
      val NameDupsList     = existingNameList.intersect(nameList) //Check if Name Exists
      if (TypeDupsList.isEmpty && NameDupsList.isEmpty) true
      else {
        val duplicateUfNames = existingModels(ufAlarmList, items) //check if Node Models are present in existing List
        val duplicateModels  = duplicateUfNames.map(_._2)
        val duplicateNames   = duplicateUfNames.map(_._1)

        (TypeDupsList.isEmpty, duplicateModels.isEmpty) match {
          case (true, true) => true
          case (false, _) =>
            throw new AlarmTypeExistsException(s"Alarm Type ${TypeDupsList.mkString(",")} already exists")
          case (_, false) =>
            throw new AlarmTypeExistsException(
              s"UFAlarm exists with name: ${duplicateNames.mkString(",")} for Models: ${duplicateModels.mkString(",")}"
            )
        }
      }
    }
  }

  def create_UfAlarm(ufAlarm: UfAlarm)(implicit ec: ExecutionContext): Future[Option[UfAlarm]] =
    createUfAlarmByType(ufAlarm)
      .flatMap(_ => createUfAlarmById(ufAlarm))
      .map { result =>
        if (result)
          Some(ufAlarm)
        else
          None
      }

  def getOrElseCreateUfAlarm(ufAlarm: UfAlarm)(implicit ec: ExecutionContext): Future[UfAlarm] =
    get_UfAlarmByType(ufAlarm.alarmType).flatMap {
      case Some(result) =>
        if (result.nodeModels.intersect(ufAlarm.nodeModels).nonEmpty)
          Future(result)
        else {
          log.error("Node Model not in Scope: " + ufAlarm.nodeModels.mkString(","))
          update_UfAlarm(result.copy(nodeModels = result.nodeModels.union(ufAlarm.nodeModels))).map(_ => ufAlarm)
        }
      case None =>
        log.error(s"No Uf Name found for alarmType: ${ufAlarm.alarmType} and Model: ${ufAlarm.nodeModels}")
        createUfAlarmByType(ufAlarm)
          .flatMap(_ => createUfAlarmById(ufAlarm))
          .map(_ => ufAlarm)
    }

  def updateUfAlarmById(ufAlarm: UfAlarm)(implicit ec: ExecutionContext): Future[Boolean] =
    ufAlarmUpdateByIdTimer
      .time(database.UFAlarmsById.updateUfAlarmById(ufAlarm))
      .recover {
        case ex: Exception =>
          log.error(s"Unable to update UF Alarm: ${ufAlarm.toJSON} : ${ex.getMessage}", ex)
          throw new CassandraDBException(ex.getMessage)
      }
      .map(_.wasApplied())

  def updateUfAlarmByType(ufAlarm: UfAlarm)(implicit ec: ExecutionContext): Future[Boolean] =
    ufAlarmUpdateTimer
      .time(database.UFAlarms.updateUfAlarmByType(ufAlarm))
      .recover {
        case ex: Exception =>
          log.error(s"Unable to update UF Alarm: ${ufAlarm.toJSON} : ${ex.getMessage}", ex)
          throw new CassandraDBException(ex.getMessage)
      }
      .map(_.wasApplied())

  def update_UfAlarm(ufAlarm: UfAlarm)(implicit ec: ExecutionContext): Future[Option[UfAlarm]] =
    updateUfAlarmByType(ufAlarm).flatMap(_ => updateUfAlarmById(ufAlarm)).map { result =>
      if (result)
        Some(ufAlarm)
      else None
    }

  def deleteUfAlarmByType(alarmType: String)(implicit ec: ExecutionContext): Future[Boolean] =
    ufAlarmDeleteTimer
      .time(database.UFAlarms.deleteUfAlarmByType(alarmType))
      .recover {
        case ex: Exception =>
          log.error(s"Unable to delete UF Alarm: ${ex.getMessage}", ex)
          throw new CassandraDBException(ex.getMessage)
      }
      .map(_.wasApplied())

  def deleteUfAlarmById(mappingId: String)(implicit ec: ExecutionContext): Future[Boolean] =
    ufAlarmDeleteByIdTimer
      .time(database.UFAlarmsById.deleteUfAlarmById(mappingId))
      .recover {
        case ex: Exception =>
          log.error("Unable to delete UF Alarm: " + mappingId + " with error: " + ex.getMessage, ex)
          throw new CassandraDBException(ex.getMessage)
      }
      .map(_.wasApplied())

  def delete_UfAlarm(mappingId: String)(implicit ec: ExecutionContext): Future[Boolean] =
    get_UfAlarmById(mappingId).flatMap {
      case Some(record) =>
        deleteUfAlarmByType(record.alarmType).flatMap(_ => deleteUfAlarmById(mappingId))
      case None => Future(false)
    }

  def get_NotificationTypes(userType: String)(implicit ec: ExecutionContext): Future[List[NotificationType]] =
    getNotificationTypesTimer.time {
      val ufAlarmList = userType.toLowerCase match {
        case "partner"  => get_UfAlarmsForPartner
        case "customer" => get_UfAlarmsForCustomer
        case "sensity"  => get_AllUfAlarms
        case _          => throw new UserTypeMissingException(s"Invalid user type: $userType")
      }
      ufAlarmList.map { ufAlarm =>
        ufAlarm.map(NotificationType(_))
      }
    }
}
