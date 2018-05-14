package com.verizon.netsense.services.alert.services

import com.verizon.netsense.services.alert.customexceptions._
import com.verizon.netsense.services.alert.database.PhantomConnector
import com.verizon.netsense.services.alert.model.CaselType._
import com.verizon.netsense.services.alert.model._
import com.verizon.netsense.utils.Logging

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class NotificationRequestHandler(implicit ec: ExecutionContext) extends PhantomConnector with JsonUtils with Logging {

  val notificationsSuccessResponse = successResponse[List[Notification]]
  val notificationSuccessResponse  = successResponse[Notification]
  val NotificationTypeResponse     = successResponse[List[NotificationType]]
  val statusResponse               = successResponse[Status]

  def getAppResponse(appRequest: AppRequest): Future[AppResponse] = {

    import appRequest.request._
    val orgId: String = orgprops match {
      case Some(x) => x.orgId
      case None =>
        if (`type`.equalsIgnoreCase(GET_NOTIFICATION_TYPES.value)) "None"
        else throw new NotificationIdMissingException("OrgId is missing in the Payload")
    }
    val siteId: String = siteprops match {
      case Some(x) => x.siteId
      case None =>
        if (`type`.equalsIgnoreCase(GET_NOTIFICATION_TYPES.value)) "None"
        else throw new NotificationIdMissingException("OrgId is missing in the Payload")
    }
    val name: Option[String] = notificationprops match {
      case Some(x) => x.name
      case None    => None
    }
    val notificationId: String = notificationprops match {
      case Some(x) => x.notificationId.getOrElse("")
      case None    => ""
    }
    appRequest.request.`type` match {
      case NOTIFICATION_FOR_NAME.value =>
        getNotificationsByName(name, orgId, siteId).map {
          notificationsSuccessResponse(_, appRequest)
        }
      case NOTIFICATION_FOR_ID.value =>
        getNotificationById(notificationId, orgId, siteId).map {
          case Some(notification) => notificationSuccessResponse(notification, appRequest)
          case None               => failureResponse(s"No Notification found for $notificationId", appRequest)
        }
      case NOTIFICATION_SYS.value =>
        getNotificationById(notificationId, orgId, siteId).map {
          case Some(notification) => notificationSuccessResponse(notification, appRequest)
          case None               => failureResponse(s"No Notifications found for $notificationId", appRequest)
        }
      case DELETE_NOTIFICATION.value =>
        deleteNotification(notificationId, orgId, siteId).map {
          case true  => statusResponse(Status(true), appRequest)
          case false => failureResponse(s"Notification with $notificationId not found", appRequest)
        }
      case ACTIVATE_NOTIFICATION.value =>
        activateNotification(notificationId, orgId, siteId).map {
          case true  => statusResponse(Status(true), appRequest)
          case false => failureResponse(s"Notification with $notificationId not found", appRequest)
        }
      case DEACTIVATE_NOTIFICATION.value =>
        deActivateNotification(notificationId, orgId, siteId).map {
          case true  => statusResponse(Status(true), appRequest)
          case false => failureResponse(s"Notification with $notificationId not found", appRequest)
        }
      case UPDATE_NOTIFICATION.value =>
        val userType: String = userprops match {
          case Some(x) => x.userType.getOrElse(throw new UserTypeMissingException)
          case None    => throw new UserTypeMissingException
        }
        val notificationPayload = Try { notificationprops.get.toNotification(orgId, siteId) }.toEither match {
          case Left(ex) => throw ex
          case Right(n) => n
        }
        NotificationUpdater(notificationPayload, userType).map {
          case Some(notification) => notificationSuccessResponse(notification, appRequest)
          case None               => failureResponse(s"Unable to update Notification: $notificationprops", appRequest)
        }
      case NOTIFICATIONS_FOR_SITE_ID.value =>
        getNotificationBySiteId(orgId, siteId).map {
          notificationsSuccessResponse(_, appRequest)
        }
      case ADD_NOTIFICATION.value =>
        val userType: String = userprops match {
          case Some(x) => x.userType.getOrElse(throw new UserTypeMissingException)
          case None    => throw new UserTypeMissingException
        }
        val notificationPayload = Try { notificationprops.get.toNotification(orgId, siteId) }.toEither match {
          case Left(ex) => throw ex
          case Right(n) => n
        }
        createNotification(notificationPayload, userType).map {
          case Some(notification) => notificationSuccessResponse(notification, appRequest)
          case None               => failureResponse(s"Unable to create Notification: $notificationprops", appRequest)
        }
      case GET_NOTIFICATION_TYPES.value =>
        val userType: String = userprops match {
          case Some(x) => x.userType.getOrElse(throw new UserTypeMissingException)
          case None    => throw new UserTypeMissingException
        }
        getNotificationTypes(userType).map {
          NotificationTypeResponse(_, appRequest)
        }
      case _ => throw new InvalidRequestPayloadException("Invalid AppRequest type" + appRequest.request.`type`)
    }
  }

  def checkTypeExists(notificationType: Set[String], userType: String): Future[Boolean] =
    get_NotificationTypes(userType)
      .recover { case ex: Exception => throw ex }
      .map { ufAlarmList =>
        val typeList      = ufAlarmList.map(_.alarmType)
        val existingTypes = notificationType.partition(!typeList.contains(_))._1
        if (existingTypes.isEmpty)
          true
        else
          throw new NotificationIdMissingException(
            s"Invalid Notification Types: ${existingTypes.mkString(",")} for User: $userType."
          )
      }

  def createNotification(notification: Notification, userType: String): Future[Option[Notification]] =
    if (notification.name.getOrElse("").contains("DeviceAlarm"))
      addNotification(notification, userType) //For Device Alarm
    else
      addParkingNotification(notification) // For Business Alert - Parking

  def NotificationUpdater(notification: Notification, userType: String): Future[Option[Notification]] =
    if (notification.name.getOrElse("").contains("DeviceAlarm"))
      updateNotification(notification, userType) //For Device Alarm
    else
      updateParkingNotification(notification) // For Business Alert - Parking

  def getNotificationsByName(name: Option[String], orgId: String, siteId: String): Future[List[Notification]] = {
    log.debug("Get Notifications for name " + name)
    getNotificationsForName(name, orgId, siteId)
  }

  def getNotificationById(notificationId: String, orgId: String, siteId: String): Future[Option[Notification]] = {
    log.debug("Get Notification: " + notificationId)
    getNotification(notificationId, orgId, siteId)
  }

  def deleteNotification(notificationId: String, orgId: String, siteId: String): Future[Boolean] = {
    log.debug("Delete Notification: " + notificationId)
    delete_Notification(notificationId, orgId, siteId)
  }

  def activateNotification(notificationId: String, orgId: String, siteId: String): Future[Boolean] = {
    log.debug("Activate Notification: " + notificationId)
    activate_Notification(notificationId, orgId, siteId)
  }

  def deActivateNotification(notificationId: String, orgId: String, siteId: String): Future[Boolean] = {
    log.debug("Deactivate Notification: " + notificationId)
    deactivate_Notification(notificationId, orgId, siteId)
  }

  def getNotificationBySiteId(orgId: String, siteId: String): Future[List[Notification]] = {
    log.debug("Get Notifications for Site " + siteId + " and Org: " + orgId)
    getNotificationsForSite(orgId, siteId)
  }

  def addNotification(notification: Notification, userType: String): Future[Option[Notification]] = {
    log.debug("Create Notification: " + notification.toJSON)
    checkTypeExists(notification.notificationType, userType).recover { case ex: Exception => throw ex }.flatMap {
      result =>
        if (result) {
          persistNotification(notification)
        } else Future(None)
    }
  }

  def addParkingNotification(notification: Notification): Future[Option[Notification]] = {
    log.debug("Create Parking Notification: " + notification.toJSON)
    persistNotification(notification)
  }

  def updateNotification(notification: Notification, userType: String): Future[Option[Notification]] = {
    log.debug("Update Notification: " + notification.toJSON)
    checkTypeExists(notification.notificationType, userType).recover { case ex: Exception => throw ex }.flatMap {
      result =>
        if (result)
          update_Notification(notification)
        else Future(None)
    }
  }

  def updateParkingNotification(notification: Notification): Future[Option[Notification]] = {
    log.debug("Update Parking Notification: " + notification.toJSON)
    update_Notification(notification)
  }

  def getNotificationTypes(userType: String): Future[List[NotificationType]] = {
    log.debug("Get Notification types for " + userType)
    get_NotificationTypes(userType)
  }

}
object NotificationRequestHandler {
  def apply(implicit ec: ExecutionContext): NotificationRequestHandler =
    new NotificationRequestHandler()(ec)
}
