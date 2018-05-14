package com.verizon.netsense.services.alert.services.database

import java.util.UUID

import com.verizon.netsense.services.alert.customexceptions.UserTypeMissingException
import com.verizon.netsense.services.alert.model.Notification
import com.verizon.netsense.services.alert.services.NotificationRequestHandler
import com.verizon.netsense.services.alert.services.data.TestData
import org.scalatest.MustMatchers._
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FunSuite}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor}

class NotificationsDBSpec extends FunSuite with TestData with BeforeAndAfterAll with BeforeAndAfterEach {

  implicit val ex: ExecutionContextExecutor = ExecutionContext.global

  val notificationAPI = new NotificationRequestHandler()

  override def beforeAll(): Unit = {
    super.beforeAll()
    Await.ready(CassandraDBTest.Notifications.createTable(), 1.seconds)
    Await.ready(CassandraDBTest.UFAlarms.createTable(), 1.seconds)
    Await.ready(CassandraDBTest.UFAlarmsById.createTable(), 1.seconds)
    loadUFAlarms()
  }

  override def afterAll(): Unit = {
    super.afterAll()
    Await.ready(CassandraDBTest.Notifications.truncateTable(), 1.seconds)
    Await.ready(CassandraDBTest.UFAlarms.truncateTable(), 1.seconds)
    Await.ready(CassandraDBTest.UFAlarmsById.truncateTable(), 1.seconds)
  }

  override def beforeEach(): Unit = {
    super.afterAll()
    Await.result(notificationAPI.addNotification(getNotification(), "sensity"), 1.seconds)
  }

  override def afterEach(): Unit = {
    super.afterAll()
    Await.ready(CassandraDBTest.Notifications.truncateTable(), 1.seconds)
  }

  def getNotification(): Notification = {
    val notificationId = "fe6f26b9-b9d0-4867-b33b-0e2b2e4c3555"
    Notification(
      notificationId,
      "orgId1",
      true,
      "siteid1",
      name = Some("DeviceAlarm"),
      notificationType = Set("Disconnect", "CommFail"),
      severity = Set("Critical", "Major"),
      description = Some("NewDescription"),
      smsUsersList = Set("abc", "def", "abc")
    )
  }

  test("should fetch notification for the given notification id") {

    val notification = getNotification()

    val notificationResult =
      notificationAPI.getNotificationById(notification.notificationId, notification.orgId, notification.siteId)
    val result = Await.result(notificationResult, 1.seconds)
    result match {
      case Some(x) =>
        x.notificationId must be(notification.notificationId)
        x.orgId must be(notification.orgId)
      case None => fail("Record not found")
    }
  }

  test("should not return any results when fetching notification id that does not exist") {

    val notification = getNotification()

    val notificationResult =
      notificationAPI.getNotificationById(UUID.randomUUID().toString, notification.orgId, notification.siteId)
    val result = Await.result(notificationResult, 1.seconds)
    result match {
      case Some(x) => fail("Fetched Notification that did not exist")
      case None    => succeed
    }
  }

  test("should fetch notifications for the given Name and Site") {

    val notification = getNotification()

    val notificationResult =
      notificationAPI.getNotificationsByName(notification.name, notification.orgId, notification.siteId)
    val result = Await.result(notificationResult, 1.seconds)
    if (result.nonEmpty) result.foreach(x => {
      x.siteId must be(notification.siteId)
      x.name must be(notification.name)
    })
    else {
      fail("Record with name and siteid not found")
    }
  }

  test("should not return any results when fetching notification for the Name and Site that does not exist") {

    val notification = getNotification()

    val notificationResult =
      notificationAPI.getNotificationsByName(Some("SomeName"), "org1", UUID.randomUUID().toString)
    val result = Await.result(notificationResult, 1.seconds)
    if (result.isEmpty) {
      succeed
    } else {
      fail("Fetched Notifications for Name and Site that did not exist")
    }
  }

  test("should fetch notifications for the given siteId") {

    val notification = getNotification()

    val notificationResult = notificationAPI.getNotificationBySiteId(notification.orgId, notification.siteId)
    val result             = Await.result(notificationResult, 1.seconds)
    if (result.nonEmpty) result.foreach(x => {
      x.siteId must be(notification.siteId)
      x.orgId must be(notification.orgId)
    })
    else {
      fail("Record with siteid not found")
    }
  }

  test("should not return any results when fetching notification for the siteId that does not exist") {

    val notification = getNotification()

    val notificationResult = notificationAPI.getNotificationBySiteId(notification.orgId, UUID.randomUUID().toString)
    val result             = Await.result(notificationResult, 1.seconds)
    if (result.isEmpty) {
      succeed
    } else {
      fail("Fetched Notifications for SiteId that did not exist")
    }
  }

  test("should delete notification for the given notification id") {
    val notification = getNotification()

    val deleteResult =
      notificationAPI.deleteNotification(notification.notificationId, notification.orgId, notification.siteId)
    if (Await.result(deleteResult, 1.seconds)) {
      val notificationResult =
        notificationAPI.getNotificationById(notification.notificationId, notification.orgId, notification.siteId)
      Await.result(notificationResult, 1.seconds) match {
        case Some(x) => fail("Record not deleted")
        case _       => succeed
      }
    } else {
      fail("Delete Notification Failed")
    }
  }

  test("should give error for deleting the notification that does not exist") {

    val notification = getNotification()

    val invalidDeleteResult =
      notificationAPI.deleteNotification(UUID.randomUUID().toString, notification.orgId, notification.siteId)
    val result = Await.result(invalidDeleteResult, 1.seconds)
    if (result) {
      fail("Trying to delete non-existing notification")
    } else {
      succeed
    }
  }

  test("should create a new notification") {

    val notification = getNotification()

    val notificationResult =
      notificationAPI.getNotificationById(notification.notificationId, notification.orgId, notification.siteId)
    val result = Await.result(notificationResult, 1.seconds)
    result match {
      case Some(x) =>
        x.notificationId must be(notification.notificationId)
        x.orgId must be(notification.orgId)
        x.active must be(notification.active)
        x.siteId must be(notification.siteId)
      case None => fail("Record not found")
    }
  }

  test("should give error when creating a new notification with invalid user type") {

    val notification = getNotification()
    assertThrows[UserTypeMissingException](notificationAPI.addNotification(notification, "Unknown"))
  }

  test("should update an existing notification") {

    val notification = getNotification()

    val updatedNotification = notification.copy(
      name = Some("UpdatedName"),
      description = None,
      notificationType = Set("ConfigFail", "Disconnect"),
      severity = Set("Critical", "Major", "Minor"),
      smsUsersList = Set.empty[String]
    )
    val updateResult = notificationAPI.updateNotification(updatedNotification, "sensity")
    val result       = Await.result(updateResult, 1.seconds)
    result match {
      case Some(x) =>
        x.notificationId must be(updatedNotification.notificationId)
        x.orgId must be(updatedNotification.orgId)
        x.active must be(updatedNotification.active)
        x.siteId must be(updatedNotification.siteId)
        x.name must be(updatedNotification.name)
        x.notificationType must be(updatedNotification.notificationType)
      case None => fail("Record not found")
    }
  }

  test("should not return any results when updating notification that does not exist") {

    val notification = getNotification()

    val updatedNotification = notification.copy(notificationId = UUID.randomUUID().toString,
                                                orgId = UUID.randomUUID().toString,
                                                true,
                                                "siteid1",
                                                name = Some("Updated Name"))

    val updateResult = notificationAPI.updateNotification(updatedNotification, "sensity")
    val result       = Await.result(updateResult, 1.seconds)
    result match {
      case Some(_) => fail("Updated the notification that does not exist")
      case None               => succeed
    }
  }

  test("should change inactive notification to active") {

    val notification = getNotification()
    val inactiveNotification =
      notification.copy(notificationId = notification.notificationId,
                        orgId = notification.orgId,
                        active = false,
                        description = None,
                        smsUsersList = Set.empty[String])

    val activeResult =
      notificationAPI.activateNotification(notification.notificationId, notification.orgId, notification.siteId)
    if (Await.result(activeResult, 1.seconds)) {
      val notificationResult =
        notificationAPI.getNotificationById(notification.notificationId, notification.orgId, notification.siteId)
      val result = Await.result(notificationResult, 1.seconds)
      result match {
        case Some(x) =>
          x.notificationId must be(inactiveNotification.notificationId)
          x.orgId must be(inactiveNotification.orgId)
          x.siteId must be(inactiveNotification.siteId)
          x.active must be(!inactiveNotification.active)
        case _ => fail("Record not found")
      }
    } else {
      fail("Activate Notification failed")
    }
  }

  test("should not return any results when activating notification that does not exist") {

    val notification = getNotification()

    val inactiveResult =
      notificationAPI.activateNotification(UUID.randomUUID().toString, notification.orgId, notification.siteId)
    val result = Await.result(inactiveResult, 1.seconds)
    if (result) {
      fail("Activated the notification that does not exist")
    } else {
      succeed
    }
  }

  test("should change active notification to inactive") {

    val notification = getNotification()

    val inactiveResult =
      notificationAPI.deActivateNotification(notification.notificationId, notification.orgId, notification.siteId)
    if (Await.result(inactiveResult, 1.seconds)) {
      val notificationResult =
        notificationAPI.getNotificationById(notification.notificationId, notification.orgId, notification.siteId)
      val result = Await.result(notificationResult, 1.seconds)
      result match {
        case Some(x) =>
          x.notificationId must be(notification.notificationId)
          x.orgId must be(notification.orgId)
          x.siteId must be(notification.siteId)
          x.active must be(!notification.active)
        case _ => fail("Record not found")
      }
    } else {
      fail("Activate Notification failed")
    }

  }

  test("should not return any results when de-activating notification that does not exist") {

    val notification = getNotification()

    val activeResult =
      notificationAPI.deActivateNotification(UUID.randomUUID().toString, notification.orgId, notification.siteId)
    val result = Await.result(activeResult, 1.seconds)
    if (result) {
      fail("De-activated the notification that does not exist")
    } else {
      succeed
    }
  }

  test("should fetch NotificationTypes for partner") {
    val ufAlarmResult = notificationAPI.getNotificationTypes("partner")
    val result        = Await.result(ufAlarmResult, 1.seconds)
    if (result.nonEmpty) {
      assert(result.length == 2)
      result.foreach(x => {
        assert(x.ufName != null)
        assert(x.alarmType != null)
      })
    } else fail("No Records found")
  }

  test("should fetch NotificationTypes for customer") {
    val ufAlarmResult = notificationAPI.getNotificationTypes("customer")
    val result        = Await.result(ufAlarmResult, 1.seconds)
    if (result.nonEmpty) {
      assert(result.length == 3)
      result.foreach(x => {
        assert(x.ufName != null)
        assert(x.alarmType != null)
      })
    } else fail("No Records found")
  }

  test("should fetch NotificationTypes for sensity") {
    val ufAlarmResult = notificationAPI.getNotificationTypes("sensity")
    val result        = Await.result(ufAlarmResult, 1.seconds)
    if (result.nonEmpty) {
      assert(result.length == 3)
      result.foreach(x => {
        assert(x.ufName != null)
        assert(x.alarmType != null)
      })
    } else fail("No Records found")
  }

  test("should give error for fetching NotificationTypes for invalid user") {

    assertThrows[UserTypeMissingException](notificationAPI.getNotificationTypes("Unknown"))
  }

}
