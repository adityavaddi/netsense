package com.verizon.netsense.services.alert.services.data
import java.util.{Calendar, UUID}

import com.verizon.netsense.services.alert.database.CassandraDB
import com.verizon.netsense.services.alert.helper.KafkaConfig.kafkaConfig
import com.verizon.netsense.services.alert.model._
import com.verizon.netsense.services.alert.services.database.CassandraDBTest
import com.verizon.netsense.utils.Logging

/**
 * Created by subrsi9 on 6/29/17.
 */
trait TestData extends Logging {

  val notificationId1         = "dc0bd8e7-9fa1-4171-b309-b032aa65efb2"
  val notificationId2         = "6a2a4319-ebaf-42dd-a802-b69aef9c6df3"
  val notificationId3         = "bf662519-7e23-4a45-a7d7-965526222809"
  val notificationId_inactive = "6cb7bc02-2f1c-464a-9df6-ea38c87dab6d"
  val notificationId_random   = UUID.randomUUID().toString

  val notificationName               = "DeviceAlarm"
  val notificationType               = Set("Disconnect", "CommFail")
  val notificationTypeForPartner     = Set("CommFail")
  val notificationTypeInvalidPartner = Set("ConfigFail")
  val notificationSeverity           = Set("Major", "Minor")

  val (orgId1, orgId2, orgId_inactive) = ("org1", "org2", "org3")
  val orgId_random                     = UUID.randomUUID().toString

  val (siteId1, siteId2, siteId_inactive) = ("site1", "site2", "site3")
  val siteId_random                       = UUID.randomUUID().toString

  val notificationId_update = notificationId1
  val orgId_update          = orgId1
  val siteId_update         = siteId1

  def loadNotifications(): Unit = {
    CassandraDB.Notifications.store(
      Notification(notificationId1, orgId1, true, siteId1, Some(notificationName), notificationType = notificationType)
    )

    CassandraDB.Notifications.store(
      Notification(notificationId2, orgId1, true, siteId2, Some(notificationName), notificationType = notificationType)
    )

    CassandraDB.Notifications.store(
      Notification(notificationId3, orgId2, true, siteId1, Some(notificationName), notificationType = notificationType)
    )

    CassandraDB.Notifications.store(
      Notification(notificationId_inactive,
                   orgId_inactive,
                   false,
                   siteId_inactive,
                   Some("InactiveNotification"),
                   notificationType = notificationType)
    )
    log.info("4 Sample Records Loaded to Notifications Table")

  }

  def getNotificationProps: NotificationProps =
    NotificationProps(
      Some(UUID.randomUUID().toString),
      Some(notificationName),
      Some("NewDescription"),
      Some(true),
      None,
      Some(notificationType.toSeq),
      Some(notificationSeverity.toSeq)
    )

  def getNotificationPropsForPartner: NotificationProps =
    NotificationProps(
      Some(UUID.randomUUID().toString),
      Some(notificationName),
      Some("NewDescription"),
      Some(true),
      None,
      Some(notificationTypeForPartner.toSeq),
      Some(notificationSeverity.toSeq)
    )

  def getNotificationPropsForUpdate: NotificationProps =
    NotificationProps(
      Some(notificationId1),
      Some(notificationName),
      Some("UpdatedDescription"),
      Some(true),
      None,
      Some(notificationTypeInvalidPartner.toSeq),
      Some(notificationSeverity.toSeq)
    )

  def getNotificationPropsInvalidForSensity: NotificationProps =
    NotificationProps(None,
                      Some(notificationName),
                      Some("NewDescription"),
                      Some(true),
                      None,
                      Some(Set("Unknown").toSeq),
                      Some(notificationSeverity.toSeq))

  def getNotificationPropsInvalidForPartner: NotificationProps =
    NotificationProps(None,
                      Some(notificationName),
                      Some("NewDescription"),
                      Some(true),
                      None,
                      Some(notificationType.toSeq),
                      Some(notificationTypeInvalidPartner.toSeq))

  def getNotificationPropsInvalid: NotificationProps =
    NotificationProps(None,
                      Some(notificationName),
                      Some("NewDescription"),
                      Some(true),
                      None,
                      Some(notificationType.toSeq),
                      Some(notificationSeverity.toSeq))

  def getNotificationPropsInvalidSeverity: NotificationProps =
    NotificationProps(
      Some(UUID.randomUUID().toString),
      Some(notificationName),
      Some("NewDescription"),
      Some(true),
      None,
      Some(notificationType.toSeq),
      Some(Seq("Major", "Unknown"))
    )

  def getNotificationPropsInvalidHoldOff: NotificationProps =
    NotificationProps(
      Some(UUID.randomUUID().toString),
      Some(notificationName),
      Some("NewDescription"),
      Some(true),
      None,
      Some(notificationType.toSeq),
      Some(Seq("Major", "Minor")),
      hold_off = Some(-1),
      resend_interval = Some(-2)
    )

  val (nodeId1, nodeId2)     = ("node1", "core-node2")
  val (nodeName1, nodeName2) = ("Falcon", "unode-v4")
  val (orgName1, orgName2)   = ("Sensity Systems", "Verizon")
  val (siteName1, siteName2) = ("Lowell", "Sunnyvale")
  val orgData1 = OrgHierarchy(nodeId1,
                              Some(nodeName1),
                              Some(orgId1),
                              Some(orgName1),
                              Some(siteId1),
                              Some(siteName1),
                              None,
                              None,
                              Some("unode-v4"))
  val orgData2 = OrgHierarchy(nodeId2,
                              Some(nodeName2),
                              Some(orgId2),
                              Some(orgName2),
                              Some(siteId2),
                              Some(siteName2),
                              None,
                              None,
                              Some("falcon-q"))

  def loadOrgHierarchy(): Unit = {
    CassandraDBTest.OrgHierarchyByNode.store(orgData1)
    CassandraDBTest.OrgHierarchyByNode.store(orgData2)
    log.info("2 Sample Records Loaded to Org Hierarchy Table")

  }

  val alertId1         = "dc0bd8e7-9fa1-4171-b309-b032aa65efb2"
  val alertId2         = "6a2a4319-ebaf-42dd-a802-b69aef9c6df3"
  val alertId3         = "bf662519-7e23-4a45-a7d7-965526222809"
  val alertId_inactive = "6cb7bc02-2f1c-464a-9df6-ea38c87dab6d"
  val alertId_create   = "6ba9ee2e-5c79-4f5b-9576-c907d650ac4f"
  val alertId_random   = UUID.randomUUID().toString

  val alertName     = Some("DeviceAlarm")
  val alertType     = "Disconnect"
  val alertSeverity = Some("Critical")
  val alertCategory = Some("Network")
  val alertMsg      = Some("Node Disconnected")

  val alertId_update  = alertId1
  val updatedSeverity = Some("Minor")
  val updateType      = "ConfigFail"

  val nodeId_random = UUID.randomUUID().toString

  val alert1 = Alert(
    alertId1,
    alertName,
    alertMsg,
    nodeId1,
    orgId1,
    alertSeverity,
    alertType,
    alertCategory,
    Some(Calendar.getInstance().toInstant.toString),
    Some(Calendar.getInstance().toInstant.toString),
    None,
    None,
    None,
    None,
    siteId1,
    None,
    None,
    true
  )
  val alert2 = alert1.copy(alertId = alertId2, nodeId = nodeId2, orgId = orgId2, siteId = siteId2)
  val alert3 = alert1.copy(alertId = alertId3)

  def loadAlerts(): Unit = {
    CassandraDB.DeviceAlerts.store(alert1)
    CassandraDB.DeviceAlerts.store(alert2)
    CassandraDB.DeviceAlerts.store(alert3)
    CassandraDB.DeviceAlerts.store(alert3.copy(alertId = alertId_inactive, active = false))
    log.info("4 Sample Records Loaded to Alerts Table")

  }

  def getAlertProps: AlertProps =
    AlertProps(Some(alertId_create), alertName, Some(updateType), Some(nodeId1), alertMsg, alertSeverity, alertCategory)

  def getAlertPropsInvalid: AlertProps =
    AlertProps(Some(UUID.randomUUID().toString), alertName, alertMsg, alertSeverity, alertCategory)

  def getAlertPropsForUpdate: AlertProps =
    AlertProps(Some(alertId_create), alertName, None, None, alertMsg, updatedSeverity, alertCategory)

  val alarmId1 = UUID.randomUUID().toString
  val alarmId2 = UUID.randomUUID().toString
  val alarmId3 = UUID.randomUUID().toString

  val coreAlarmPayload1 = CoreAlarmEvent(
    "DeviceAlarm",
    nodeId2,
    "Disconnect",
    "Warning",
    "Node Disconnected"
  )
  val coreAlarmPayload2 = CoreAlarmEvent(
    "DeviceAlarm",
    nodeId2,
    "Disconnect",
    "Clear",
    "Node Disconnected"
  )
  val coreAlarmPayload_updated = CoreAlarmEvent(
    "DeviceAlarm",
    nodeId2,
    "Disconnect",
    "Minor",
    "Node Disconnected"
  )
  val coreAlarmPayload1_invalidNode = CoreAlarmEvent(
    "DeviceAlarm",
    nodeId_random,
    "Disconnect",
    "Warning",
    "Node Disconnected"
  )
  val coreAlarmPayload_Append = CoreAlarmEvent(
    "DeviceAlarm",
    nodeId2,
    "Disconnect",
    "Major",
    "Node Disconnected"
  )

  val alarmPayload1 = AlarmEvent(
    alarmId1,
    "UNSOL",
    "NW/status/disconnected1",
    "v1/falcon-q_1/out/UNSOL/alarm/{alarmid1}",
    DeviceAlarmPayload("lwt", 1, "Node Disconnected", 1),
    nodeId1,
    Calendar.getInstance().toInstant.toString
  )
  val alarmPayload2 = AlarmEvent(
    alarmId2,
    "UNSOL",
    "NW/status/disconnected2",
    "v1/falcon-q_1/out/UNSOL/alarm/{alarmid2}",
    DeviceAlarmPayload("Disconnect", 0, "Node Disconnected", 1),
    nodeId1,
    Calendar.getInstance().toInstant.toString
  )
  val alarmPayload3 = AlarmEvent(
    alarmId3,
    "UNSOL",
    "SW/diskfree/disk",
    "v1/falcon-q_1/out/UNSOL/alarm/{alarmid3}",
    DeviceAlarmPayload("SW/diskfree/disk", 2, "Disk volume more than 98% full", 1),
    nodeId1,
    Calendar.getInstance().toInstant.toString
  )
  val alarmPayload_updated = AlarmEvent(
    alarmId1,
    "UNSOL",
    "NW/status/disconnected1updated",
    "v1/falcon-q_1/out/UNSOL/alarm/{alarmid1updated}",
    DeviceAlarmPayload("lwt", 2, "Node Disconnected", 1),
    nodeId1,
    Calendar.getInstance().toInstant.toString
  )
  val alarmPayload_invalidNode = AlarmEvent(
    alarmId3,
    "UNSOL",
    "NW/status/disconnected3",
    "v1/falcon-q_1/out/UNSOL/alarm/{alarmid3}",
    DeviceAlarmPayload("Disconnect", 1, "Node Disconnected", 1),
    nodeId_random,
    Calendar.getInstance().toInstant.toString
  )
  val alarmPayload_Append = AlarmEvent(
    alarmId1,
    "UNSOL",
    "NW/status/disconnected99",
    "v1/falcon-q_1/out/UNSOL/alarm/{alarmid1}",
    DeviceAlarmPayload("Disconnect", 3, "Node Disconnected", 3),
    nodeId1,
    "2017-10-18T16:48:47Z"
  )

  val responseTopicTest = kafkaConfig.getString("interface-service-response-topic")

  val appReq_NoAlertProps = AppRequest(
    UUID.randomUUID.toString,
    responseTopicTest,
    RequestBody(
      UUID.randomUUID.toString,
      "createAlert",
      "AlertModel",
      "CAN_CREATE",
      None,
      None,
      Some(OrgProps(orgId1)),
      Some(SiteProps(siteId1)),
      None,
      None,
      None,
      Some(UUID.randomUUID.toString),
      UUID.randomUUID.toString,
      Calendar.getInstance.toString
    )
  )
  val appReq_NoNodeProps = AppRequest(
    UUID.randomUUID.toString,
    responseTopicTest,
    RequestBody(
      UUID.randomUUID.toString,
      "getAlertsForNode",
      "AlertModel",
      "CAN_CREATE",
      None,
      Some(getAlertProps),
      Some(OrgProps(orgId1)),
      Some(SiteProps(siteId1)),
      None,
      None,
      None,
      Some(UUID.randomUUID.toString),
      UUID.randomUUID.toString,
      Calendar.getInstance.toString
    )
  )
  val appReq_NoNotificationProps = AppRequest(
    UUID.randomUUID.toString,
    responseTopicTest,
    RequestBody(
      UUID.randomUUID.toString,
      "createNotification",
      "NotificationModel",
      "CAN_CREATE",
      None,
      None,
      Some(OrgProps(orgId1)),
      Some(SiteProps(siteId1)),
      None,
      None,
      None,
      Some(UUID.randomUUID.toString),
      UUID.randomUUID.toString,
      Calendar.getInstance.toString
    )
  )
  val appReq_NoOrgProps = AppRequest(
    UUID.randomUUID.toString,
    responseTopicTest,
    RequestBody(
      UUID.randomUUID.toString,
      "createNotification",
      "NotificationModel",
      "CAN_CREATE",
      None,
      Some(getAlertProps),
      None,
      Some(SiteProps(siteId1)),
      None,
      Some(getNotificationProps),
      None,
      Some(UUID.randomUUID.toString),
      UUID.randomUUID.toString,
      Calendar.getInstance.toString
    )
  )
  val appReq_NoSiteProps = AppRequest(
    UUID.randomUUID.toString,
    responseTopicTest,
    RequestBody(
      UUID.randomUUID.toString,
      "createNotification",
      "NotificationModel",
      "CAN_CREATE",
      None,
      Some(getAlertProps),
      Some(OrgProps(orgId1)),
      None,
      None,
      Some(getNotificationProps),
      None,
      Some(UUID.randomUUID.toString),
      UUID.randomUUID.toString,
      Calendar.getInstance.toString
    )
  )

  val mappingId1       = "dc0bd8e7-9fa1-4171-b309-b032aa65efb2"
  val mappingId2       = "6a2a4319-ebaf-42dd-a802-b69aef9c6df3"
  val mappingId3       = "bf662519-7e23-4a45-a7d7-965526222809"
  val mappingId_random = UUID.randomUUID().toString

  val alarmType1   = "Disconnect"
  val alarmType2   = "CommFail"
  val alarmType3   = "ConfigFail"
  val ufName1      = "Node Disconnected"
  val ufName2      = "Comunication Failure"
  val ufName3      = "Configuration Failure"
  val description1 = "The Node is Disconnected from the platform"
  val description2 = "Node is unable to communicate with the network"
  val description3 = "Configuration is Not working"
  val nodeModels   = Set("falcon-q", "merlin", "vdkmaster", "cnext", "unode")

  def loadUFAlarms(): Unit = {
    CassandraDB.UFAlarms.createUfAlarmByType(
      UfAlarm(mappingId1, alarmType1, nodeModels, Some(ufName1), Some(description1), true, true)
    )

    CassandraDB.UFAlarms.createUfAlarmByType(
      UfAlarm(mappingId2, alarmType2, nodeModels, Some(ufName2), Some(description2), true, true)
    )

    CassandraDB.UFAlarms.createUfAlarmByType(
      UfAlarm(mappingId3, alarmType3, nodeModels, Some(ufName3), Some(description3), true, false)
    )

    CassandraDB.UFAlarmsById.createUfAlarmById(
      UfAlarm(mappingId1, alarmType1, nodeModels, Some(ufName1), Some(description1), true, true)
    )

    CassandraDB.UFAlarmsById.createUfAlarmById(
      UfAlarm(mappingId2, alarmType2, nodeModels, Some(ufName2), Some(description2), true, true)
    )

    CassandraDB.UFAlarmsById.createUfAlarmById(
      UfAlarm(mappingId3, alarmType3, nodeModels, Some(ufName3), Some(description3), true, false)
    )
    Thread.sleep(1000)

    log.info("3 Sample Records Loaded to UFAlarms Table")
  }

  val getUFAlarmProps: UfAlarmProps =
    UfAlarmProps(
      Some(UUID.randomUUID().toString),
      Some("DiskFail"),
      Some(Seq("falcon-q", "unode")),
      Some("Disk Failure"),
      Some("Disk Failed without any reason"),
      Some(true),
      Some(true)
    )

  val getUFAlarmProps1: UfAlarmProps =
    UfAlarmProps(
      Some(UUID.randomUUID().toString),
      Some("DiskReset"),
      Some(Seq("merlin", "vdkmaster")),
      Some("Disk Failure"),
      Some("Disk Failed without any reason"),
      Some(false),
      Some(false)
    )

  val getUFAlarmPropsExistingName: UfAlarmProps =
    UfAlarmProps(
      Some(UUID.randomUUID().toString),
      Some("DiskFull"),
      Some(Seq("merlin", "unode")),
      Some("Disk Failure"),
      Some("Disk Failed without any reason"),
      Some(true)
    )

  val getUFAlarmProps_Invalid: UfAlarmProps =
    UfAlarmProps(None,
                 Some("alarmType"),
                 Some(Seq("falcon-q")),
                 Some("ufName"),
                 Some("description"),
                 Some(true),
                 Some(true))

  val getUFAlarmPropsForUpdate: UfAlarmProps =
    UfAlarmProps(
      Some(mappingId1),
      None,
      Some(Seq("falcon-q", "unode")),
      Some("Node Disconnected_updated"),
      Some("description_updated"),
      Some(true),
      Some(true)
    )

  val getUFAlarmPropsForUpdate1: UfAlarmProps =
    UfAlarmProps(
      Some(mappingId2),
      None,
      Some(Seq("merlin")),
      Some("Node Disconnected_updated"),
      Some("description_updated"),
      Some(true),
      Some(true)
    )

  val getUFAlarmPropsForBulk: UfAlarmProps =
    UfAlarmProps(
      None,
      None,
      ufAlarmsList = Some(
        Seq(
          UfAlarm(UUID.randomUUID().toString,
                  "alarmType1",
                  nodeModels,
                  Some("ufName1"),
                  Some("description1"),
                  true,
                  true),
          UfAlarm(UUID.randomUUID().toString,
                  "alarmType2",
                  nodeModels,
                  Some("ufName2"),
                  Some("description2"),
                  true,
                  true),
          UfAlarm(UUID.randomUUID().toString,
                  "alarmType3",
                  nodeModels,
                  Some("ufName3"),
                  Some("description3"),
                  true,
                  true),
          UfAlarm(UUID.randomUUID().toString,
                  "alarmType4",
                  nodeModels,
                  Some("ufName4"),
                  Some("description4"),
                  true,
                  true)
        )
      )
    )

  val getInvalidUFAlarmPropsForBulk: UfAlarmProps =
    UfAlarmProps(
      None,
      None,
      ufAlarmsList = Some(
        Seq(
          UfAlarm(UUID.randomUUID().toString,
                  "alarmType5",
                  nodeModels,
                  Some("ufName5"),
                  Some("description5"),
                  true,
                  true),
          UfAlarm(UUID.randomUUID().toString, "alarmType2", nodeModels, None, Some("description2"), true, true)
        )
      )
    )

  val getInvalidUFAlarmPropsForBulk1: UfAlarmProps =
    UfAlarmProps(
      None,
      None,
      ufAlarmsList = Some(
        Seq(
          UfAlarm(UUID.randomUUID().toString,
                  "alarmType1",
                  nodeModels,
                  Some("ufName1"),
                  Some("description1"),
                  true,
                  true),
          UfAlarm(UUID.randomUUID().toString,
                  "alarmType2",
                  nodeModels,
                  Some("ufName2"),
                  Some("description2"),
                  true,
                  true)
        )
      )
    )

  val getInvalidUFAlarmPropsForBulk2: UfAlarmProps =
    UfAlarmProps(
      None,
      None,
      ufAlarmsList = Some(
        Seq(
          UfAlarm(UUID.randomUUID().toString,
                  "alarmType6",
                  nodeModels,
                  Some("ufName1"),
                  Some("description1"),
                  true,
                  true),
          UfAlarm(UUID.randomUUID().toString,
                  "alarmType7",
                  nodeModels,
                  Some("ufName2"),
                  Some("description2"),
                  true,
                  true)
        )
      )
    )

  val appReq_NoUfAlarmProps = AppRequest(
    UUID.randomUUID.toString,
    responseTopicTest,
    RequestBody(
      UUID.randomUUID.toString,
      "createUFAlarm",
      "UFAlarmModel",
      "CAN_CREATE",
      None,
      None,
      Some(OrgProps(orgId1)),
      Some(SiteProps(siteId1)),
      None,
      None,
      None,
      Some(UUID.randomUUID.toString),
      UUID.randomUUID.toString,
      Calendar.getInstance.toString
    )
  )

}
