package com.verizon.netsense.services.alert.services.database

import java.util.{Calendar, UUID}

import com.verizon.netsense.services.alert.database.PhantomConnector
import com.verizon.netsense.services.alert.model.{AlarmSeverity, Alert, ApAlarm, OrgHierarchy}
import com.verizon.netsense.services.alert.services.AlertRequestHandler
import org.scalatest.MustMatchers._
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FunSuite}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}

class AlertsDBSpec extends FunSuite with PhantomConnector with BeforeAndAfterAll with BeforeAndAfterEach {

  implicit val ex = ExecutionContext.global

  val alertAPI = new AlertRequestHandler()

  override def beforeAll(): Unit = {
    super.beforeAll()
    Await.ready(CassandraDBTest.DeviceAlerts.createTable(), 1.seconds)
    Await.ready(CassandraDBTest.OrgHierarchyByNode.createTable(), 1.seconds)
    Await.ready(CassandraDBTest.AlertsByNode.createTable(), 1.seconds)
    Await.ready(CassandraDBTest.DeviceAlarms.createTable(), 1.seconds)
  }

  override def beforeEach(): Unit = {
    super.afterAll()
    Await.result(alertAPI.lookupAndAddAlert(getAlert()), 2.seconds)
  }

  override def afterEach(): Unit = {
    super.afterAll()
    Await.ready(CassandraDBTest.DeviceAlerts.truncateTable(), 1.seconds)
    Await.ready(CassandraDBTest.AlertsByNode.truncateTable(), 1.seconds)
  }

  override def afterAll(): Unit = {
    super.afterAll()
    Await.ready(CassandraDBTest.DeviceAlerts.truncateTable(), 1.seconds)
    Await.ready(CassandraDBTest.AlertsByNode.truncateTable(), 1.seconds)
    Await.ready(CassandraDBTest.DeviceAlarms.truncateTable(), 1.seconds)
    Await.ready(CassandraDBTest.OrgHierarchyByNode.truncateTable(), 1.seconds)
  }

  def getAlert(): Alert = {
    val alertId = "f91e84ba-760d-4d56-884d-68bbd290d1e5"
    Alert(
      alertId,
      Some("DeviceAlarm"),
      Some("My Msg"),
      "falcon-q_1",
      "org1",
      Some("Critical"),
      "Disconnect",
      Some("Network"),
      Some(Calendar.getInstance().toInstant.toString),
      Some(Calendar.getInstance().toInstant.toString),
      None,
      None,
      None,
      None,
      "site1",
      None,
      None,
      true
    )
  }

  test("should fetch alert for the given alert id") {

    val alert = getAlert()

    val alertResult = alertAPI.getAlertByAlertId(alert.alertId, alert.orgId, alert.siteId)
    val result      = Await.result(alertResult, 2.seconds)
    result match {
      case Some(x) =>
        x.alertId must be(alert.alertId)
        x.orgId must be(alert.orgId)
      case None => fail("Record not found")
    }
  }

  test("should not return any results when fetching alert id that does not exist") {

    val alert = getAlert()

    val alertResult =
      alertAPI.getAlertByAlertId(UUID.randomUUID().toString, UUID.randomUUID().toString, UUID.randomUUID().toString)
    val result = Await.result(alertResult, 2.seconds)
    result match {
      case Some(x) => fail("Fetched Alert that did not exist")
      case None    => succeed
    }
  }

  test("should fetch alerts for the given siteId") {

    val alert = getAlert()

    val alertResult = alertAPI.getAlertByOrgAndSite(alert.orgId, alert.siteId)
    val result      = Await.result(alertResult, 2.seconds)
    if (result.nonEmpty) result.foreach(x => {
      x.siteId must be(alert.siteId)
      x.orgId must be(alert.orgId)
      x.active must be(true)
    })
    else {
      fail("Record with siteid not found")
    }
  }

  test("should not return any alerts for the siteId that does not exist") {

    val alert = getAlert()

    val alertResult = alertAPI.getAlertByOrgAndSite(UUID.randomUUID().toString, UUID.randomUUID().toString)
    val result      = Await.result(alertResult, 2.seconds)
    if (result.isEmpty) {
      succeed
    } else {
      fail("Fetched Alert for SiteId that did not exist")
    }
  }

  test("should fetch alerts for the given nodeId") {

    val alert = getAlert()

    val alertResult = alertAPI.getAlertByNode(alert.orgId, alert.siteId, alert.nodeId)
    val result      = Await.result(alertResult, 2.seconds)
    if (result.nonEmpty) result.foreach(x => {
      x.siteId must be(alert.siteId)
      x.nodeId must be(alert.nodeId)
      x.active must be(true)
    })
    else {
      fail("Record with nodeId not found")
    }
  }

  test("should not return any alerts for nodeId that does not exist") {

    val alert = getAlert()

    val alertResult = alertAPI.getAlertByNode(alert.orgId, alert.siteId, UUID.randomUUID().toString)
    val result      = Await.result(alertResult, 2.seconds)
    if (result.isEmpty) {
      succeed
    } else {
      fail("Fetched Alert for SiteId that did not exist")
    }
  }

  //DELETE
  test("should delete Alert for the given Alert id") {
    val alert = getAlert()

    val deleteResult = alertAPI.deleteAlert(alert.alertId, alert.orgId, alert.siteId)
    if (Await.result(deleteResult, 2.seconds)) {
      val alertResult = alertAPI.getAlertByAlertId(alert.alertId, alert.orgId, alert.siteId)
      Await.result(alertResult, 2.seconds) match {
        case Some(x) => fail("Record not deleted")
        case _       => succeed
      }
    } else {
      fail("Delete Failed")
    }
  }

  test("should give error for deleting the alert that does not exist") {

    val alert = getAlert()

    val invalidDeleteResult = alertAPI.deleteAlert(UUID.randomUUID().toString, alert.orgId, alert.siteId)
    val result              = Await.result(invalidDeleteResult, 2.seconds)
    if (result) {
      fail("Trying to delete non-existing alert")
    } else {
      succeed
    }
  }

  test("should create a new Alert") {

    val alert = getAlert()

    val alertResult = alertAPI.getAlertByAlertId(alert.alertId, alert.orgId, alert.siteId)
    val result      = Await.result(alertResult, 2.seconds)
    result match {
      case Some(x) =>
        result.get.alertId must be(alert.alertId)
        result.get.orgId must be(alert.orgId)
        result.get.active must be(alert.active)
        result.get.siteId must be(alert.siteId)
        result.get.`type` must be(alert.`type`)
        result.get.severity must be(alert.severity)
      case None => fail("Record not found")
    }
  }

  test("should update an existing Alert") {

    val alert = getAlert()

    val updatedAlert = alert.copy(msg = Some("UpdatedMsg"), severity = Some("Major"))
    val updateResult = alertAPI.updateAlert(updatedAlert)
    val result       = Await.result(updateResult, 2.seconds)
    result match {
      case Some(x) =>
        result.get.alertId must be(updatedAlert.alertId)
        result.get.orgId must be(updatedAlert.orgId)
        result.get.active must be(updatedAlert.active)
        result.get.siteId must be(updatedAlert.siteId)
        result.get.msg must be(updatedAlert.msg)
        result.get.severity must be(updatedAlert.severity)
      case None => fail("Record not found")
    }
  }

  test("should not return any results when updating Alert that does not exist") {

    val alert = getAlert()

    val updatedAlert = alert.copy(alertId = UUID.randomUUID().toString)

    val updateResult = alertAPI.updateAlert(updatedAlert)
    val result       = Await.result(updateResult, 2.seconds)
    result match {
      case Some(x) => fail("Updated the Alert that does not exist")
      case None    => succeed
    }
  }

  test("should dismiss Alert") {

    val alert = getAlert()

    val dismissResult = alertAPI.dismissAlert(alert.alertId, alert.orgId, alert.siteId)
    if (Await.result(dismissResult, 2.seconds)) {
      val alertResult = alertAPI.getAlertByAlertId(alert.alertId, alert.orgId, alert.siteId)
      val result      = Await.result(alertResult, 2.seconds)
      result match {
        case Some(x) =>
          result.get.severity must be(Some(AlarmSeverity.Clear.toString))
        case _ => fail("Record not found")
      }
    } else {
      fail("Dismiss Alert failed")
    }
  }

  test("should not dismiss Alert that did not exist") {

    val alert = getAlert()

    val alertResult = alertAPI.dismissAlert(UUID.randomUUID().toString, alert.orgId, alert.siteId)
    if (Await.result(alertResult, 2.seconds)) {
      fail("Dismissing Alert that should not exist")
    } else {
      succeed
    }
  }

  test("should get Alert Sys for alert that exists") {

    val alert = getAlert()

    val alertResult = alertAPI.getActiveAlert(alert.alertId, alert.orgId, alert.siteId)
    val result      = Await.result(alertResult, 2.seconds)
    result match {
      case Some(x) =>
        result.get.alertId must be(alert.alertId)
        result.get.orgId must be(alert.orgId)
        result.get.active must be(true)
      case None => fail("Record not found")
    }
  }

  test("should not get Alert Sys for alert that did not exist") {

    val alert = getAlert()

    val alertResult = alertAPI.getActiveAlert(UUID.randomUUID().toString, alert.orgId, alert.siteId)
    val result      = Await.result(alertResult, 2.seconds)
    result match {
      case Some(x) => fail("Fetched Alert that did not exist")
      case None    => succeed
    }
  }

  test("should fetch alerts for the given nodeId and type from mappings table") {

    val alert = getAlert()

    val mappingResult = checkExistingAlert(alert.nodeId, alert.`type`, alert.orgId, alert.siteId)
    val result        = Await.result(mappingResult, 2.seconds)
    result match {
      case Some(x) =>
        x.nodeId must be(alert.nodeId)
        x.orgId must be(alert.orgId)
        x.siteId must be(alert.siteId)
        val alertResult = Await.result(alertAPI.getAlertByAlertId(alert.alertId, alert.orgId, alert.siteId), 2.seconds)
        alertResult match {
          case Some(y) =>
            y.`type` must be(alert.`type`)
            y.severity must be(alert.severity)
            y.active must be(alert.active)
          case None => fail("Record is in mapping tbale but not in alerts table")
        }
      case None => fail("Record with nodeid, type not found")
    }
  }

  test("should not fetch alerts for the nodeId and type that do not exist") {

    val alert = getAlert()

    val alertResult = checkExistingAlert(UUID.randomUUID().toString, "NoType", alert.orgId, alert.siteId)
    val result      = Await.result(alertResult, 2.seconds)
    result match {
      case Some(x) => fail("Fetching alerts for nodeId and type that do not exist")
      case None    => succeed
    }
  }

  test("should fetch alert_mappings table for the given nodeId and type") {

    val alert = getAlert()

    val alertResult = checkExistingAlertsForNode(alert.nodeId, alert.`type`)
    val result      = Await.result(alertResult, 2.seconds)
    if (result.nonEmpty) result.foreach(x => {
      x.nodeId must be(alert.nodeId)
      x.`type` must be(alert.`type`)
    })
    else {
      fail("Record with nodeid not found")
    }
  }

  test("should not return any mappings for the invalid nodeId and type") {

    val alert = getAlert()

    val alertResult = checkExistingAlertsForNode(alert.nodeId, "Unknown Type")
    val result      = Await.result(alertResult, 2.seconds)
    if (result.isEmpty) {
      succeed
    } else {
      fail("Fetched Mapping for nodeId that did not exist")
    }
  }

  test("should fetch org hierarchy details for the nodeId") {

    val orgHierarchy =
      OrgHierarchy("node1", Some("Falcon"), Some("org1"), Some("Sensity Systems"), Some("site1"), Some("Lowell"))
    Await.result(CassandraDBTest.OrgHierarchyByNode.store(orgHierarchy), 2.seconds)

    val orgHierarchyResult = getOrgHierarchy(orgHierarchy.nodeId)
    val result             = Await.result(orgHierarchyResult, 2.seconds)
    result match {
      case Some(x) => x.nodeId must be(orgHierarchy.nodeId)
      case None    => fail("Org Hierarchy not found for the nodeid")
    }
  }

  test("should store alarm in the Device_alarms table") {
    val alarm =
      ApAlarm("node1", "Disconnect", "Critical", "My Msg", "Network", Calendar.getInstance().toInstant.toString)
    Await.result(persistAlarm(alarm), 2.seconds)

    val alarmResult = CassandraDBTest.DeviceAlarms.getAlarm(alarm.nodeId, alarm.alarmType)
    val result      = Await.result(alarmResult, 2.seconds)
    result match {
      case Some(x) => x.nodeId must be(alarm.nodeId); x.alarmType must be(alarm.alarmType)
      case None    => fail("Org Hierarchy not found for the nodeid")
    }
  }

}
