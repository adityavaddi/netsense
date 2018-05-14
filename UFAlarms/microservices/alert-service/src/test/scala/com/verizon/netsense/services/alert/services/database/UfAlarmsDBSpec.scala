package com.verizon.netsense.services.alert.services.database

import java.util.UUID

import com.verizon.netsense.services.alert.customexceptions.{AlarmTypeExistsException, UfAlarmPropsMissingException}
import com.verizon.netsense.services.alert.database.{CassandraDB, PhantomConnector}
import com.verizon.netsense.services.alert.model.UfAlarm
import com.verizon.netsense.services.alert.services.UFAlarmRequestHandler
import org.scalatest.MustMatchers._
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FunSuite}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}

class UfAlarmsDBSpec extends FunSuite with PhantomConnector with BeforeAndAfterAll with BeforeAndAfterEach {
  implicit val ex = ExecutionContext.global
  val ufAlarmsAPI = new UFAlarmRequestHandler()

  override def beforeAll(): Unit = {
    super.beforeAll()
    Await.ready(CassandraDBTest.UFAlarms.createTable(), 1.seconds)
    Await.ready(CassandraDBTest.UFAlarmsById.createTable(), 1.seconds)
    session.init()
  }

  override def afterAll(): Unit = {
    super.afterAll()
    Await.ready(CassandraDBTest.UFAlarms.truncateTable(), 1.seconds)
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    val ufAlarm = getUfAlarm
    Await.result(ufAlarmsAPI.createUfAlarm(ufAlarm), 1.seconds)
  }

  override def afterEach(): Unit = {
    super.afterEach()
    Await.ready(CassandraDBTest.UFAlarms.truncateTable(), 1.seconds)
    Await.ready(CassandraDBTest.UFAlarmsById.truncateTable(), 1.seconds)
  }

  val ufAlarmDB = CassandraDB.UFAlarms

  def getUfAlarm: UfAlarm = {
    val mappingId = "dc0bd8e7-9fa1-4171-b309-b032aahclibm"
    UfAlarm(mappingId,
            "HWFail",
            Set("falcon-q", "merlin"),
            Some("Hardware Failure"),
            Some("Hardware failure on the node"),
            true,
            true)
  }

  test("should fetch all UFAlarms") {
    val ufAlarmResult = get_AllUfAlarms
    val result        = Await.result(ufAlarmResult, 1.seconds)
    if (result.nonEmpty) {
      result.foreach(x => {
        assert(x.ufName != null)
        assert(x.alarmType != null)
      })
    } else fail("No Records found")
  }

  test("should create a new UFAlarm") {
    val ufAlarm       = getUfAlarm
    val ufAlarmResult = ufAlarmsAPI.getUfAlarm(ufAlarm.mappingId)
    val result        = Await.result(ufAlarmResult, 1.seconds)
    result match {
      case Some(x) =>
        x.alarmType must be(ufAlarm.alarmType)
        x.mappingId must be(ufAlarm.mappingId)
        x.ufName must be(ufAlarm.ufName)
        x.description must be(ufAlarm.description)
      case None => fail("Record not found")
    }
  }

  test("should not create a new UFAlarm if the alarm type already exists") {

    assertThrows[AlarmTypeExistsException](Await.result(ufAlarmsAPI.createUfAlarm(getUfAlarm), 1.seconds))
  }

  test("should not create a new UFAlarm if the alarm name already exists") {
    val existingUfAlarm = UfAlarm(UUID.randomUUID().toString, "DiskFull", Set("falcon-q"), Some("Hardware Failure"))

    assertThrows[AlarmTypeExistsException](Await.result(ufAlarmsAPI.createUfAlarm(existingUfAlarm), 1.seconds))
  }

  test("should fetch UFAlarm") {

    val ufAlarm = getUfAlarm

    val ufAlarmResult = ufAlarmsAPI.getUfAlarm(ufAlarm.mappingId)
    val result        = Await.result(ufAlarmResult, 1.seconds)
    result match {
      case Some(x) =>
        x.alarmType must be(ufAlarm.alarmType)
        x.mappingId must be(ufAlarm.mappingId)
        x.ufName must be(ufAlarm.ufName)
        x.description must be(ufAlarm.description)
      case None => fail("Record not found")
    }
  }

  test("should fail to fetch UFAlarm that does not exist") {
    val ufAlarmResult = ufAlarmsAPI.getUfAlarm(UUID.randomUUID().toString)
    val result        = Await.result(ufAlarmResult, 1.seconds)
    result match {
      case Some(x) => fail("Fetched Record that does not exist")
      case None    => succeed
    }
  }

  test("should fetch UFAlarms for customer") {
    val ufAlarmResult = get_UfAlarmsForCustomer
    val result        = Await.result(ufAlarmResult, 1.seconds)
    if (result.nonEmpty) {
      result.foreach(x => {
        assert(x.displayToCustomer)
      })
    } else fail("No Records found")
  }

  test("should fetch UFAlarms for partner") {
    val ufAlarmResult = get_UfAlarmsForCustomer
    val result        = Await.result(ufAlarmResult, 1.seconds)
    if (result.nonEmpty) {
      result.foreach(x => {
        assert(x.displayToPartner)
      })
    } else fail("No Records found")
  }

  test("should update existing UFAlarm") {

    val ufAlarm        = getUfAlarm
    val updatedUfAlarm = ufAlarm.copy(ufName = Some("HWFail_updated"), description = Some("description_updated"))

    val ufAlarmResult = ufAlarmsAPI.updateUfAlarm(updatedUfAlarm)
    val result        = Await.result(ufAlarmResult, 1.seconds)
    result match {
      case Some(x) =>
        x.alarmType must be(updatedUfAlarm.alarmType)
        x.mappingId must be(updatedUfAlarm.mappingId)
        x.ufName must be(updatedUfAlarm.ufName)
        x.description must be(updatedUfAlarm.description)
      case None => fail("Updated Record not found")
    }
  }

  test("should fail to update UFAlarm with UfName that already exists") {

    val ufAlarm = UfAlarm("947f53f8-dfa6-4f36-936d-6219ba73b40e",
                          "DiskFull",
                          Set("falcon-q", "merlin"),
                          Some("Disk Volume is Full"))
    Await.result(ufAlarmsAPI.createUfAlarm(ufAlarm), 1.seconds)

    val updatedUfAlarm = ufAlarm.copy(ufName = getUfAlarm.ufName)
    assertThrows[AlarmTypeExistsException](Await.result(ufAlarmsAPI.updateUfAlarm(updatedUfAlarm), 1.seconds))
  }

  test("should fail to update UFAlarm that do not exist") {

    val ufAlarm        = getUfAlarm
    val updatedUfAlarm = ufAlarm.copy(mappingId = UUID.randomUUID().toString)

    val ufAlarmResult = update_UfAlarm(updatedUfAlarm)
    val result        = Await.result(ufAlarmResult, 1.seconds)
    result match {
      case Some(x) => fail("Updated a record that must not exist")
      case None    => succeed
    }
  }

  test("should reset existing UFAlarm") {

    val ufAlarm       = getUfAlarm
    val ufAlarmResult = ufAlarmsAPI.resetUfAlarm(ufAlarm.mappingId)

    val result = Await.result(ufAlarmResult, 1.seconds)
    result match {
      case Some(record) =>
        assert(record.nodeModels.isEmpty)
        assert(record.ufName.getOrElse("").equals(ufAlarm.alarmType))
        assert(!record.displayToPartner)
        assert(!record.displayToCustomer)
      case None => fail("Record not reset")
    }
  }

  test("should fail to reset a UFAlarm that do not exist") {

    val ufAlarmResult = ufAlarmsAPI.resetUfAlarm(UUID.randomUUID().toString)
    val result        = Await.result(ufAlarmResult, 1.seconds)
    result match {
      case Some(record) => fail("Reset a Record that do not exist")
      case None         => succeed
    }
  }

  test("should delete existing UFAlarm") {

    val ufAlarm       = getUfAlarm
    val ufAlarmResult = ufAlarmsAPI.deleteUfAlarm(ufAlarm.mappingId)

    val result = Await.result(ufAlarmResult, 1.seconds)
    if (result) {
      val result1 = Await.result(ufAlarmsAPI.getUfAlarm(ufAlarm.mappingId), 1.seconds)
      result1 match {
        case Some(x) => fail("Fetched a Deleted Record")
        case None    => succeed
      }
    } else fail("Record not deleted")
  }

  test("should fail to delete a UFAlarm that do not exist") {

    val ufAlarmResult = ufAlarmsAPI.deleteUfAlarm(UUID.randomUUID().toString)
    val result        = Await.result(ufAlarmResult, 1.seconds)
    if (result)
      fail("Deleted a Record that do not exist")
    else
      succeed
  }

  test("should create bulk UFAlarm") {
    val ufAlarmList =
      List(
        UfAlarm(UUID.randomUUID().toString, "SWFail", Set("falcon-q", "unode"), Some("Software Failure")),
        UfAlarm(UUID.randomUUID().toString, "DiskFail", Set("falcon-q", "unode"), Some("Disk Failure")),
        UfAlarm(UUID.randomUUID().toString, "PMACFail", Set("falcon-q", "unode"), Some("PMAC Failure"))
      )
    Await.result(ufAlarmsAPI.createBulkUfAlarms(ufAlarmList), 1.seconds)
    val result = Await.result(get_AllUfAlarms, 1.seconds)
    assert(result.nonEmpty)
    assert(result.length >= 3)
  }

  test("should give error when creating bulk UFAlarm with invalid payload") {
    val ufAlarmList =
      List(
        UfAlarm("", "", Set("falcon-q", "unode"), Some("Software Failure")),
        UfAlarm("", "DiskFail", Set("falcon-q", "unode"), Some("Disk Failure")),
        UfAlarm(UUID.randomUUID().toString, "PMACFail", Set("falcon-q", "unode"), Some("PMAC Failure"))
      )
    assertThrows[UfAlarmPropsMissingException](ufAlarmList.map(ufAlarmsAPI.validateUfAlarm))
  }

  test("should give error when creating bulk UFAlarm with existing type") {
    val ufAlarmList =
      List(
        UfAlarm(UUID.randomUUID().toString, "ConfigFail", Set("falcon-q", "unode"), Some("Config Failure")),
        UfAlarm(UUID.randomUUID().toString, "DiskFail", Set("falcon-q", "unode"), Some("Disk Failure")),
        UfAlarm(UUID.randomUUID().toString, "HWFail", Set("falcon-q", "unode"), Some("Hardware Failure"))
      )
    assertThrows[AlarmTypeExistsException](Await.result(ufAlarmsAPI.createBulkUfAlarms(ufAlarmList), 1.seconds))
  }

  test("should give error when creating bulk UFAlarm with existing uf name") {
    val ufAlarmList =
      List(
        UfAlarm(UUID.randomUUID().toString, "ConfigFail", Set("vdkmaster", "merlin"), Some("Config Failure")),
        UfAlarm(UUID.randomUUID().toString, "BootFail", Set("falcon-q", "unode"), Some("Hardware Failure")),
        UfAlarm(UUID.randomUUID().toString, "MemFail", Set("falcon-q", "unode"), Some("PMAC Failure"))
      )
    assertThrows[AlarmTypeExistsException](Await.result(ufAlarmsAPI.createBulkUfAlarms(ufAlarmList), 1.seconds))
  }

}
