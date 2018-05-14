package com.verizon.netsense.services

import java.util.UUID
import com.verizon.netsense.helper.BaseSpec
import com.verizon.netsense.model._
import com.verizon.netsense.service.ScheduleTransformer

class SCHTransformerSpec extends BaseSpec {

  "SCH Transformer" should "Transform STS object to vector of slots" in {
    var slotList:Vector[SlotAction] = Vector()
    slotList = slotList :+ SlotAction(level= 10, time= Some(new ScheduleCalendar(10, 23, 30, 2018, 2, 4, 22)), mode="normal", lowLevel = None)
    slotList = slotList :+ SlotAction(level= 60, time= Some(new ScheduleCalendar(5, 21, 31, 2018, 2, 4, 22)), mode="normal", lowLevel = None)
    slotList = slotList :+ SlotAction(level= 80, time= Some(new ScheduleCalendar(23, 24, 32, 2018, 2, 4, 22)), mode="normal", lowLevel = None)

    var networkList:Vector[SlotAction] = Vector()
    networkList = networkList :+ SlotAction(level= 100, time= Some(new ScheduleCalendar(20, 1, 2, 2018, 2, 4, 22)), mode="normal", lowLevel = None)
    networkList = networkList :+ SlotAction(level= 0, time= Some(new ScheduleCalendar(6, 10, 12, 2018, 2, 4, 22)), mode="normal", lowLevel = None)

    val id = UUID.randomUUID().toString
    val model = new STSModel(id, slots = Some(slotList), network = networkList, nodeids = Vector("cnext_1", "cnext_2"))
    val dslots: Vector[SlotPayload] = ScheduleTransformer.transform(model)

    assert(dslots.size == 5)
    assert(!dslots.exists(dmodel => dmodel.lctl.mask != 1))

    //Check wday mday claculations are correct or not
    assert(dslots(0).cal == SchCalendar(12, 10, 6, 16, 2097152, 2, 4))

    //assign priority 7 and qualifier 0 for normal network
    assert(dslots.filter(dm => dm.lctl.quals == 0).size == 3)
    assert(dslots.filter(dm => dm.lctl.pri == 7).size == 3)

    //assign priority 2 and qualifier 4 for no network
    assert(dslots.filter(dm => dm.lctl.quals == 4).size == 2)
    assert(dslots.filter(dm => dm.lctl.pri == 2).size == 2)
  }

  it should "should sort vector of slots by time" in {
    var slotList:Vector[SlotAction] = Vector()
    slotList = slotList :+ SlotAction(level= 10, time= Some(new ScheduleCalendar(10, 23, 30, 2018, 2, 4, 22)), mode="normal", lowLevel = None)
    slotList = slotList :+ SlotAction(level= 60, time= Some(new ScheduleCalendar(5, 21, 31, 2018, 2, 4, 22)), mode="normal", lowLevel = None)
    slotList = slotList :+ SlotAction(level= 80, time= Some(new ScheduleCalendar(23, 24, 32, 2018, 2, 4, 22)), mode="normal", lowLevel = None)

    var networkList:Vector[SlotAction] = Vector()
    networkList = networkList :+ SlotAction(level= 100, time= Some(new ScheduleCalendar(20, 1, 2, 2018, 2, 4, 22)), mode="normal", lowLevel = None)
    networkList = networkList :+ SlotAction(level= 0, time= Some(new ScheduleCalendar(6, 10, 12, 2018, 2, 4, 22)), mode="normal", lowLevel = None)

    val id = UUID.randomUUID().toString
    val model = new STSModel(id, slots = Some(slotList), network = networkList, nodeids = Vector("cnext_1", "cnext_2"))
    val dslots: Vector[SlotPayload] = ScheduleTransformer.transform(model)

    assert(dslots(0).lctl.level.head == 0)
    assert(dslots(1).lctl.level.head == 100)

    assert(dslots(2).lctl.level.head == 60)
    assert(dslots(3).lctl.level.head == 10)
    assert(dslots(4).lctl.level.head == 80)
  }

  it should "assign priority 2 and qualifier 6 for photocell no network" in {
    var slotList:Vector[SlotAction] = Vector()
    slotList = slotList :+ SlotAction(level= 10, time= Some(new ScheduleCalendar(10, 23, 30, 2018, 2, 4, 22)), mode="normal", lowLevel = None)
    slotList = slotList :+ SlotAction(level= 60, time= Some(new ScheduleCalendar(5, 21, 31, 2018, 2, 4, 22)), mode="normal", lowLevel = None)
    slotList = slotList :+ SlotAction(level= 80, time= Some(new ScheduleCalendar(23, 24, 32, 2018, 2, 4, 22)), mode="normal", lowLevel = None)

    var networkList:Vector[SlotAction] = Vector()
    networkList = networkList :+ SlotAction(level= 10, time= None, mode="photocell", lowLevel = Some(80))

    val id = UUID.randomUUID().toString
    val model = new STSModel(id, slots = Some(slotList), network = networkList, nodeids = Vector("cnext_1"))
    val dslots: Vector[SlotPayload] = ScheduleTransformer.transform(model)

    //assign priority 7 and qualifier 0 for normal network
    assert(dslots.filter(dm => dm.lctl.quals == 0).size == 3)
    assert(dslots.filter(dm => dm.lctl.pri == 7).size == 3)

    //assign priority 1 and qualifier 6 for no network low light
    assert(dslots(0).lctl.quals == 6 && dslots(0).lctl.pri == 2)
    assert(dslots(0).lctl.level.head == 80 && dslots(0).id == 0)

    //assign priority 2 and qualifier 4 for no network normal light
    assert(dslots(1).lctl.quals == 4 && dslots(1).lctl.pri == 3)
    assert(dslots(1).lctl.level.head == 10 && dslots(1).id == 1)
  }

  it should "assign priority 6 and qualifier 2 for photocell normal network" in {
    var slotList:Vector[SlotAction] = Vector()
    slotList = slotList :+ SlotAction(level= 20, time= None, mode="photocell", lowLevel = Some(60))

    var networkList:Vector[SlotAction] = Vector()
    networkList = networkList :+ SlotAction(level= 10, time= None, mode="photocell", lowLevel = Some(80))

    val id = UUID.randomUUID().toString
    val model = new STSModel(id, slots = Some(slotList), network = networkList, nodeids = Vector("cnext_1", "cnext_2"))
    val dslots: Vector[SlotPayload] = ScheduleTransformer.transform(model)

    assert(dslots(2).lctl.quals == 2 && dslots(2).lctl.pri == 6)
    assert(dslots(2).lctl.level.head == 60 && dslots(2).id == 2)

    assert(dslots(3).lctl.quals == 0 && dslots(3).lctl.pri == 7)
    assert(dslots(3).lctl.level.head == 20 && dslots(3).id == 3)
  }

  it should "handles network with just one slot" in {
    var slotList:Vector[SlotAction] = Vector()
    slotList = slotList :+ SlotAction(level= 10, time= Some(new ScheduleCalendar(10, 23, 30, 2018, 2, 4, 22)), mode="normal", lowLevel = None)
    slotList = slotList :+ SlotAction(level= 60, time= Some(new ScheduleCalendar(5, 21, 31, 2018, 2, 4, 22)), mode="normal", lowLevel = None)

    var networkList:Vector[SlotAction] = Vector()
    networkList = networkList :+ SlotAction(level= 0, time= Some(new ScheduleCalendar(6, 10, 12, 2018, 2, 4, 22)), mode="normal", lowLevel = None)

    val id = UUID.randomUUID().toString
    val model = new STSModel(id, slots = Some(slotList), network = networkList, nodeids = Vector("cnext_1", "cnext_2"))
    val dslots: Vector[SlotPayload] = ScheduleTransformer.transform(model)

    assert(dslots(0).cal == SchCalendar(12, 10, 6, 16, 2097152, 2, 4))
    assert(dslots(0).lctl.level.head == 0)

    assert(dslots.size == 3)
  }

}
