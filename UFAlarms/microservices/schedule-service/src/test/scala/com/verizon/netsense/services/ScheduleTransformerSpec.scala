package com.verizon.netsense.services

import java.util.UUID

import com.verizon.netsense.constants.Constants
import com.verizon.netsense.enums.Weekday
import com.verizon.netsense.exceptions.CustomExceptions.{MalformedScheduleDataException, NoNodesFoundToSendScheduleException, NoScheduleFoundException}
import com.verizon.netsense.helper.BaseSpec
import com.verizon.netsense.model._
import com.verizon.netsense.service.ScheduleTransformer
import org.joda.time.format.DateTimeFormat
import org.joda.time.{DateTime, DateTimeZone}

class ScheduleTransformerSpec extends BaseSpec {

  "Schedule Transformer" should "Transform LightingSchedule to STSModel" in {
      var events:Vector[ScheduleEvent] = Vector()
      val days1 = Vector(Weekday.sun, Weekday.sat)
      val action1 = Vector(
        new Action(Some(0), Some("sunrise+30")),
        new Action(Some(100), Some("05:00:00")),
        new Action(Some(0), Some("23:00:00")),
        new Action(Some(100), Some("sunset"))
      )
      events = events :+ new ScheduleEvent(days=Some(days1), date=None, actions=action1,
        photocell_enabled = Some(false), photocell_highLevel = None, photocell_lowLevel = None)

      val action2 = Vector(
        new Action(Some(0), Some("sunrise")),
        new Action(Some(100), Some("sunset"))
      )
      events = events :+ new ScheduleEvent(days=None, date=Some("2015-12-31"), actions=action2,
        photocell_enabled = Some(false), photocell_highLevel = None, photocell_lowLevel = None)

      val days3 = Vector(Weekday.mon, Weekday.tue, Weekday.thu, Weekday.fri, Weekday.wed)
      val action3 = Vector(
        new Action(Some(0), Some("sunrise+30")),
        new Action(Some(50), Some("23:00:00")),
        new Action(Some(100), Some("05:00:00")),
        new Action(Some(50), Some("sunrise")),
        new Action(Some(50), Some("sunset-30")),
        new Action(Some(100), Some("sunset"))
      )
      events = events :+ new ScheduleEvent(days=Some(days3), date=None, actions=action3,
        photocell_enabled = Some(false), photocell_highLevel = None, photocell_lowLevel = None)

    val network = new Network(Some("2:00:00"), Some(73), Some("22:00:00"), Some(37),
      photocell_enabled = Some(false), photocell_highLevel = None, photocell_lowLevel = None)

    val scheduleid = UUID.randomUUID().toString
    val ls = new LightingSchedule(events, Some(network), Some(Vector("cnext_1")), "37.380996", "-121.992299", Some(scheduleid))

    val currentTime = System.currentTimeMillis()
    def tokenPrefix = ScheduleTransformer().tokenPrefix(currentTime)
    val sts = ScheduleTransformer(currentTime).transform(ls)

    assert(sts != null)
    assert(sts.id.contains(tokenPrefix + scheduleid))
    assert(sts.slots.isDefined)
    assert(sts.network.size == 2)
    assert(sts.name == "LightingScheduledEvent")
  }

  it should "return date over day values" in {
    var events: Vector[ScheduleEvent] = Vector()
    val scheduleid = UUID.randomUUID().toString
    val days1 = Vector(Weekday.sun, Weekday.sat)
    val action1 = Vector(
      new Action(Some(0), Some("sunrise+30")),
      new Action(Some(100), Some("05:00:00")),
      new Action(Some(0), Some("23:00:00")),
      new Action(Some(100), Some("sunset"))
    )
    events = events :+ new ScheduleEvent(days=Some(days1), date=None, actions=action1,
      photocell_enabled = Some(false), photocell_highLevel = None, photocell_lowLevel = None)

    val action2 = Vector(
      new Action(Some(0), Some("sunrise")),
      new Action(Some(100), Some("sunset"))
    )
    //Get Todays Date
    val zone = DateTimeZone.forID("America/Los_Angeles")
    val now = new DateTime(zone)
    val pattern = "yyyy-MM-dd"
    val formatter = DateTimeFormat.forPattern(pattern)
    val created = formatter.print(now)
    events = events :+ new ScheduleEvent(days = None, date = Some(created), actions = action2,
      photocell_enabled = Some(false), photocell_highLevel = None, photocell_lowLevel = None)

    val days3 = Vector(Weekday.mon, Weekday.tue, Weekday.thu, Weekday.fri, Weekday.wed)
    val action3 = Vector(
      new Action(Some(0), Some("sunrise+30")),
      new Action(Some(50), Some("23:00:00")),
      new Action(Some(100), Some("05:00:00")),
      new Action(Some(50), Some("sunrise")),
      new Action(Some(50), Some("sunset-30")),
      new Action(Some(100), Some("sunset"))
    )
    events = events :+ new ScheduleEvent(days=Some(days3), date=None, actions=action3,
      photocell_enabled = Some(false), photocell_highLevel = None, photocell_lowLevel = None)

    val network = new Network(Some("2:00:00"), Some(73), Some("22:00:00"), Some(37),
      photocell_enabled = Some(false), photocell_highLevel = None, photocell_lowLevel = None)

    val ls = new LightingSchedule(events, Some(network), Some(Vector("cnext_1")), "37.380996", "-121.992299", scheduleid = Some(scheduleid))
    val sts = ScheduleTransformer().transform(ls)

    //event with two slots should get selected
    assert(sts.slots.get.size == 2)
  }

  it should "handle photocell mode in no network" in {
    var events: Vector[ScheduleEvent] = Vector()
    val scheduleid = UUID.randomUUID().toString
    val days1 = Vector(Weekday.sun, Weekday.sat, Weekday.mon, Weekday.tue, Weekday.thu, Weekday.fri, Weekday.wed)
    val action1 = Vector(
      new Action(Some(100), Some("05:00:00"))
    )
    events = events :+ new ScheduleEvent(days=Some(days1), date=None, actions=action1,
      photocell_enabled = Some(false), photocell_highLevel = None, photocell_lowLevel = None)

    val network = new Network(Some("2:00:00"), Some(73), Some("22:00:00"), Some(37),
      photocell_enabled = Some(true), photocell_highLevel = Some(90), photocell_lowLevel = Some(10))

    val ls = new LightingSchedule(events, Some(network), Some(Vector("cnext_1", "cnext_2")), "37.380996", "-121.992299", scheduleid = Some(scheduleid))

    val currentTime = System.currentTimeMillis()
    def tokenPrefix = ScheduleTransformer().tokenPrefix(currentTime)
    val sts = ScheduleTransformer(currentTime).transform(ls)

    assert(sts.slots.isDefined)
    assert(sts.nodeids.nonEmpty)
    assert(sts.id.contains(tokenPrefix + scheduleid))

    assert(sts.network.head.mode == Constants.PHOTOCELL)
    assert(sts.network.head.level == 10)
    assert(sts.network.head.lowLevel.contains(90))
  }

  it should "use default high and low values in no network" in {
    var events: Vector[ScheduleEvent] = Vector()
    val scheduleid = UUID.randomUUID().toString
    val days1 = Vector(Weekday.sun, Weekday.sat, Weekday.mon, Weekday.tue, Weekday.thu, Weekday.fri, Weekday.wed)
    val action1 = Vector(
      new Action(Some(0), Some("sunrise+30")),
      new Action(Some(100), Some("sunset"))
    )
    events = events :+ new ScheduleEvent(days=Some(days1), date=None, actions=action1,
      photocell_enabled = Some(false), photocell_highLevel = None, photocell_lowLevel = None)

    val network = new Network(Some("2:00:00"), None, Some("22:00:00"), None,
      photocell_enabled = Some(false), photocell_highLevel = None, photocell_lowLevel = None)

    val ls = new LightingSchedule(events, Some(network), Some(Vector("cnext_1", "cnext_2")), "37.380996", "-121.992299", scheduleid = Some(scheduleid))

    val currentTime = System.currentTimeMillis()
    def tokenPrefix = ScheduleTransformer().tokenPrefix(currentTime)
    val sts = ScheduleTransformer(currentTime).transform(ls)

    assert(sts.id.contains(tokenPrefix + scheduleid))
    assert(sts.slots.isDefined)
    assert(sts.nodeids.nonEmpty)
    assert(sts.network.size == 2)
    assert(sts.network.head.level == 0 || sts.network.head.level == 100)
    assert(sts.network.head.mode == Constants.NORMAL)
  }

  it should "use default high and low values in no network + photocell" in {
    var events: Vector[ScheduleEvent] = Vector()
    val scheduleid = UUID.randomUUID().toString
    val days1 = Vector(Weekday.sun, Weekday.sat, Weekday.mon, Weekday.tue, Weekday.thu, Weekday.fri, Weekday.wed)
    val action1 = Vector(
      new Action(Some(0), Some("sunrise+30")),
      new Action(Some(100), Some("sunset"))
    )
    events = events :+ new ScheduleEvent(days=Some(days1), date=None, actions=action1,
      photocell_enabled = Some(false), photocell_highLevel = None, photocell_lowLevel = None)

    val network = new Network(None, None , None, None,
      photocell_enabled = Some(true), photocell_highLevel = None, photocell_lowLevel = None)

    val ls = new LightingSchedule(events, Some(network), Some(Vector("cnext_1", "cnext_2")), "37.380996", "-121.992299", scheduleid = Some(scheduleid))
    val currentTime = System.currentTimeMillis()
    def tokenPrefix = ScheduleTransformer().tokenPrefix(currentTime)
    val sts = ScheduleTransformer(currentTime).transform(ls)

    assert(sts.slots.isDefined)
    assert(sts.nodeids.nonEmpty)
    assert(sts.id.contains(tokenPrefix + scheduleid))

    assert(sts.network.size == 1)
    assert(sts.network.head.level == 0 || sts.network.head.level == 100)
    assert(sts.network.head.mode == Constants.PHOTOCELL)
  }

  it should "handle photocell mode in normal network" in {
    var events: Vector[ScheduleEvent] = Vector()
    val scheduleid = UUID.randomUUID().toString
    val days1 = Vector(Weekday.sun, Weekday.sat, Weekday.mon, Weekday.tue, Weekday.thu, Weekday.fri, Weekday.wed)
    val action1 = Vector(
      new Action(Some(50), Some("23:00:00")),
      new Action(Some(100), Some("05:00:00"))
    )
    events = events :+ new ScheduleEvent(days=Some(days1), date=None, actions=action1,
      photocell_enabled = Some(true), photocell_highLevel = Some(80), photocell_lowLevel = Some(20))

    val network = new Network(Some("23:00:00"), Some(73), Some("6:00:00"), Some(37),
      photocell_enabled = Some(false), photocell_highLevel = None, photocell_lowLevel = None)

    val ls = new LightingSchedule(events, Some(network), Some(Vector("cnext_1", "cnext_2")), "37.380996", "-121.992299", scheduleid = Some(scheduleid))

    val currentTime = System.currentTimeMillis()
    def tokenPrefix = ScheduleTransformer().tokenPrefix(currentTime)
    val sts = ScheduleTransformer(currentTime).transform(ls)

    assert(sts.slots.get.size == 1)
    assert(sts.slots.get.head.time == None)
    assert(sts.slots.get.head.level == 20)
    assert(sts.slots.get.head.lowLevel == Some(80))
    assert(sts.slots.get.head.mode == Constants.PHOTOCELL)

    assert(!sts.nodeids.isEmpty)
    assert(sts.id.contains(tokenPrefix + scheduleid))
    assert(!sts.network.isEmpty)
  }

  it should "handle case when there is no low time low level" in {
    var events: Vector[ScheduleEvent] = Vector()
    val scheduleid = UUID.randomUUID().toString
    val days1 = Vector(Weekday.sun, Weekday.sat, Weekday.mon, Weekday.tue, Weekday.thu, Weekday.fri, Weekday.wed)
    val action1 = Vector(
      new Action(Some(86), Some("06:40:00"))
    )
    events = events :+ new ScheduleEvent(days=Some(days1), date=None, actions=action1,
      photocell_enabled = Some(false), photocell_highLevel = None, photocell_lowLevel = None)
    val network = new Network(Some("10:00:00"), Some(100), None, None,
      photocell_enabled = Some(false), photocell_highLevel = None, photocell_lowLevel = None)

    val ls = new LightingSchedule(events, Some(network), Some(Vector("cnext_1")), "37.380996", "-121.992299", scheduleid = Some(scheduleid))

    val currentTime = System.currentTimeMillis()
    def tokenPrefix = ScheduleTransformer().tokenPrefix(currentTime)
    val sts = ScheduleTransformer(currentTime).transform(ls)

    assert(sts.id.contains(tokenPrefix + scheduleid))
    assert(sts.slots.get.size == 1)
    assert(sts.network.size == 1)
    assert(sts.network.head.time.get.hr == 18 || sts.network.head.time.get.hr == 17)
    assert(sts.network.head.time.get.min == 0)
    assert(sts.network.head.time.get.sec == 0)
    assert(sts.network.head.lowLevel.isEmpty)
    assert(sts.network.head.level == 100)
    assert(sts.network.head.mode == Constants.NORMAL)  }

  it should "handle case when there is no high time level" in {
    var events: Vector[ScheduleEvent] = Vector()
    val scheduleid = UUID.randomUUID().toString
    val days1 = Vector(Weekday.sun, Weekday.sat, Weekday.mon, Weekday.tue, Weekday.thu, Weekday.fri, Weekday.wed)
    val action1 = Vector(
      new Action(Some(86), Some("06:40:00"))
    )
    events = events :+ new ScheduleEvent(days=Some(days1), date=None, actions=action1,
      photocell_enabled = Some(false), photocell_highLevel = None, photocell_lowLevel = None)
    val network = new Network(None, None, Some("10:00:00"), Some(10),
      photocell_enabled = Some(false), photocell_highLevel = None, photocell_lowLevel = None)

    val ls = new LightingSchedule(events, Some(network), Some(Vector("cnext_1")), "37.380996", "-121.992299", scheduleid = Some(scheduleid))
    val sts = ScheduleTransformer().transform(ls)

    assert(sts.slots.get.size == 1)
    assert(sts.network.size == 1)
    assert(sts.network.head.time.get.hr == 18 || sts.network.head.time.get.hr == 17)
    assert(sts.network.head.time.get.min == 0)
    assert(sts.network.head.time.get.sec == 0)
    assert(sts.network.head.lowLevel == None)
    assert(sts.network.head.level == 10)
    assert(sts.network.head.mode == Constants.NORMAL)
  }

  it should "handle case when there is no photocell_highLevel time photocell_lowLevel" in {
    var events: Vector[ScheduleEvent] = Vector()
    val scheduleid = UUID.randomUUID().toString
    val days1 = Vector(Weekday.sun, Weekday.sat, Weekday.mon, Weekday.tue, Weekday.thu, Weekday.fri, Weekday.wed)
    val action1 = Vector(
      new Action(Some(86), Some("06:40:00"))
    )
    events = events :+ new ScheduleEvent(days=Some(days1), date=None, actions=action1,
      photocell_enabled = Some(true), photocell_highLevel = None, photocell_lowLevel = None)
    val network = new Network(None, None, Some("10:00:00"), Some(10),
      photocell_enabled = Some(true), photocell_highLevel = None, photocell_lowLevel = None)

    val ls = new LightingSchedule(events, Some(network), Some(Vector("cnext_1")), "37.380996", "-121.992299", scheduleid = Some(scheduleid))
    val sts = ScheduleTransformer().transform(ls)

    assert(sts.slots.get.size == 1)
    assert(sts.slots.head.head.mode == Constants.PHOTOCELL)
    assert(sts.slots.head.head.time.isEmpty)
    assert(sts.slots.head.head.lowLevel.contains(100))
    assert(sts.slots.head.head.level == 0)

    assert(sts.network.size == 1)
    assert(sts.network.head.mode == Constants.PHOTOCELL)
    assert(sts.network.head.time.isEmpty)
    assert(sts.network.head.lowLevel.contains(100))
    assert(sts.network.head.level == 0)
  }

  it should "work with the single days event list" in {
    var events:Vector[ScheduleEvent] = Vector()
    val days1 = Vector(Weekday.sat, Weekday.mon, Weekday.tue, Weekday.thu, Weekday.fri, Weekday.wed)
    val action1 = Vector(new Action(Some(86),Some("06:40:00")))
    events = events :+ new ScheduleEvent(days=Some(days1), date=None, actions=action1,
      photocell_enabled = Some(false), photocell_highLevel = None, photocell_lowLevel = None)

    val network = new Network(Some("00:00:00"),Some(100),None,None,
      photocell_enabled = Some(false), photocell_highLevel = None, photocell_lowLevel = None)

    val ls = new LightingSchedule(events, Some(network), Some(Vector("N06c01393")), "42.614468", "-71.324623", Some("358a9272-326c-4cd5-85b9-9ee59a5723e9"))
    val sts = ScheduleTransformer().transform(ls)

    assert(sts.slots.isDefined)
    assert(sts.slots.size == 1)
    assert(sts.network.size == 1)
    assert(sts != null)
  }


  it should "apply fallback values for NoNetwork" in {
    val scheduleid = UUID.randomUUID().toString
    val days = Vector(Weekday.sat, Weekday.mon, Weekday.tue, Weekday.thu, Weekday.fri, Weekday.wed)
    val ls: LightingSchedule = LightingSchedule(events = Vector(ScheduleEvent(days = Some(days),
      actions = Vector(Action(level = Some(0), time = Some("12:12:12"))),
      photocell_enabled = Some(false), photocell_highLevel = None, photocell_lowLevel = None)),
      network = Some(Network(None, None, None, None, None, None, None)), nodes = Some(Vector("cnext_1")), scheduleid = Some(scheduleid))

    val currentTime = System.currentTimeMillis()
    def tokenPrefix = ScheduleTransformer().tokenPrefix(currentTime)
    val sts = ScheduleTransformer(currentTime).transform(ls)

    assert(sts.id.contains(tokenPrefix + scheduleid))
    assert(sts.slots.isDefined)
    assert(sts.nodeids.nonEmpty)

    assert(sts.network.size == 1)
    assert(sts.network.head.level == 0)
    assert(sts.network.head.lowLevel.contains(100))
    assert(sts.network.head.time.isEmpty)
    assert(sts.network.head.mode == Constants.PHOTOCELL)
  }

  it should "apply fallback values for missing events " in {
    val scheduleid = UUID.randomUUID().toString
    val ls: LightingSchedule = LightingSchedule(events = null,
      network = Some(new Network(Some("00:00:00"),Some(100),None,None, photocell_enabled = Some(false), photocell_highLevel = None, photocell_lowLevel = None)),
      nodes = Some(Vector("nodeid")),
      scheduleid = Some(scheduleid))

    an[MalformedScheduleDataException] mustBe thrownBy(ScheduleTransformer().transform(ls))
  }

  it should "throw NoNodesFoundToSendSchedule exception for missing nodeid for LightSchedule" in {
    val scheduleid = UUID.randomUUID().toString
    var events:Vector[ScheduleEvent] = Vector()
    val days1 = Vector(Weekday.sat, Weekday.mon, Weekday.tue, Weekday.thu, Weekday.fri, Weekday.wed)
    val action1 = Vector(new Action(Some(86),Some("06:40:00")))
    events = events :+ new ScheduleEvent(days=Some(days1), date=None, actions=action1,
      photocell_enabled = Some(false), photocell_highLevel = None, photocell_lowLevel = None)

    val ls: LightingSchedule = LightingSchedule(events = events,
      network = Some(Network(None, None, None, None,None, None, None)), nodes = Some(Vector.empty), scheduleid = Some(scheduleid))

    an[NoNodesFoundToSendScheduleException] mustBe thrownBy(ScheduleTransformer().transform(ls))
  }

  it should "throw NoScheduleFound exception for missing scheduleId for LightSchedule" in {
    val scheduleid = UUID.randomUUID().toString
    var events:Vector[ScheduleEvent] = Vector()
    val days1 = Vector(Weekday.sat, Weekday.mon, Weekday.tue, Weekday.thu, Weekday.fri, Weekday.wed)
    val action1 = Vector(new Action(Some(86),Some("06:40:00")))
    events = events :+ new ScheduleEvent(days=Some(days1), date=None, actions=action1,
      photocell_enabled = Some(false), photocell_highLevel = None, photocell_lowLevel = None)

    val lightingSchedule: LightingSchedule = LightingSchedule(events = events,
      network = Some(Network(None, None, None, None, None, None, None)), nodes = Some(Vector("nodeid")), scheduleid = None)

    an[NoScheduleFoundException] mustBe thrownBy(ScheduleTransformer().transform(lightingSchedule))
  }

}
