package com.verizon.netsense.model

case class Fixture(nodeid: String,
                   nodeType: Option[String],
                   minPower0: Option[String],
                   minPower10: Option[String],
                   minPower50: Option[String],
                   minPower100: Option[String],
                   maxPower0: Option[String],
                   maxPower10: Option[String],
                   maxPower50: Option[String],
                   maxPower100: Option[String])

case class FixtureQueryRequestBinded(sensorSampleEvent: SensorSampleEvent,
                                     fixture: Fixture,
                                     driverLevelValue: List[(Long, Double)] = List[(Long, Double)](),
                                     retrialCounter:Int = 1)