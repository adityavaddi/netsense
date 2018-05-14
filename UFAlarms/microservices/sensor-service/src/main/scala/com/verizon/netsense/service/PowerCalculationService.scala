package com.verizon.netsense.service

import com.verizon.netsense.constants.SensorSampleConstants
import com.verizon.netsense.exceptions.CustomExceptions.{DriverLevelOutOfRangeException, FixtureValueNotFound, UnableToConvertStringToDouble, UnableToGetLtDataFromDB}
import com.verizon.netsense.model.{DeviceAlarm, Fixture, FixtureQueryRequestBinded}
import com.verizon.netsense.utils.Logging

/**
  * Created by nalamte on 3/24/18.
  */

object PowerCalculationService extends Logging {

  def calculatePower(fQB: FixtureQueryRequestBinded): List[DeviceAlarm] = {
    fQB.driverLevelValue.headOption match {
      case Some(dlevel) => checkPower(dlevel._2, fQB.fixture, fQB.sensorSampleEvent.l.v.toDouble, fQB.sensorSampleEvent.l.n)
      case None => throw new UnableToGetLtDataFromDB(s"Driver level ${fQB.driverLevelValue} missing from cassandra") {
        override val underlyingMessage = ""
      }
    }
  }

  lazy val convertStringToDouble: (Option[String], String, String) => Double = (fixtureValue, nodeId, fixtureName) => {
    fixtureValue match {
      case Some(value) => try {
        value.toDouble
      } catch {
        case ex: Exception => throw new UnableToConvertStringToDouble(s"Unable to convert $fixtureValue from string " +
          s"to integer for nodeid $nodeId") {
          override val underlyingMessage = ""
        }
      }
      case None => throw new FixtureValueNotFound(s"Unable to fetch the fixture values for nodeId $nodeId and for " +
        s"fixture $fixtureName") {
        override val underlyingMessage = ""
      }
    }
  }

  def checkPower(driverLevel: Double, f: Fixture, powerLevel: Double, nodeid: String): List[DeviceAlarm] = driverLevel match {
    case level if level >= 0 && level <= 10 => getPower(level, powerLevel, 0, 10,
      convertStringToDouble(f.minPower0, nodeid, "minPower0"),
      convertStringToDouble(f.minPower10, nodeid, "minPower10"),
      convertStringToDouble(f.maxPower0, nodeid, "maxPower0"),
      convertStringToDouble(f.maxPower10, nodeid, "maxPower10"),
      nodeid)
    case level if level >= 10 && level <= 50 => getPower(level, powerLevel, 10, 50,
      convertStringToDouble(f.minPower10, nodeid, "minPower10"),
      convertStringToDouble(f.minPower50, nodeid, "minPower50"),
      convertStringToDouble(f.maxPower10, nodeid, "maxPower10"),
      convertStringToDouble(f.maxPower50, nodeid, "maxPower50"),
      nodeid)
    case level if level >= 50 && level <= 100 => getPower(level, powerLevel, 50, 100,
      convertStringToDouble(f.minPower50, nodeid, "minPower50"),
      convertStringToDouble(f.minPower100, nodeid, "minPower100"),
      convertStringToDouble(f.maxPower50, nodeid, "maxPower50"),
      convertStringToDouble(f.maxPower100, nodeid, "maxPower100"),
      nodeid)
    case _ => throw new DriverLevelOutOfRangeException(s"Driver level $driverLevel is not in range of 0 to 100 for nodeid $nodeid") {
      override val underlyingMessage = ""
    }
  }

  def getPower(driverLevel: Double,
               powerLevel: Double,
               driver1: Double,
               driver2: Double,
               minPower1: Double,
               minPower2: Double,
               maxPower1: Double,
               maxPower2: Double,
               nodeid: String): List[DeviceAlarm] = {
    val minRange = calculatePowerLevel(driverLevel, driver1, driver2, minPower1, minPower2)
    val maxRange = calculatePowerLevel(driverLevel, driver1, driver2, maxPower1, maxPower2)
    log.debug(s" PowerLevel: $powerLevel, minRange: $minRange, maxRange: $maxRange")
    if (powerLevel < minRange)
      generateAlarms("UnderPower", nodeid, driverLevel)
    else if (powerLevel > maxRange)
      generateAlarms("OverPower", nodeid, driverLevel)
    else
      generateAlarms("Clear", nodeid, driverLevel)
  }

  def generateAlarms(_alarmType: String, _nodeid: String,_driverLevel: Double): List[DeviceAlarm] = _alarmType match {
    case "UnderPower" => List(DeviceAlarm(nodeid = _nodeid,
      alarmType = _alarmType,
      alarmSeverity = SensorSampleConstants.criticalSeverity,
      msg = SensorSampleConstants.underPowerMsg+_driverLevel),
      DeviceAlarm(nodeid = _nodeid,
        alarmType = "OverPower",
        alarmSeverity = SensorSampleConstants.clearSeverity,
        msg = SensorSampleConstants.overPowerMsg+_driverLevel))
    case "OverPower" => List(DeviceAlarm(nodeid = _nodeid,
      alarmType = _alarmType,
      alarmSeverity = SensorSampleConstants.criticalSeverity,
      msg = SensorSampleConstants.overPowerMsg+_driverLevel),
      DeviceAlarm(nodeid = _nodeid,
        alarmType = "UnderPower",
        alarmSeverity = SensorSampleConstants.clearSeverity,
        msg = SensorSampleConstants.underPowerMsg+_driverLevel))
    case "Clear" => List(DeviceAlarm(nodeid = _nodeid,
      alarmType = "UnderPower",
      alarmSeverity = SensorSampleConstants.clearSeverity,
      msg = SensorSampleConstants.underPowerMsg+_driverLevel),
      DeviceAlarm(nodeid = _nodeid,
        alarmType = "OverPower",
        alarmSeverity = SensorSampleConstants.clearSeverity,
        msg = SensorSampleConstants.overPowerMsg+_driverLevel))
  }

  def calculatePowerLevel(driverLevel: Double, driver1: Double, driver2: Double, power1: Double, power2: Double): Double = {
    val slope = (power2 - power1) / (driver2 - driver1)
    val constant = power1 - (driver1 * slope)
    slope * driverLevel + constant
  }
}
