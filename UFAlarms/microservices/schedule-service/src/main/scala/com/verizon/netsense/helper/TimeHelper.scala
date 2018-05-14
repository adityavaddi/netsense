package com.verizon.netsense.helper

import org.joda.time.{DateTime, DateTimeZone, Duration}

import scala.collection.JavaConverters._


/**
  * Created by maidapr on 2/8/18.
  */
trait TimeHelper {

  lazy val zoneIds = DateTimeZone.getAvailableIDs.asScala.toVector

  def convertTimeStampToZone(dateTime: DateTime, zoneIdStr: String) =
    dateTime.withZoneRetainFields(DateTimeZone.forID(zoneIdStr))

  def midnightWithInAllowedTimeRange(dateTime: DateTime)(passedIntervalTime: DateTime, givenTime: DateTime) =
    dateTime.isEqual(passedIntervalTime) ||
      dateTime.isEqual(givenTime) ||
      (dateTime.isBefore(givenTime) && dateTime.isAfter(passedIntervalTime))

  def calcTimeout(startDt: DateTime): Long = (startDt.getMillis - System.currentTimeMillis()) / 1000
  
  def resolveTimeStamp(timeStamp: DateTime, intervalTime: Duration = Duration.standardMinutes(15)): Vector[String] = {

    val givenTime: DateTime = timeStamp
    val passedIntervalTime: DateTime = givenTime.minus(intervalTime)

    val startOfToday = givenTime.withTimeAtStartOfDay()
    val startOfTomorrow = startOfToday.plusDays(1)
    val startTimeOfDays = Vector(startOfToday, startOfTomorrow)

    val convertedTimeList = startTimeOfDays.par.flatMap(a => zoneIds.par.map(x => convertTimeStampToZone(a, x)))
    val filteredTimeZoneIds = convertedTimeList.filter(midnightWithInAllowedTimeRange(_)(passedIntervalTime, givenTime))
    filteredTimeZoneIds match {
      case tz if tz.nonEmpty => tz.map(_.getZone.getID).toVector
      case _ => Vector()
    }
  }

}
