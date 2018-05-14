package com.verizon.netsense.util

import com.sensity.netsense.geolocation.TimezoneMapper
import com.verizon.netsense.enums.Weekday
import com.verizon.netsense.model.ScheduleCalendar
import com.verizon.netsense.utils.TimeConverter
import org.joda.time.format.DateTimeFormat
import org.joda.time.{DateTime, DateTimeZone}

object DateTimeUtil extends TimeConverter {

  val Today = "Today"
  val DefaultDateFormat = "yyyy-MM-dd"
  val UTCTimezone = "UTC"

  def getDayOfWeek(tz: String, date: String=Today): Weekday.Value = {
    val dt = getJodaDateTime(tz, date)
    val day = dt.getDayOfWeek
    Weekday.apply(day)
  }

  def getTimeStamp(tz: String, date: String=Today): ScheduleCalendar = {
    val dt = getJodaDateTime(tz, date)
    getTimeStamp(dt)
  }


  def getTimeStamp(dt: DateTime): ScheduleCalendar = {
    new ScheduleCalendar(hr = dt.getHourOfDay,
      min = dt.getMinuteOfHour,
      sec = dt.getSecondOfMinute,
      year = dt.year.get,
      month = dt.monthOfYear.get,
      dow = if(dt.dayOfWeek.get == 7) 0 else dt.dayOfWeek.get,
      dom = dt.dayOfMonth.get
    )
  }

  def getTimezone(lat: Double, lon: Double) : String = {
    val timezone = TimezoneMapper.latLngToTimezoneString(lat, lon)
    if(timezone == "unknown") UTCTimezone else timezone
  }

  def getJodaDateTime(tz: String, date: String=Today): DateTime = {
    val zone = DateTimeZone.forID(tz)
    date match {
      case Today => {
        new DateTime(zone)
      }
      case _ => {
        //We expect the correct date format
        val dateDecoder = DateTimeFormat.forPattern(DefaultDateFormat).withZone(zone)
        val dateTime = dateDecoder.parseDateTime(date)
        dateTime
      }
    }
  }

  def calculateTimeoutForLight(lightStartDateTime: DateTime, currentDateTime: DateTime) = {
    val diff = lightStartDateTime.getMillis - currentDateTime.getMillis
    val lightTimeoutThreshold: Int = 0
    if (diff > lightTimeoutThreshold) diff else 0L
  }

}
