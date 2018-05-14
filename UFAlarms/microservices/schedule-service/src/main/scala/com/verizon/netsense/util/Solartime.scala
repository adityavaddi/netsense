package com.verizon.netsense.util

import com.luckycatlabs.sunrisesunset.SunriseSunsetCalculator
import com.luckycatlabs.sunrisesunset.dto.Location
import com.verizon.netsense.model.ScheduleCalendar
import com.verizon.netsense.utils.Logging
import org.joda.time.{DateTime, DateTimeZone}
import org.joda.time.format.{ISODateTimeFormat}

object Solartime extends Logging{

  def resolveExpressionToUTC(timeExpr: String,
                             lat: Double,
                             lon: Double,
                             tz: String,
                             localDate: String="Today"): ScheduleCalendar = {
    val expr = timeExpr.toLowerCase()
    val loc = new Location(lat,lon)
    val calculator = new SunriseSunsetCalculator(loc, tz)
    val date = DateTimeUtil.getJodaDateTime(tz, localDate)

    val cal = date.toGregorianCalendar
    expr match {
      case expr if expr.startsWith("sunset") => {
        val sunset = calculator.getOfficialSunsetCalendarForDate(cal)
        //Calendar returns the current time as UTC milliseconds from the epoch.
        val dtWoutOffset = new DateTime(sunset.getTimeInMillis, DateTimeZone.UTC)
        val dt = apllySunriseSunsetOffset(expr, dtWoutOffset)
        DateTimeUtil.getTimeStamp(dt)
      }
      case expr if expr.startsWith("sunrise") => {
        val sunrise = calculator.getOfficialSunriseCalendarForDate(cal)
        val dtWoutOffset = new DateTime(sunrise.getTimeInMillis, DateTimeZone.UTC)
        val dt = apllySunriseSunsetOffset(expr, dtWoutOffset)
        DateTimeUtil.getTimeStamp(dt)
      }
      case expr if expr.split(":").size == 3  => {
        val dt = date.withTime(ISODateTimeFormat.hourMinuteSecond().parseLocalTime(expr))
        val utcTime = dt.withZone(DateTimeZone.UTC)
        log.debug(s"Converting expr $expr to LocalDate $dt == $utcTime in UTC")
        DateTimeUtil.getTimeStamp(utcTime)
      }
      case _ => throw new IllegalArgumentException("Time expression for the schedule is not in correct format" + timeExpr)
    }
  }

  private def apllySunriseSunsetOffset(expr: String, dt: DateTime) = expr match {
      case expr if expr.contains('+') => {
        val splitExpr = expr.split('+')
        val offset = if(splitExpr.size > 0) splitExpr(1).toInt else 0
        dt.plusMinutes(offset)
      }
      case expr if expr.contains('-') => {
        val splitExpr = expr.split('-')
        val offset = if(splitExpr.size > 0) splitExpr(1).toInt else 0
        dt.minusMinutes(offset)
      }
      case _ => {
        dt
      }
    }

}
