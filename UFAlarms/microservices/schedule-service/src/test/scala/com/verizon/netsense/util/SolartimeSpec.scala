package com.verizon.netsense.services.util

import com.verizon.netsense.helper.BaseSpec
import com.verizon.netsense.model.ScheduleCalendar
import com.verizon.netsense.util.{DateTimeUtil, Solartime}
import com.verizon.netsense.utils.Logging
import org.joda.time.{DateTime, DateTimeZone}

class SolartimeSpec extends BaseSpec with Logging {

  "Solar Transformer" should "transform sunset to UTC format" in {
    val utcTime = Solartime.resolveExpressionToUTC("sunset", 37.380996, -121.992299, "America/Los_Angeles", "2018-01-23")
    assert(utcTime.hr == 1)
    assert(utcTime.min == 22)
    assert(utcTime.sec == 0)
  }

  it should "transform sunset + offset to UTC format" in {
    val utcTime = Solartime.resolveExpressionToUTC("sunset+30", 37.380996, -121.992299, "America/Los_Angeles", "2018-01-23")
    assert(utcTime.hr == 1)
    assert(utcTime.min == 52)
    assert(utcTime.sec == 0)
  }

  /**
    * Parity checks with Datadealer [test/utils/test/solartime.clj] for test-local-sunrise+
    *
    * resolved time from DD:
    * utils.test.solartime - sunrise-plus-expr-map
    * {:timestamp #object[org.joda.time.DateTime 0x21f459fc "2015-12-22T15:49:00.000Z"], :level 50, :qualifiers 0}
    *
    * */
  it should "transform sunrise+30 offset to UTC format for PST" in {
    val latitude = 37.380996
    val longitude = -121.992299
    val timeZone = DateTimeUtil.getTimezone(latitude, longitude)
    val utcTime = Solartime.resolveExpressionToUTC("sunrise+30", latitude, longitude, timeZone, "2015-12-22")
    assert(utcTime == ScheduleCalendar(15, 49, 0, 2015, 12, 2, 22))
  }

  /**
    * Parity checks with Datadealer [test/utils/test/solartime.clj] for test-local-sunrise-30
    *
    * resolved time from DD:
    * utils.test.solartime - sunset-minus-expr-map
    * {:timestamp #object[org.joda.time.DateTime 0xbfc14b9 "2015-12-23T00:24:00.000Z"], :level 50, :qualifiers 0}
    *
    * */
  it should "transform sunset-30 offset to UTC format for PST" in {
    val latitude = 37.380996
    val longitude = -121.992299
    val timeZone = DateTimeUtil.getTimezone(latitude, longitude)
    val utcTime = Solartime.resolveExpressionToUTC("sunset-30", latitude, longitude, timeZone, "2015-12-22")
    assert(utcTime == ScheduleCalendar(0, 24, 0, 2015, 12, 3, 23))
  }

  it should "transform sunrise to UTC format" in {
    val utcTime = Solartime.resolveExpressionToUTC("sunrise", 37.380996, -121.992299, "America/Los_Angeles", "2018-01-23")
    assert(utcTime.hr == 15)
    assert(utcTime.min == 18)
    assert(utcTime.sec == 0)
  }

  it should "transform sunrise + offset to UTC format" in {
    val utcTime = Solartime.resolveExpressionToUTC("sunrise+30", 37.380996, -121.992299, "America/Los_Angeles", "2018-01-23")
    assert(utcTime.hr == 15)
    assert(utcTime.min == 48)
    assert(utcTime.sec == 0)
  }

  it should "transform to UTC with behind timezone" in {
    val utcTime = Solartime.resolveExpressionToUTC("07:00:00", 37.380996, -121.992299, "America/Los_Angeles", "2018-01-23")
    //LA is 8 hrs behind UTC
    assert(utcTime.hr == 15)
    assert(utcTime.min == 0)
    assert(utcTime.sec == 0)
  }

  it should "transform to UTC with ahead timezone" in {
    val utcTime = Solartime.resolveExpressionToUTC("07:00:00", 28.644800, 77.216721, "Asia/Kolkata", "2018-01-23")
    //India is 5:30 hrs ahead of UTC
    assert(utcTime.hr == 1)
    assert(utcTime.min == 30)
    assert(utcTime.sec == 0)
  }

  it should "return different sunset times  for LA and Dallas" in {
    val laSunsetTime = Solartime.resolveExpressionToUTC("sunset", 37.380996, -121.992299, "America/Los_Angeles", "2018-01-23")
    val dalSunsetTime = Solartime.resolveExpressionToUTC("sunset", 32.89, -97.04, "America/Chicago", "2018-01-23")
    val delhiSunsetTime = Solartime.resolveExpressionToUTC("sunset", 28.644800, 77.216721, "Asia/Kolkata", "2018-01-23")

    assert(laSunsetTime != dalSunsetTime)
    assert(delhiSunsetTime != dalSunsetTime)
  }

  it should "loose hour when DST is applied in LA" in {
    val californiaTimeZone = DateTimeZone.forID("America/Los_Angeles")
    val now = new DateTime(californiaTimeZone)

    //val dstday = new DateMidnight(2018, 3, 24)
    val previousTransition = new DateTime(californiaTimeZone.previousTransition(now.getMillis))
    val nextTransition = new DateTime(californiaTimeZone.nextTransition(now.getMillis))
    val next2Transition = new DateTime(californiaTimeZone.nextTransition(nextTransition.getMillis))

    var time = Solartime.resolveExpressionToUTC("00:00:00", 37.380996, -121.992299, "America/Los_Angeles", "2018-03-11")
    assert(time == ScheduleCalendar(8, 0, 0, 2018, 3, 0, 11))
    time = Solartime.resolveExpressionToUTC("01:00:00", 37.380996, -121.992299, "America/Los_Angeles", "2018-03-11")
    assert(time == ScheduleCalendar(9, 0, 0, 2018, 3, 0, 11))
    time = Solartime.resolveExpressionToUTC("02:00:00", 37.380996, -121.992299, "America/Los_Angeles", "2018-03-11")
    assert(time == ScheduleCalendar(10, 0, 0, 2018, 3, 0, 11))
    time = Solartime.resolveExpressionToUTC("03:00:00", 37.380996, -121.992299, "America/Los_Angeles", "2018-03-11")
    assert(time == ScheduleCalendar(10, 0, 0, 2018, 3, 0, 11))
    time = Solartime.resolveExpressionToUTC("04:30:00", 37.380996, -121.992299, "America/Los_Angeles", "2018-03-11")
    assert(time == ScheduleCalendar(11, 30, 0, 2018, 3, 0, 11))
    time = Solartime.resolveExpressionToUTC("23:00:00", 37.380996, -121.992299, "America/Los_Angeles", "2018-03-11")
    assert(time == ScheduleCalendar(6, 0, 0, 2018, 3, 1, 12))
    time = Solartime.resolveExpressionToUTC("23:30:00", 37.380996, -121.992299, "America/Los_Angeles", "2018-03-11")
    assert(time == ScheduleCalendar(6, 30, 0, 2018, 3, 1, 12))
    time = Solartime.resolveExpressionToUTC("00:00:00", 37.380996, -121.992299, "America/Los_Angeles", "2018-03-12")
    assert(time == ScheduleCalendar(7, 0, 0, 2018, 3, 1, 12))
    time = Solartime.resolveExpressionToUTC("01:00:00", 37.380996, -121.992299, "America/Los_Angeles", "2018-03-12")
    assert(time == ScheduleCalendar(8, 0, 0, 2018, 3, 1, 12))
  }

  it should "loose hour when DST is applied in Denver" in {
    var time = Solartime.resolveExpressionToUTC("00:00:00", 39.76185, -104.881105, "America/Denver", "2018-03-11")
    assert(time == ScheduleCalendar(7, 0, 0, 2018, 3, 0, 11))
    time = Solartime.resolveExpressionToUTC("01:00:00", 39.76185, -104.881105, "America/Denver", "2018-03-11")
    assert(time == ScheduleCalendar(8, 0, 0, 2018, 3, 0, 11))
    time = Solartime.resolveExpressionToUTC("02:00:00", 39.76185, -104.881105, "America/Denver", "2018-03-11")
    assert(time == ScheduleCalendar(9, 0, 0, 2018, 3, 0, 11))
    time = Solartime.resolveExpressionToUTC("03:00:00", 39.76185, -104.881105, "America/Denver", "2018-03-11")
    assert(time == ScheduleCalendar(9, 0, 0, 2018, 3, 0, 11))
    time = Solartime.resolveExpressionToUTC("04:00:00", 39.76185, -104.881105, "America/Denver", "2018-03-11")
    assert(time == ScheduleCalendar(10, 0, 0, 2018, 3, 0, 11))
    time = Solartime.resolveExpressionToUTC("00:00:00", 39.76185, -104.881105, "America/Denver", "2018-03-12")
    assert(time == ScheduleCalendar(6, 0, 0, 2018, 3, 1, 12))
  }

  it should "not loose hour when DST is applied in Arizona(DST is not applied in Arizona)" in {
    var time = Solartime.resolveExpressionToUTC("00:00:00", 33.45, -112.066667, "America/Phoenix", "2018-03-11")
    assert(time == ScheduleCalendar(7, 0, 0, 2018, 3, 0, 11))
    time = Solartime.resolveExpressionToUTC("01:00:00", 33.45, -112.066667, "America/Phoenix", "2018-03-11")
    assert(time == ScheduleCalendar(8, 0, 0, 2018, 3, 0, 11))
    time = Solartime.resolveExpressionToUTC("02:00:00", 33.45, -112.066667, "America/Phoenix", "2018-03-11")
    assert(time == ScheduleCalendar(9, 0, 0, 2018, 3, 0, 11))
    time = Solartime.resolveExpressionToUTC("03:00:00", 33.45, -112.066667, "America/Phoenix", "2018-03-11")
    assert(time == ScheduleCalendar(10, 0, 0, 2018, 3, 0, 11))
    time = Solartime.resolveExpressionToUTC("04:30:00", 33.45, -112.066667, "America/Phoenix", "2018-03-11")
    assert(time == ScheduleCalendar(11, 30, 0, 2018, 3, 0, 11))
    time = Solartime.resolveExpressionToUTC("23:00:00", 33.45, -112.066667, "America/Phoenix", "2018-03-11")
    assert(time == ScheduleCalendar(6, 0, 0, 2018, 3, 1, 12))
    time = Solartime.resolveExpressionToUTC("23:30:00", 33.45, -112.066667, "America/Phoenix", "2018-03-11")
    assert(time == ScheduleCalendar(6, 30, 0, 2018, 3, 1, 12))
    time = Solartime.resolveExpressionToUTC("00:00:00", 33.45, -112.066667, "America/Phoenix", "2018-03-12")
    assert(time == ScheduleCalendar(7, 0, 0, 2018, 3, 1, 12))
    time = Solartime.resolveExpressionToUTC("01:00:00", 33.45, -112.066667, "America/Phoenix", "2018-03-12")
    assert(time == ScheduleCalendar(8, 0, 0, 2018, 3, 1, 12))
  }

  it should "gain hour when DST ends in LA" in {
    var time = Solartime.resolveExpressionToUTC("00:00:00", 37.380996, -121.992299, "America/Los_Angeles", "2018-11-04")
    assert(time == ScheduleCalendar(7, 0, 0, 2018, 11, 0, 4))
    time = Solartime.resolveExpressionToUTC("01:00:00", 37.380996, -121.992299, "America/Los_Angeles", "2018-11-04")
    assert(time == ScheduleCalendar(8, 0, 0, 2018, 11, 0, 4))
    time = Solartime.resolveExpressionToUTC("02:00:00", 37.380996, -121.992299, "America/Los_Angeles", "2018-11-04")
    assert(time == ScheduleCalendar(10, 0, 0, 2018, 11, 0, 4))
    time = Solartime.resolveExpressionToUTC("03:00:00", 37.380996, -121.992299, "America/Los_Angeles", "2018-11-04")
    assert(time == ScheduleCalendar(11, 0, 0, 2018, 11, 0, 4))
  }

  it should "gain hour when DST ends in Denver" in {
    var time = Solartime.resolveExpressionToUTC("00:00:00", 39.76185, -104.881105, "America/Denver", "2018-11-04")
    assert(time == ScheduleCalendar(6, 0, 0, 2018, 11, 0, 4))
    time = Solartime.resolveExpressionToUTC("01:00:00", 39.76185, -104.881105, "America/Denver", "2018-11-04")
    assert(time == ScheduleCalendar(7, 0, 0, 2018, 11, 0, 4))
    time = Solartime.resolveExpressionToUTC("02:00:00", 39.76185, -104.881105, "America/Denver", "2018-11-04")
    assert(time == ScheduleCalendar(9, 0, 0, 2018, 11, 0, 4))
    time = Solartime.resolveExpressionToUTC("03:00:00", 39.76185, -104.881105, "America/Denver", "2018-11-04")
    assert(time == ScheduleCalendar(10, 0, 0, 2018, 11, 0, 4))
  }

  it should "not gain hour when DST ends in Arizona(DST is not applied in Arizona)" in {
    var time = Solartime.resolveExpressionToUTC("00:00:00", 33.45, -112.066667, "America/Phoenix", "2018-11-04")
    assert(time == ScheduleCalendar(7, 0, 0, 2018, 11, 0, 4))
    time = Solartime.resolveExpressionToUTC("01:00:00", 33.45, -112.066667, "America/Phoenix", "2018-11-04")
    assert(time == ScheduleCalendar(8, 0, 0, 2018, 11, 0, 4))
    time = Solartime.resolveExpressionToUTC("02:00:00", 33.45, -112.066667, "America/Phoenix", "2018-11-04")
    assert(time == ScheduleCalendar(9, 0, 0, 2018, 11, 0, 4))
    time = Solartime.resolveExpressionToUTC("03:00:00", 33.45, -112.066667, "America/Phoenix", "2018-11-04")
    assert(time == ScheduleCalendar(10, 0, 0, 2018, 11, 0, 4))
  }

}
