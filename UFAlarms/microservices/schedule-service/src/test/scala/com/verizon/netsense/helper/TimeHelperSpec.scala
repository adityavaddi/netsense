package com.verizon.netsense.helper

import org.joda.time.{DateTime, Duration}

/**
  * Created by maidapr on 2/13/18.
  */
class TimeHelperSpec extends BaseSpec with TimeHelper {

  "TimeHelper" should "get the midnight timezones for the given timestamp" in {
    val testTime = DateTime.parse("2018-02-13T01:10:29.285-05:00")
    val timeZoneIds = time(resolveTimeStamp(testTime, Duration.millis(899998)))
    assert(timeZoneIds.nonEmpty)
  }

  it should "return empty Vector for timestamp without midnight zones" in {
    val testTime = DateTime.parse("2018-02-13T01:17:29.285-05:00")
    val timeZoneIds = time(resolveTimeStamp(testTime, Duration.millis(899998)))
    assert(timeZoneIds.isEmpty)
  }

  it should "return expected time zone for the given Time stamp as it has midnight time" in {
    val testTime = DateTime.parse("2018-02-13T01:10:29.285-05:00")
    val timeZoneIds = time(resolveTimeStamp(testTime, Duration.millis(899998)))
    assert(timeZoneIds.size.equals(34))
    assert(timeZoneIds.contains("America/Chicago"))
    assert(!timeZoneIds.contains("America/New_York"))
  }


  it should "return midnight timezone for absolute midnight " in {
    val testTime = DateTime.parse("2018-02-13T00:00:00.000-00:00")
    val timeZoneIds = time(resolveTimeStamp(testTime, Duration.millis(899998)))
    assert(timeZoneIds.nonEmpty)
    assert(timeZoneIds.contains("UTC"))
  }

  it should "return midnight timezone for beginning of the day for last 15 minutes " in {
    val testTime = DateTime.parse("2018-02-13T00:10:00.000-00:00")
    val timeZoneIds = time(resolveTimeStamp(testTime, Duration.millis(899998)))
    assert(timeZoneIds.nonEmpty)
    assert(timeZoneIds.contains("UTC"))
  }

  it should "return midnight timezone for beginning of the day at PST time " in {
    val testTime = DateTime.parse("2018-03-11T08:00:00.000Z")
    val timeZoneIds = time(resolveTimeStamp(testTime, Duration.millis(899998)))
    assert(timeZoneIds.nonEmpty)
    assert(timeZoneIds.contains("America/Los_Angeles"))
  }
  it should "return midnight timezone for beginning of the day at PDT time " in {
    val testTime = DateTime.parse("2018-03-12T07:00:00.000Z")
    val timeZoneIds = time(resolveTimeStamp(testTime, Duration.millis(899998)))
    assert(timeZoneIds.nonEmpty)
    assert(timeZoneIds.contains("America/Los_Angeles"))
  }

  it should "return midnight timezone with UTC if beginning of the day is with +1 millis" in {
    val testTime = DateTime.parse("2018-02-13T00:00:00.001Z")
    val timeZoneIds = time(resolveTimeStamp(testTime, Duration.millis(899998)))
    assert(timeZoneIds.nonEmpty)
    assert(timeZoneIds.contains("UTC"))
  }

  it should "not return midnight timezone with UTC if beginning of the day for after 15 minutes of startOfDay" in {
    val testTime = DateTime.parse("2018-02-13T00:15:00.000Z")
    val timeZoneIds = time(resolveTimeStamp(testTime, Duration.millis(899998)))
    assert(!timeZoneIds.contains("UTC"))
  }

  it should "return midnight timezone for a different past time " in {
    val testTime = DateTime.parse("2018-02-13T00:18:29.285-05:00")
    val timeZoneIds = time(resolveTimeStamp(testTime, Duration.standardMinutes(30)))
    assert(timeZoneIds.nonEmpty)
    assert(timeZoneIds.contains("America/New_York"))
  }

  it should "check if the given Date is within the range of past considered time " in {
    val timeNow                   = DateTime.now()
    val passTimeStamp             = timeNow.minus(Duration.standardMinutes(20))
    val passTimeStampToPass       = timeNow.minus(Duration.standardMinutes(15))
    val testTimeToFailOutOfRange  = timeNow.plus(Duration.standardMinutes(20))
    val isNotInRange: Boolean     = midnightWithInAllowedTimeRange(testTimeToFailOutOfRange)(passTimeStamp, timeNow)
    val isInRange: Boolean        = midnightWithInAllowedTimeRange(passTimeStampToPass)(passTimeStamp, timeNow)
    assert(!isNotInRange)
    assert(isInRange)
  }

  it should "check if the given Date is within the range of intact time " in {
    val timeNow       = DateTime.now()
    val passTimeStamp = timeNow.minus(Duration.standardMinutes(20))
    val timeNowIntact: Boolean = midnightWithInAllowedTimeRange(timeNow)(passTimeStamp, timeNow)
    val passedTimeIntact: Boolean = midnightWithInAllowedTimeRange(passTimeStamp)(passTimeStamp, timeNow)
    assert(timeNowIntact)
    assert(passedTimeIntact)
  }

  it should "calculate the timeout from startDt" in {

    val millis: Long = System.currentTimeMillis()
    def calcTimeout(startDt: DateTime): Long = {
      startDt.getMillis - millis
    }
    val startDt = DateTime.now()
    val timeout: Long = calcTimeout(startDt)
    assert(timeout >= 0)
  }


}
