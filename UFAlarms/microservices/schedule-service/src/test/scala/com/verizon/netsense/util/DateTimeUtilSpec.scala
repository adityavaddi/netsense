package com.verizon.netsense.util

import com.verizon.netsense.enums.Weekday
import com.verizon.netsense.helper.BaseSpec
import org.joda.time.{DateTime, Period}

/**
  * Created by maidapr on 1/25/18.
  */
class DateTimeUtilSpec extends BaseSpec {

  "DateTime Diff calculator" should "have +ve time difference in millis" in {
    val timeoutInDb = DateTime.now()
    val delay: Long = 50
    Thread.sleep(delay)
    val currentTimeNow = DateTime.now()
    val timeDiff: Long = DateTimeUtil.calculateTimeoutForLight(currentTimeNow, timeoutInDb)
    timeDiff mustBe > (delay - 5)
  }

  it should "have 0 time difference if threshold is met" in {
    val delay: Long = 50
    Thread.sleep(delay)
    val date2 = DateTime.now()
    Thread.sleep(2)
    val date1 = DateTime.now()
    val timeDiff: Long = DateTimeUtil.calculateTimeoutForLight(date1, date2)
    assert(timeDiff > 0L)
  }

  "getTimezone" should "return correct for the given lat and long" in {
    val latz = DateTimeUtil.getTimezone(37.380996, -121.992299)
    assert(latz == "America/Los_Angeles")

    val itz = DateTimeUtil.getTimezone(28.644800, 77.216721)
    assert(itz == "Asia/Kolkata")

    val datz = DateTimeUtil.getTimezone(32.89, -97.04)
    assert(datz == "America/Chicago")
  }

  "getDayOfWeek" should "return the day of week for the given day in given timezone" in {
    val dow = DateTimeUtil.getDayOfWeek("America/Los_Angeles", "2018-01-24")
    assert(dow == Weekday.wed)
  }

  "getJodaDateTime" should "return different local time for different timezone" in {
    val dateLA = DateTimeUtil.getJodaDateTime("America/Los_Angeles")
    val dateC = DateTimeUtil.getJodaDateTime("America/Chicago")

    val period = new Period( dateLA , dateC )
    assert(period.getHours == 0)

    val civilTimeDiff = new Period(dateLA.toLocalDateTime , dateC.toLocalDateTime)
    assert(civilTimeDiff.toStandardHours.getHours == 2)
  }

  it should "return different time for different dates" in {
    val date1 = DateTimeUtil.getJodaDateTime("America/Los_Angeles", "2018-01-18")
    val date2 = DateTimeUtil.getJodaDateTime("America/Los_Angeles", "2018-01-19")

    val period = new Period( date1 , date2 )
    assert(period.getDays == 1)
  }

  "getTimeStamp" should "return proper schedule calendar for a given date" in {
      val date1 = DateTimeUtil.getJodaDateTime("America/Los_Angeles", "2018-01-7")
      val schedCal = DateTimeUtil.getTimeStamp(date1)

      assert(schedCal.year == 2018)
      assert(schedCal.month == 1)
      assert(schedCal.dow == 0)
      assert(schedCal.dom == 7)

  }

}
