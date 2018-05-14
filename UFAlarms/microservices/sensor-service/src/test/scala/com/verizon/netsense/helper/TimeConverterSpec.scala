package com.verizon.netsense.helper


import com.verizon.netsense.config.TestSuiteConfig
import java.time.ZoneId

import com.verizon.netsense.data.TestData
import com.verizon.netsense.util.BaseSpec
import com.verizon.netsense.utils.TimeConverter

/**
 * Created by maidapr on 6/14/17.
 */
class TimeConverterSpec extends BaseSpec with TestSuiteConfig with TestData with TimeConverter {


  "TimeConverter" should "convert the ISO timeStamp to Micros" in {
    val convertedValueInMicros = convertISOToMicros(startTimeInISO)
    val convertedIsoTimeStamp = convertMicrosToISO(convertedValueInMicros)
    convertedIsoTimeStamp mustBe startTimeInISO
  }

  it should "convert the micros to ISO" in {
    val convertISOtoMicros = convertISOToMicros(startTimeInISO)
    val convertedMicrosToISO = convertMicrosToISO(convertISOtoMicros, ZoneId.of("UTC"))
    convertedMicrosToISO mustBe startTimeInISO
  }

  it should "validate the date format" in {
    val date = convertMicrosToISO(starttimeInMicros - 10L)
    validateDateFormat(date) mustBe true
  }

  it should "validate the date1 format" in {
    val date = convertMicrosToISO(starttimeInMicros - 10L)
    validateDateFormat(date1) mustBe true
  }

  it should "validate the date2 format" in {
    val date = convertMicrosToISO(starttimeInMicros - 10L)
    validateDateFormat(date2) mustBe true
  }

  it should "negative scenario - validate the date format " in {
    val date = convertMicrosToISO(starttimeInMicros - 10L)
    validateDateFormat("djslakdqw") mustBe false
  }
}
