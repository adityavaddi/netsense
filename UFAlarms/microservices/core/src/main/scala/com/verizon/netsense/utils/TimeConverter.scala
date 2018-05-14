package com.verizon.netsense.utils

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}
import java.util.concurrent.TimeUnit

import org.joda.time.format.DateTimeFormat

/**
 * Created by maidapr on 6/14/17.
 */
trait TimeConverter {

  implicit lazy val ISODateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")

  def convertISOToMicros(timeString: String) =
    Instant.parse(timeString).toEpochMilli * 1000

  def convertMicrosToISO(micros: Long, zoneId: ZoneId = ZoneId.of("UTC")): String =
    Instant
      .ofEpochMilli(TimeUnit.MILLISECONDS.convert(micros, TimeUnit.MICROSECONDS))
      .atZone(zoneId)
      .format(ISODateFormat)

  def validateDateFormat(date: String): Boolean =
    try {
      implicit lazy val ISODateFormat = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
      ISODateFormat.parseMillis(date)
      true
    } catch {
      case e: IllegalArgumentException => false
    }

}
