package com.vz.nsp.parking

import java.time.Instant

import com.verizon.netsense.utils.Logging
import com.vz.nsp.parking.config.ServiceConfig._
import com.vz.nsp.parking.model._
import org.joda.time.IllegalFieldValueException
import org.joda.time.format._


object CommonHelper extends Logging {

  def isEmpty(x: String): Boolean = x == null || x.trim.isEmpty

  def isMoreThanOneScheduleProvided(daysOfWeek: Option[Set[String]],
                                    months: Option[Set[String]],
                                    datePeriod: Option[DatePeriod]): Boolean =
    (daysOfWeek, months, datePeriod) match {
      case (null, null, null) => true
      case (None, None, None) => true
      case (Some(day), Some(mon), Some(per)) => true
      case (Some(day), Some(mon), None) => true
      case (Some(day), Some(mon), null) => true
      case (Some(day), None, Some(per)) => true
      case (Some(day), null, Some(per)) => true
      case (None, Some(mon), Some(per)) => true
      case (null, Some(mon), Some(per)) => true
      case _ => false
    }

  def isBothNameAndTagProvided(name: Option[String], tagid: Option[String]): Boolean =
    (name, tagid) match {
      case (null, null) => true
      case (None, None) => true
      case (Some(name), Some(tagid)) => true
      case _ => false
    }


  def generateAppRequestForValidationError(msgid: String, responsetopic: String, errormsg: String): AppRequest =
    AppRequest(
      msgid,
      responsetopic,
      RequestBody(
        null,
        null,
        null,
        `type` = "validationfailed",
        model = null,
        null,
        None,
        null,
        null,
        ConfigProps(None, None, None, None, None, None, None, Some(errormsg), None, None, None, None, None, None),
        AppUserDataProps(None, null, null, None)
      )
    )

  def isValidDayOfWeek(daysOfWeek: Option[Set[String]]): Boolean =
    daysOfWeek.getOrElse(Set()).map(_.toLowerCase) subsetOf daysSet

  def isValidMonth(months: Option[Set[String]]): Boolean =
    months.getOrElse(Set()).map(_.toLowerCase) subsetOf monthsSet

  def isValidOccurs(occurs: String): Boolean =
    occursSet.contains(occurs.toLowerCase)


  def isValidReccurence(reccurence: Option[String]): Boolean =
    reccurenceSet.contains(reccurence.getOrElse("").toLowerCase())

  def isValidUnits(units: String): Boolean =
    unitsSet.contains(units.toLowerCase)

  def isValidCoarseDuration(coarseDuration: Double, sliceDuration: Double): Boolean = {
    if (coarseDuration == -1D)
      true
    else
      coarseDuration >= sliceDuration && coarseDuration % sliceDuration == 0D

  }

  def isValidViolations(violations: String): Boolean =
    violationsSet.contains(violations.toLowerCase())

  def validateDateFormat(dateAndTime: String): Boolean = try {
    val fmt = DateTimeFormat forPattern "yyyy-MM-dd;HH:mm:ss"
    Some(fmt parseDateTime dateAndTime)
    true
  } catch {
    case e: IllegalArgumentException => log.error("Error while validating dateformat " + e.getMessage); false
  }

  def startDataTimeltEndDateTime(startDateTime: String, endDateTime: String): Boolean = {
    val fmt = DateTimeFormat forPattern "yyyy-MM-dd;HH:mm:ss"
    val result = (fmt parseDateTime startDateTime) isBefore (fmt parseDateTime endDateTime)
    result
  }

  def validateTimeFormat(time: String): Boolean = try {
    val fmt = DateTimeFormat forPattern "HH:mm:ss"
    Some(fmt parseDateTime time)
    true
  } catch {
    case e@(_: IllegalArgumentException | _: IllegalFieldValueException) => log.error("Error while validating time format " + e.getMessage); false
  }

  def startTimeltEndTime(startTime: String, endTime: String): Boolean = {
    val fmt = DateTimeFormat forPattern "HH:mm:ss"
    val result = (fmt parseDateTime startTime) isBefore (fmt parseDateTime endTime)
    result
  }

  def isValidTypeOfVehicle(typeOfVehicle: Option[List[String]]): Boolean = {
    typeOfVehicle match {
      case Some(value) =>
        value.forall(typeOfVehicleSet.contains)
      case None => true
    }
  }


  def isValidformFactor(formFactor: Option[String]): Boolean = {
    formFactor match {
      case Some(value) =>
        formFactorSet contains (value.toLowerCase)
      case None => true
    }
  }


  def isValidBusinessUse(businessUse: Option[String]): Boolean = {
    businessUse match {
      case Some(value) =>
        businessUseSet.contains(value.toLowerCase)
      case None => true
    }
  }


  def isValidhowMetered(howMetered: Option[String]): Boolean =
    howMetered match {
      case Some(value) =>
        howMeteredSet.contains(value.toLowerCase)
      case None => true
    }

  def isValidAreaType(areaType: Option[List[String]]): Boolean =
    areaType match {
      case Some(value) =>
        value.map(_.toLowerCase).forall(areaTypeSet.contains)
      case None => true
    }

  def isValidGeoCords(corner: Option[SpaceGeometry]): Boolean =
    corner match {
      case Some(value) =>
        if(value.p1 == null || value.p2 == null || value.p3 == null || value.p4 == null) false
        else if (!((-90 <= value.p1.latitude && value.p1.latitude <= 90) &&
          (-180 <= value.p1.longitude && value.p1.longitude <= 180) &&
          (-90 <= value.p2.latitude && value.p2.latitude <= 90) &&
          (-180 <= value.p2.longitude && value.p2.longitude <= 180) &&
          (-90 <= value.p3.latitude && value.p3.latitude <= 90) &&
          (-180 <= value.p3.longitude && value.p4.longitude <= 180) &&
          (-90 <= value.p4.latitude && value.p4.latitude <= 90) &&
          (-180 <= value.p4.longitude && value.p4.longitude <= 180))) false
        else true
      case None => true
    }

  def convertEpochToDate(epoch: Long) = {
    Instant.ofEpochMilli(epoch).toString
  }

  def convertToSeconds(time: String) = {
    assert(validateTimeFormat(time))
    val timeArray: Array[String] = time.split(":")
    val timeInSeconds: Long = (timeArray(0).toLong * 60 * 60) + (timeArray(1).toLong * 60) + timeArray(2).toLong
    timeInSeconds
  }

  def overlappingTimeRange(tr1: TimeRange, timeRangeSet: Set[TimeRange], policyRuleName: String): List[String] = {
    val listdItem = timeRangeSet
      .foldLeft(List[String]()) { (z: List[String], f: TimeRange) =>
        z :+ (overlappingTimeRange(tr1, f) match {
          case true => "(" + tr1.startTime + " - " + tr1.endTime + ") and " + "(" + f.startTime + " - " + f.endTime + ") for rulename: " + policyRuleName
          case false => "Not-Conflicting"
        })
      }
    listdItem.filterNot(x => x == "Not-Conflicting")
  }

  def overlappingTimeRange(tr1: TimeRange, tr2: TimeRange): Boolean = {
    val tR1StartTimeInSeconds: Long = convertToSeconds(tr1.startTime)
    val tR1EndTimeInSeconds: Long = convertToSeconds(tr1.endTime)
    val tR2StartTimeInSeconds: Long = convertToSeconds(tr2.startTime)
    val tR2EndTimeInSeconds: Long = convertToSeconds(tr2.endTime)
    val overlap: Boolean = tR1StartTimeInSeconds < tR2EndTimeInSeconds && tR2StartTimeInSeconds < tR1EndTimeInSeconds
    overlap
  }

  def validateTimeRangeConflicts(timerange: Option[Set[TimeRange]], policyRuleName: Option[String]): List[String] = {
    timerange match {
      case Some(timerange) => {
        timerange.foldLeft((List[String](), timerange))((acc, tr: TimeRange) => (acc._1 ++ overlappingTimeRange(tr,
          acc._2.tail, policyRuleName.getOrElse("")), acc._2.tail)
        )._1
      }
      case None => List()
    }
  }

}

