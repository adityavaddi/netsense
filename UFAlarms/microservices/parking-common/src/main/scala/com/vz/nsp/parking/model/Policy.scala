package com.vz.nsp.parking.model

import com.outworkers.phantom.udt.Udt
import com.vz.nsp.parking.CommonHelper._
import com.vz.nsp.parking.config.ServiceConfig._
import com.vz.nsp.parking.model.ValidationMessages._

/**
 * Created by jittara on 6/1/17.
 */
@Udt
case class Period(startDateTime: String, endDateTime: String) {
  require(!isEmpty(startDateTime), RequiredFieldMissing.value + "startDateTime")
  require(!isEmpty(endDateTime), RequiredFieldMissing.value + "endDateTime")
  require(validateDateFormat(startDateTime), DateShouldBeValidFormat.value)
  require(validateDateFormat(endDateTime), DateShouldBeValidFormat.value)
  require(startDataTimeltEndDateTime(startDateTime, endDateTime), StartDateTimeLtEndDateTime.value)

}

@Udt
case class DatePeriod(period: Period, occurs: String) {
  require(period != null, RequiredFieldMissing.value + "period")
  require(!isEmpty(occurs), RequiredFieldMissing.value + "occurs")
  require(isValidOccurs(occurs), OccursShouldbeAnyOne.value + occursConfig)
}

@Udt
case class TimeRange(startTime: String, endTime: String) {
  require(!isEmpty(startTime), RequiredFieldMissing.value + "startTime")
  require(!isEmpty(endTime), RequiredFieldMissing.value + "endTime")
  require(validateTimeFormat(startTime), TimeShouldBeValidFormat.value)
  require(validateTimeFormat(endTime), TimeShouldBeValidFormat.value)
  require(startTimeltEndTime(startTime, endTime), StartTimeLtEndTime.value)

}

@Udt
case class ChargeDuration(name: Option[String],
                          coarseDuration: Double,
                          sliceDuration: Double,
                          units: String,
                          price: Double) {
  require(coarseDuration != null, RequiredFieldMissing.value + "coarseDuration")
  require(sliceDuration != null, RequiredFieldMissing.value + "sliceDuration")
  require(isValidCoarseDuration(coarseDuration, sliceDuration), CoarseAndSlice.value)
  require(!isEmpty(units), RequiredFieldMissing.value + "units")
  require(price != null, RequiredFieldMissing.value + "price")
  require(isValidUnits(units), UnitsShouldBeAnyOne.value + unitsConfig)
}

@Udt
case class MaxDuration(duration: Double, units: String) {
  require(duration != null, RequiredFieldMissing.value + "duration")
  require(!isEmpty(units), RequiredFieldMissing.value + "units")
  require(isValidUnits(units), UnitsShouldBeAnyOne.value + unitsConfig)
}

@Udt
case class ViolationFine(violationType: String, violationFee: Double, recurrence: Option[String]) {
  require(violationType != null, RequiredFieldMissing.value + "violationType")
  require(violationFee != null, RequiredFieldMissing.value + "violationFee")
  require(isValidViolations(violationType), ViolationShouldBeAnyOne.value + violationsConfig)
  require(isValidReccurence(recurrence), RecurrenceShouldbeAnyOne.value + reccurenceConfig)
}

@Udt
case class ParkingPenalty(description: Option[String], violationFine: Set[ViolationFine])

@Udt
case class ParkingCharge(name: Option[String],
                         description: Option[String],
                         chargeDuration: Option[List[ChargeDuration]],
                         maxDuration: Option[MaxDuration],
                         maxCharge: Double)

@Udt
case class ParkingRule(description: Option[String],
                       parkingAllowed: Boolean,
                       parkingCharge: Option[ParkingCharge],
                       parkingPenalty: Option[ParkingPenalty]) {
  require(parkingAllowed != null, RequiredFieldMissing.value + "parkingAllowed")
}

@Udt
case class PolicyRule(policyRuleId: Option[String],
                      name: Option[String],
                      description: Option[String],
                      priority: Int,
                      parkingSchedule: ParkingSchedule,
                      parkingRule: ParkingRule) {
  require(priority != null, RequiredFieldMissing.value + "priority")
  require(priority > 0, "priority " + ShouldBeGreaterThanZero.value)
  require(parkingSchedule != null, RequiredFieldMissing.value + "parkingSchedule")
  require(parkingRule != null, RequiredFieldMissing.value + "parkingRule")
}

@Udt
case class ParkingSchedule(description: Option[String],
                           daysOfWeek: Option[Set[String]],
                           months: Option[Set[String]],
                           datePeriod: Option[DatePeriod],
                           timeRange: Option[Set[TimeRange]]) {
  require(!isMoreThanOneScheduleProvided(daysOfWeek, months, datePeriod), ChooseOnlyOne.value)
  require(isValidDayOfWeek(daysOfWeek), DaysShouldBeInList.value + daysConfig)
  require(isValidMonth(months), MonthsShouldBeInList.value + monthsConfig)

}

@Udt
case class PolicyLevelViolation(policyViolationId: String,
                                policyViolationName: String,
                                policyViolationDescription: Option[String],
                                policyViolationType: String,
                                policyViolationFee: Double,
                                policyViolationFeeRecurrence: Option[String]) {
  require(!isEmpty(policyViolationId), RequiredFieldMissing.value + "policyViolationId")
  require(!isEmpty(policyViolationName), RequiredFieldMissing.value + "policyViolationName")
  require(!isEmpty(policyViolationType), RequiredFieldMissing.value + "policyViolationType")
  require(isValidViolations(policyViolationType), "policy" + ViolationShouldBeAnyOne.value + violationsConfig)
  require(isValidReccurence(policyViolationFeeRecurrence),
          "policyViolationFee" + RecurrenceShouldbeAnyOne.value + reccurenceConfig)
}

@Udt
case class SuccessMessage(success: Boolean)

@Udt
case class SearchPolicy(name: String, tag: String)

@Udt
case class HistoryOfPolicies(policyStartDateTime: String, policyEndDateTime: String, createdOn: Long, version: Int)

case class Policy(uid: String,
                  policyAuthorizerid: String,
                  siteid: String,
                  orgid: String,
                  tags: Set[String],
                  policyLevelViolations: Set[PolicyLevelViolation],
                  isLibraryPolicy: Boolean,
                  hashValue: String,
                  createdOn: Long,
                  lastUpdated: Long,
                  isDeleted: Boolean,
                  version: Int,
                  name: String,
                  description: Option[String],
                  timeZone: String,
                  state: Option[String],
                  policyRule: Set[PolicyRule])

case class PolicyRequest(uid: Option[String],
                         policyAuthorizerid: Option[String],
                         tags: Option[Set[String]],
                         policyLevelViolations: Option[Set[PolicyLevelViolation]],
                         name: String,
                         timeZone: Option[String],
                         description: Option[String],
                         policyRule: Option[Set[PolicyRule]]) {
  require(!isEmpty(name), RequiredFieldMissing.value + "name")

}

case class SearchPolicyByNameOrTagid(name: Option[String], tagid: Option[String]) {
  require(!isBothNameAndTagProvided(name, tagid), ChooseOnlyOneNameOrTag.value)
}

case class SuccessMessageWithInvalidTags(success: Boolean, notAssociatedTagIds: Option[Set[String]] = None)

case class PolicyResponse(policyId: String,
                          userId: String,
                          siteId: String,
                          orgId: String,
                          tags: Set[String],
                          policyLevelViolations: Set[PolicyLevelViolation],
                          isLibraryPolicy: Boolean,
                          hashValue: String,
                          createdOn: String,
                          lastUpdated: String,
                          version: Int,
                          name: String,
                          description: Option[String],
                          timeZone: String,
                          state: Option[String],
                          policyRule: Set[PolicyRule],
                          whatIfJob: Option[List[WhatIfJobDetails]] = None,
                          associatedParkingGroups: Option[List[String]] = None)

case class WhatIfJobDetails(jobId: String, jobStatus: String)
