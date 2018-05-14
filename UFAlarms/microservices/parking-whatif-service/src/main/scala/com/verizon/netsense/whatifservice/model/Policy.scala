package com.verizon.netsense.whatifservice.model

import com.outworkers.phantom.udt.Udt

@Udt
case class ParkingPenalty(description: Option[String], violationFine: Set[ViolationFine])

@Udt
case class ViolationFine(violationType: String, violationFee: Double, recurrence: Option[String])

@Udt
case class MaxDuration(duration: Double, units: String)

@Udt
case class ChargeDuration(name: Option[String],coarseDuration: Double, sliceDuration: Double, units: String, price: Double)

@Udt
case class Period(startDateTime: String, endDateTime: String)


@Udt
case class DatePeriod(period: Period, occurs: String)

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
                       parkingPenalty: Option[ParkingPenalty])

@Udt
case class TimeRange(startTime: String, endTime: String)

@Udt
case class ParkingSchedule(description: Option[String],
                           daysOfWeek: Option[Set[String]],
                           months: Option[Set[String]],
                           datePeriod: Option[DatePeriod],
                           timeRange: Option[Set[TimeRange]])


@Udt
case class PolicyRule(policyRuleId: Option[String],
                      name: Option[String],
                      description: Option[String],
                      priority: Int,
                      parkingSchedule: ParkingSchedule,
                      parkingRule: ParkingRule)

@Udt
case class PolicyLevelViolation(policyViolationId: String,
                                policyViolationName: String,
                                policyViolationDescription: Option[String],
                                policyViolationType: String,
                                policyViolationFee: Double,
                                policyViolationFeeRecurrence: Option[String])

case class Policy(userid: String,
                  policyid: String,
                  siteid: String,
                  orgid: String,
                  tags: Set[String],
                  policylevelviolations: Set[PolicyLevelViolation],
                  islibrarypolicy: Boolean,
                  hashvalue: String,
                  createdon: Long,
                  lastupdated: Long,
                  isdeleted: Boolean,
                  version: Int,
                  name: String,
                  description: Option[String],
                  timezone: String,
                  state: Option[String],
                  policyrule: Set[PolicyRule])





