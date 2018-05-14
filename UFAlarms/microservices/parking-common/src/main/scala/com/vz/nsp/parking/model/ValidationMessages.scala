package com.vz.nsp.parking.model

/**
 * Created by donthgo on 8/1/17.
 */
sealed case class ValidationMessages(value: String)

object ValidationMessages {

  object RequiredFieldMissing    extends ValidationMessages("missing field ")
  object ShouldBeGreaterThanZero extends ValidationMessages("should be greater than 0")
  object ChooseOnlyOne           extends ValidationMessages("Choose only one among daysOfWeek, months, datePeriod")
  object ChooseOnlyOneNameOrTag  extends ValidationMessages("Choose only one among name, tagid")
  object DaysShouldBeInList      extends ValidationMessages("days should be anyone of these ")
  object MonthsShouldBeInList    extends ValidationMessages("months should be anyone of these ")
  object OccursShouldbeAnyOne    extends ValidationMessages("occurs should be anyone of these ")
  object RecurrenceShouldbeAnyOne    extends ValidationMessages("recurrence should be anyone of these ")
  object DateShouldBeValidFormat     extends ValidationMessages("startDateTime and endDateTime should be in this format: yyyy-MM-dd;HH:mm:ss")
  object TimeShouldBeValidFormat     extends ValidationMessages("startTime and endTime should be in this format: HH:mm:ss and HH should be in range [0,23], mm and ss should be in range [0,59] ")
  object UnitsShouldBeAnyOne         extends ValidationMessages("units should be anyone of these ")
  object ViolationShouldBeAnyOne     extends ValidationMessages("violationType should be anyone of these ")
  object LatitudeShouldBeInBetween   extends ValidationMessages("latitude should be in between -90 to 90 ")
  object LongitudeShouldBeInBetween  extends ValidationMessages("longitude should be in between -180 to 180 ")
  object TypeOfVehicleShouldBeInList extends ValidationMessages("typeOfVehicle should be anyone of these ")
  object FormFactorShouldBeInList    extends ValidationMessages("formFactor should be anyone of these ")
  object BusinessUseShouldBeInList   extends ValidationMessages("businessUse should be anyone of these ")
  object HowMeteredShouldbeInList    extends ValidationMessages("howMetered should be anyone of these ")
  object ActiveShouldbeInList        extends ValidationMessages("active should be anyone of these ")
  object AreaTypeShouldbeInList      extends ValidationMessages("areaType should be anyone of these ")
  object CoarseAndSlice              extends ValidationMessages("coarseDuration should be greater than or equal to sliceDuration and coarseDuration should be multiple of sliceDuration ")
  object StartTimeLtEndTime      extends ValidationMessages("endTime should be greater than startTime")
  object StartDateTimeLtEndDateTime      extends ValidationMessages("endDateTime should be greater than startDateTime ")
}
