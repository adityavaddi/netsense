package com.vz.nsp.trigger.service

import com.vz.nsp.trigger.model.app.{TriggerRequest, TriggerResource}
import com.vz.nsp.trigger.util.BaseSpec

class TriggerValidatorSpec extends BaseSpec {

  val triggerRequest = TriggerRequest(
    triggerCategory = "Business Alert - Parking",
    triggerSubcategory = "group",
    timePeriod = "15min",
    triggerVariable = "Occupancy",
    comparisonOperator = "GT",
    comparisonValue = "60",
    severity = "Major"
  )

  val triggerRequestNegative = TriggerRequest(comparisonVariable = Some(""), comparisonVariableOperator = Some(""))

  val triggerRequestIncludingComparison = TriggerRequest(
    triggerCategory = "Business Alert - Parking",
    triggerSubcategory = "group",
    timePeriod = "15min",
    triggerVariable = "TotalViolationCount",
    comparisonOperator = "GT",
    severity = "Major",
    comparisonValue = "60",
    comparisonVariable = Some("Turnover"),
    comparisonVariableOperator = Some("Percentage")
  )

  "TriggerValidatorSpec" should "validate trigger - Positive" in {
    val result = TriggerValidator.validateTrigger(triggerRequest)
    assert(result.equals(""))
  }

  it should "validate trigger - Negative" in {
    val result = TriggerValidator.validateTrigger(TriggerRequest())
    assert(result.split(",").length == 6)
  }

  it should "validate triggerCategory - Negative" in {
    val result = TriggerValidator.validateTrigger(triggerRequest.copy(triggerCategory = "Invalid"))
    assert(result.equals("Validation failed for following trigger variables triggerCategory"))
  }

  it should "validate trigger including comparison variable and operator - Negative" in {
    val result = TriggerValidator.validateTrigger(triggerRequestNegative)
    assert(result.split(";").length == 3)
    assert(result.split(";")(0).split(",").length == 6)
  }

  it should "validate trigger with resource - Positive" in {
    val result = TriggerValidator.validateTrigger(
      triggerRequest.copy(resourceList = List(TriggerResource("resourceId", "resourceName")))
    )
    assert(result.equals(""))
  }

  it should "validate trigger with empty resourceName - Negative" in {
    val result =
      TriggerValidator.validateTrigger(triggerRequest.copy(resourceList = List(TriggerResource("resourceId", ""))))
    assert(result.equals("Trigger resource id/name should not be empty"))
  }

  it should "validate trigger with empty resourceId - Negative" in {
    val result =
      TriggerValidator.validateTrigger(triggerRequest.copy(resourceList = List(TriggerResource("", "resourceName"))))
    assert(result.equals("Trigger resource id/name should not be empty"))
  }

  it should "validate trigger with empty resourceId and resourceName - Negative" in {
    val result =
      TriggerValidator.validateTrigger(triggerRequest.copy(resourceList = List(TriggerResource("", ""))))
    assert(result.equals("Trigger resource id/name should not be empty"))
  }

  it should "validate trigger with  multiple resources - Positive" in {
    val result =
      TriggerValidator.validateTrigger(
        triggerRequest
          .copy(
            resourceList =
              List(TriggerResource("resourceId", "resourceName"), TriggerResource("resourceId2", "resourceName2"))
          )
      )
    assert(result.equals(""))
  }

  it should "validate trigger with empty resourceName(multiple) - Negative" in {
    val result =
      TriggerValidator.validateTrigger(
        triggerRequest
          .copy(resourceList = List(TriggerResource("resourceId", "resourceName"), TriggerResource("resourceId", "")))
      )
    assert(result.equals("Trigger resource id/name should not be empty"))
  }

  it should "validate trigger with empty resourceId and invalid triggerVariable" in {
    val result =
      TriggerValidator.validateTrigger(
        triggerRequest.copy(resourceList = List(TriggerResource("", "resourceName")), triggerVariable = "invalid")
      )
    assert(result.split(";").length == 2)
    assert(result.split(";")(0).equals("Validation failed for following trigger variables triggerVariable"))
    assert(result.split(";")(1).equals("Trigger resource id/name should not be empty"))
  }

  it should "validate trigger with empty resourceId and invalid triggerVariable and invalid comparisonVariable" +
    " - E2E Negative" in {
    val result =
      TriggerValidator.validateTrigger(
        triggerRequest.copy(resourceList = List(TriggerResource("", "resourceName")), triggerVariable = "invalid",
          comparisonVariable = Some("Turnover"))
      )
    val resultSplit =  result.split(";")
    assert(resultSplit.length == 3)
    assert(resultSplit(0).equals("Validation failed for following trigger variables triggerVariable"))
    assert(resultSplit(1).equals("Invalid comparisonVariable/comparisonVariableOperator/triggerVariable"))
    assert(resultSplit(2).equals("Trigger resource id/name should not be empty"))
  }

  it should "validate trigger including comparison variable and operator - Positive" in {
    val result = TriggerValidator.validateTrigger(triggerRequestIncludingComparison)
    assert(result.equals(""))
  }

  it should "validate both comparisonVariable and comparisonVariableOperator are present" in {
    val result = TriggerValidator.validateTrigger(triggerRequestIncludingComparison.copy(
      triggerVariable = "Occupancy",
      comparisonVariable = Some("Turnover"),
      comparisonVariableOperator = Some("Percentage")
    ))
    assert(result.equals("Invalid comparisonVariable/comparisonVariableOperator/triggerVariable"))

    val result1 = TriggerValidator.validateTrigger(triggerRequestIncludingComparison.copy(comparisonVariable = None))
    assert(result1.equals("Invalid comparisonVariable/comparisonVariableOperator/triggerVariable"))

    val result2 = TriggerValidator.validateTrigger(triggerRequestIncludingComparison.copy(comparisonVariableOperator = None))
    assert(result2.equals("Invalid comparisonVariable/comparisonVariableOperator/triggerVariable"))

    val result3 = TriggerValidator.validateTrigger(triggerRequestIncludingComparison.copy(
      comparisonVariableOperator = None,
      comparisonVariable = None,
      triggerVariable = "Occupancy")
    )
    assert(result3.equals(""))

    val result4 = TriggerValidator.validateTrigger(triggerRequestIncludingComparison.copy(
      comparisonVariable = Some("Occupancy"))
    )
    assert(result4.equals("Invalid comparisonVariable/comparisonVariableOperator/triggerVariable"))

    val result5 = TriggerValidator.validateTrigger(triggerRequestIncludingComparison.copy(
      comparisonVariableOperator = Some("NotPercentage"))
    )
    assert(result5.equals("Invalid comparisonVariable/comparisonVariableOperator/triggerVariable;ComparisonValue not in the valid range or format"))

  }

  it should "validate the comparison values are in the desired range when value is null" in {
    val errorMsg = "ComparisonValue not in the valid range or format"

    var result = TriggerValidator.validateTrigger(triggerRequest.copy(
      triggerVariable = "TotalRevenue",
      comparisonValue = null
    ))
    assert(result.equals(errorMsg))
  }

  it should "Throw error when comparison values is empty" in {
    val errorMsg = "ComparisonValue not in the valid range or format"

    var result = TriggerValidator.validateTrigger(triggerRequest.copy(
      triggerVariable = "Occupancy",
      comparisonValue = ""
    ))
    assert(result.equals(errorMsg))
  }

  it should "validate the comparison values are in the desired range when triggerVariable is Occupancy " in {
    val errorMsg = "ComparisonValue not in the valid range or format"

    var result = TriggerValidator.validateTrigger(triggerRequest.copy(
      triggerVariable = "Occupancy",
      comparisonValue = "-1"
    ))
    assert(result.equals(errorMsg))

    result = TriggerValidator.validateTrigger(triggerRequest.copy(
      triggerVariable = "Occupancy",
      comparisonValue = "110"
    ))
    assert(result.equals(errorMsg))

    result = TriggerValidator.validateTrigger(triggerRequest.copy(
      triggerVariable = "Occupancy",
      comparisonValue = "abc"
    ))
    assert(result.equals(errorMsg))

    result = TriggerValidator.validateTrigger(triggerRequest.copy(
      triggerVariable = "Occupancy",
      comparisonValue = "50"
    ))
    assert(result.equals(""))

  }

  it should "validate the comparison values are in the desired range when triggerVariable is Vacancy " in {
    val errorMsg = "ComparisonValue not in the valid range or format"

    var result = TriggerValidator.validateTrigger(triggerRequest.copy(
      triggerVariable = "Vacancy",
      comparisonValue = "-1"
    ))
    assert(result.equals(errorMsg))

    result = TriggerValidator.validateTrigger(triggerRequest.copy(
      triggerVariable = "Vacancy",
      comparisonValue = "110"
    ))
    assert(result.equals(errorMsg))

    result = TriggerValidator.validateTrigger(triggerRequest.copy(
      triggerVariable = "Vacancy",
      comparisonValue = "abc"
    ))
    assert(result.equals(errorMsg))

    result = TriggerValidator.validateTrigger(triggerRequest.copy(
      triggerVariable = "Vacancy",
      comparisonValue = "60"
    ))
    assert(result.equals(""))
  }

  it should "validate the comparison values are in the desired range when triggerVariable is Turnover " in {
    val errorMsg = "ComparisonValue not in the valid range or format"

    var result = TriggerValidator.validateTrigger(triggerRequest.copy(
      triggerVariable = "Turnover",
      comparisonValue = "-1"
    ))
    assert(result.equals(errorMsg))

    result = TriggerValidator.validateTrigger(triggerRequest.copy(
      triggerVariable = "Turnover",
      comparisonValue = "abc"
    ))
    assert(result.equals(errorMsg))

    result = TriggerValidator.validateTrigger(triggerRequest.copy(
      triggerVariable = "Turnover",
      comparisonValue = "600"
    ))
    assert(result.equals(""))

  }


  it should "validate the comparison values are in the desired range when triggerVariable is TotalViolationCount " in {
    val errorMsg = "ComparisonValue not in the valid range or format"

    var result = TriggerValidator.validateTrigger(triggerRequest.copy(
      triggerVariable = "TotalViolationCount",
      comparisonValue = "-1"
    ))
    assert(result.equals(errorMsg))

    result = TriggerValidator.validateTrigger(triggerRequest.copy(
      triggerVariable = "TotalViolationCount",
      comparisonValue = "abc"
    ))
    assert(result.equals(errorMsg))

    result = TriggerValidator.validateTrigger(triggerRequest.copy(
      triggerVariable = "TotalViolationCount",
      comparisonValue = "600"
    ))
    assert(result.equals(""))
  }

  it should "validate the comparison values are in the desired range when triggerVariable is AvgDwelltime " in {
    val errorMsg = "ComparisonValue not in the valid range or format"

    var result = TriggerValidator.validateTrigger(triggerRequest.copy(
      triggerVariable = "AvgDwelltime",
      comparisonValue = "-1"
    ))
    assert(result.equals(errorMsg))

    result = TriggerValidator.validateTrigger(triggerRequest.copy(
      triggerVariable = "AvgDwelltime",
      comparisonValue = "abc"
    ))
    assert(result.equals(errorMsg))

    result = TriggerValidator.validateTrigger(triggerRequest.copy(
      triggerVariable = "AvgDwelltime",
      comparisonValue = "600.0"
    ))
    assert(result.equals(""))

  }

  it should "validate the comparison values are in the desired range when triggerVariable is TotalRevenue " in {
    val errorMsg = "ComparisonValue not in the valid range or format"

    var result = TriggerValidator.validateTrigger(triggerRequest.copy(
      triggerVariable = "TotalRevenue",
      comparisonValue = "-1"
    ))
    assert(result.equals(errorMsg))

    result = TriggerValidator.validateTrigger(triggerRequest.copy(
      triggerVariable = "TotalRevenue",
      comparisonValue = "abc"
    ))
    assert(result.equals(errorMsg))

    result = TriggerValidator.validateTrigger(triggerRequest.copy(
      triggerVariable = "TotalRevenue",
      comparisonValue = "600.0"
    ))
    assert(result.equals(""))

  }


  it should "validate the comparison values are in the desired range when comparisonVariableOperator is Percentage " in {
    val errorMsg = "ComparisonValue not in the valid range or format"

    var result = TriggerValidator.validateTrigger(triggerRequestIncludingComparison.copy(
      comparisonValue = "-1"
    ))
    assert(result.equals(errorMsg))

    result = TriggerValidator.validateTrigger(triggerRequestIncludingComparison.copy(
      comparisonValue = "101"
    ))
    assert(result.equals(errorMsg))

    result = TriggerValidator.validateTrigger(triggerRequestIncludingComparison.copy(
      comparisonValue = "abc"
    ))
    assert(result.equals(errorMsg))


    result = TriggerValidator.validateTrigger(triggerRequestIncludingComparison.copy(
      comparisonValue = "100.0"
    ))
    assert(result.equals(""))

    result = TriggerValidator.validateTrigger(triggerRequestIncludingComparison.copy(
      comparisonValue = "75.5"
    ))
    assert(result.equals(""))

  }




}
