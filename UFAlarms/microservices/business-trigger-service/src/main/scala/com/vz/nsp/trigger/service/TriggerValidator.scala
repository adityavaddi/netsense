package com.vz.nsp.trigger.service

import com.vz.nsp.trigger.config.ServiceConfig._
import com.vz.nsp.trigger.model.app.TriggerRequest
import com.vz.nsp.trigger.model.casel.ResponseMessage.{Trigger_ComparisonValueValidationFailed, Trigger_ComparisonVariableValidationFailed, Trigger_ResourceValidationFailed, Trigger_ValidationFailed}

object TriggerValidator {

  private[this] lazy val timePeriod = "timePeriod"
  private[this] lazy val triggerCategory = "triggerCategory"
  private[this] lazy val triggerSubcategory = "triggerSubcategory"
  private[this] lazy val triggerVariable = "triggerVariable"
  private[this] lazy val comparisonOperator = "comparisonOperator"
  private[this] lazy val severity = "severity"
  private[this] lazy val comparisonVariableOperator = "comparisonVariableOperator"
  private[this] lazy val comparisonVariable = "comparisonVariable"
  private[this] lazy val totalViolationCount = "TotalViolationCount"
  private[this] lazy val totalRevenue = "TotalRevenue"
  private[this] lazy val turnover = "Turnover"
  private[this] lazy val vacancy = "Vacancy"
  private[this] lazy val occupancy = "Occupancy"
  private[this] lazy val avgDwelltime = "AvgDwelltime"
  private[this] lazy val percentage = "Percentage"

  private[this] lazy val fieldValidator: (String, String, List[String], List[String]) => List[String] =
    (field, value, validationset, errorList) => if (validationset.contains(value)) errorList else errorList :+ field

  def validateTrigger(trigger: TriggerRequest): String = {
    val errorList = List(validateDeclaredFields(trigger),
      validateComparisonVariables(trigger),
      validateTriggerResources(trigger),
      validateComparisonValue(trigger)
    )
    errorList.filter(!_.isEmpty).mkString(";")
  }

  private[this] def validateDeclaredFields(trigger: TriggerRequest): String = {
    val errorList = trigger.getClass.getDeclaredFields.foldLeft(List[String]())(
      (acc, key) =>
        key.getName match {
          case `timePeriod` => fieldValidator(timePeriod, trigger.timePeriod, timePeriodSet, acc)
          case `triggerCategory` => fieldValidator(triggerCategory, trigger.triggerCategory, triggerCategorySet, acc)
          case `triggerSubcategory` =>
            fieldValidator(triggerSubcategory, trigger.triggerSubcategory, triggerSubcategorySet, acc)
          case `triggerVariable` =>
            fieldValidator(triggerVariable, trigger.triggerVariable, triggerVariableSet, acc)
          case `comparisonOperator` =>
            fieldValidator(comparisonOperator, trigger.comparisonOperator, comparisonOperatorSet, acc)
          case `severity` => fieldValidator(severity, trigger.severity, severitySet, acc)
          case _ => acc
        }
    )
    if (errorList.isEmpty) "" else Trigger_ValidationFailed.value + errorList.mkString(",")
  }

  private[this] def validateComparisonVariables(trigger: TriggerRequest): String = {
    (trigger.comparisonVariable, trigger.comparisonVariableOperator, trigger.triggerVariable) match {
      case (Some(cv), Some(co), tv) =>
        comparisonVariableOperatorSet.contains(co) && comparisonVariableSet.contains(cv) && tv == totalViolationCount match {
          case true => ""
          case false => Trigger_ComparisonVariableValidationFailed.value
        }
      case (None, None, _) => ""
      case _ => Trigger_ComparisonVariableValidationFailed.value
    }
  }

  private[this] def validateTriggerResources(trigger: TriggerRequest): String = {
    val invalidResources = trigger.resourceList.foldLeft(0)((invalidResourceCounter, resource) => {
      if (resource.resourceId.isEmpty || resource.resourceName.isEmpty) invalidResourceCounter + 1
      else invalidResourceCounter
    })
    if (invalidResources > 0) Trigger_ResourceValidationFailed.value else ""
  }

  private[this] def validateComparisonValue(trigger: TriggerRequest): String = {
    if (trigger.comparisonValue == null) Trigger_ComparisonValueValidationFailed.value else trigger.comparisonValue.equals("") match {
      case true => Trigger_ComparisonValueValidationFailed.value
      case false =>
        try {
          trigger.comparisonVariableOperator match {
            case Some(comVarOperator) =>
              val cmpVal = trigger.comparisonValue.toDouble
              if (comVarOperator == percentage && cmpVal >= 0 && cmpVal <= 100) {
                ""
              }
              else {
                Trigger_ComparisonValueValidationFailed.value
              }
            case None =>
              trigger.triggerVariable match {
                case tvar if tvar == avgDwelltime || tvar == totalRevenue =>
                  if (trigger.comparisonValue.toDouble >= 0) "" else Trigger_ComparisonValueValidationFailed.value
                case tvar if tvar == vacancy || tvar == occupancy =>
                  val cmpVal = trigger.comparisonValue.toDouble
                  if (cmpVal >= 0 && cmpVal <= 100) "" else Trigger_ComparisonValueValidationFailed.value
                case tvar if tvar == turnover || tvar == totalViolationCount =>
                  if (trigger.comparisonValue.toInt >= 0) "" else Trigger_ComparisonValueValidationFailed.value
                case _ => ""
              }
          }
        } catch {
          case nfe: NumberFormatException => Trigger_ComparisonValueValidationFailed.value
        }
    }
  }

}
