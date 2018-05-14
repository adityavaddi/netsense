package com.vz.nsp.trigger.model.app

import java.time.Instant

import com.fasterxml.jackson.annotation.JsonProperty
import com.verizon.netsense.utils.Logging
import com.vz.nsp.trigger.model.db.DBTrigger
import com.vz.nsp.util.ObjectMapperUtil

/**
  * Created by maleva on 2/13/18.
  */
case class TriggerResource(resourceId: String, resourceName: String)

case class TriggerResponse(triggerId: String,
                           triggerName: String,
                           userId: String,
                           createdOn: String,
                           lastUpdated: String,
                           triggerCategory: String,
                           triggerSubcategory: String,
                           @JsonProperty resourceList: List[TriggerResource],
                           siteId: String,
                           orgId: String,
                           timePeriod: String,
                           triggerVariable: String,
                           comparisonVariableOperator: String,
                           comparisonVariable: String,
                           comparisonOperator: String,
                           comparisonValue: String,
                           userMessage: String,
                           severity: String,
                           additionalUserMessage: String
                          )


case class TriggerRequest(triggerName: String = "",
                          triggerCategory: String = "",
                          triggerSubcategory: String = "",
                          @JsonProperty resourceList: List[TriggerResource] = List[TriggerResource](),
                          timePeriod: String = "",
                          triggerVariable: String = "",
                          comparisonVariableOperator: Option[String] = None,
                          comparisonVariable: Option[String] = None,
                          comparisonOperator: String = "",
                          comparisonValue: String = "",
                          userMessage: String = "",
                          severity: String = "",
                          additionalUserMessage: Option[String] = None
                         )

object TriggerResponse extends Logging {
  def apply(trigger: DBTrigger): TriggerResponse =
    TriggerResponse(
      triggerId = trigger.triggerid,
      triggerName = trigger.triggername,
      userId = trigger.userid,
      createdOn = convertEpochToDate(trigger.createdon),
      lastUpdated = convertEpochToDate(trigger.lastupdated),
      triggerCategory = trigger.triggercategory,
      triggerSubcategory = trigger.triggersubcategory,
      resourceList = convertToResourceList(trigger.resourcelistjson),
      siteId = trigger.siteid,
      orgId = trigger.orgid,
      timePeriod = trigger.timeperiod,
      triggerVariable = trigger.triggervariable,
      comparisonVariableOperator = trigger.comparisonvariableoperator,
      comparisonVariable = trigger.comparisonvariable,
      comparisonOperator = trigger.comparisonoperator,
      comparisonValue = trigger.comparisonvalue,
      userMessage = trigger.usermessage,
      severity = trigger.severity,
      additionalUserMessage = trigger.additionalusermessage
    )


  def convertEpochToDate(epoch: Long) =
    if (epoch == 0) "" else Instant.ofEpochMilli(epoch).toString



  def convertToResourceList(resourceList: String): List[TriggerResource] = {
    try {
      ObjectMapperUtil.fromJson[List[TriggerResource]](resourceList)
    }
    catch {
      case ex: Exception =>
        log.error(s"Exception occured while converting to TriggerResourceList from ResourceListJson: $resourceList")
        List()
    }
  }
}
