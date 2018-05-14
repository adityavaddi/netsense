package com.vz.nsp.trigger.model.db

import java.util.UUID

import com.fasterxml.jackson.annotation.JsonProperty
import com.vz.nsp.trigger.helper.CommonHelper.getCurrentTime
import com.vz.nsp.trigger.model.app.{TriggerRequest, TriggerResource, TriggerResponse}
import com.vz.nsp.util.ObjectMapperUtil

/**
  * Created by maleva on 2/13/18.
  */
case class DBTrigger(triggerid: String,
                     triggername: String,
                     userid: String,
                     createdon: Long,
                     lastupdated: Long,
                     triggercategory: String,
                     triggersubcategory: String,
                     resourcelistjson: String,
                     siteid: String,
                     orgid: String,
                     timeperiod: String,
                     triggervariable: String,
                     comparisonvariableoperator: String,
                     comparisonvariable: String,
                     comparisonoperator: String,
                     comparisonvalue: String,
                     usermessage: String,
                     additionalusermessage: String,
                     severity: String,
                     isdeleted: Boolean
                    )

object DBTrigger {
  def apply(trigger: TriggerRequest, _orgid: String, _siteid: String, _triggerid: String,
            _userid: String, _createdon: Option[Long] = None): DBTrigger =
    DBTrigger(triggerid = _triggerid,
      triggername = trigger.triggerName,
      userid = _userid,
      createdon = buildCreatedTime(_createdon),
      lastupdated = getCurrentTime,
      triggercategory = trigger.triggerCategory,
      triggersubcategory = trigger.triggerSubcategory,
      resourcelistjson = convertResourceListToJson(trigger.resourceList),
      siteid = _siteid,
      orgid = _orgid,
      timeperiod = trigger.timePeriod,
      triggervariable = trigger.triggerVariable,
      comparisonvariableoperator = trigger.comparisonVariableOperator.getOrElse(""),
      comparisonvariable = trigger.comparisonVariable.getOrElse(""),
      comparisonoperator = trigger.comparisonOperator,
      comparisonvalue = trigger.comparisonValue,
      usermessage = trigger.userMessage,
      additionalusermessage = trigger.additionalUserMessage.getOrElse(""),
      severity = trigger.severity,
      isdeleted = false
    )

  def buildCreatedTime(_createdon: Option[Long]) = {
    _createdon match {
      case Some(created) => created
      case None => getCurrentTime
    }
  }

  def convertResourceListToJson(resourceList: List[TriggerResource]): String = {
    ObjectMapperUtil.toJson(resourceList)
  }

}

