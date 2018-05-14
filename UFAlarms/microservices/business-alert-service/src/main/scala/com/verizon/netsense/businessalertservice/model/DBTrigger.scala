package com.verizon.netsense.businessalertservice.model

case class DBTrigger(triggerid: String,
                     triggername: String = "",
                     userid: String = "",
                     createdon: Long = 0,
                     lastupdated: Long = 0,
                     triggercategory: String = "",
                     triggersubcategory: String = "",
                     resourcelistjson: String = "",
                     siteid: String = "",
                     orgid: String = "",
                     timeperiod: String = "",
                     triggervariable: String = "",
                     comparisonvariableoperator: String = "",
                     comparisonvariable: String = "",
                     comparisonoperator: String = "",
                     comparisonvalue: String = "",
                     usermessage: String = "",
                     additionalusermessage: String = "",
                     severity: String = "",
                     isdeleted: Boolean
                    )

case class TriggerResource(resourceId: String, resourceName: String)
