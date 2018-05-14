package com.verizon.netsense.businessalertservice.model

sealed case class ResponseMessage(value: String)

object ResponseMessage {

  object IrrelevantType extends ResponseMessage("Irrelevant type found in the request ")

  object BusinessAlertIdMissingInRequest extends ResponseMessage("Required businessAlertId missing in request ")

  object SearchMissingInRequest extends ResponseMessage("Required search missing in request ")

  object UnAuthorizedUser extends ResponseMessage("UnAuthorized user trying to access internal business alert API")

  object condition_correct
      extends ResponseMessage("Given search criteria is invalid please give like example severity eq CLEAR")

  object invalid_key
      extends ResponseMessage("search can be performed on any one of these severity, active, triggerName, resourceId ")

  object NotFound_Orgid extends ResponseMessage("Required orgid missing in request ")

  object NotFound_Siteid extends ResponseMessage("Required siteid missing in request ")

  object ISR_WrongRequestJson extends ResponseMessage("Request Json was corrupted (or) not expected type ")

  object InternalServerError extends ResponseMessage("Internal server error ")

  object NotFound_BusinessAlertId_In_db extends ResponseMessage("Business Alert not found with id : ")

  object Active_Or_Acknowledged_Alert extends ResponseMessage("Cannot access inactive/acknowledged business alert")

  object Does_Not_Exist_OrgSite extends ResponseMessage(" or doesn't belong to orgid: ")

  object Siteid extends ResponseMessage(" siteid: ")

  object InternalError_DismissAlert
      extends ResponseMessage("Internal server error while dismissing the business alert ")
  object BusinessAlertOrSearchMissing extends ResponseMessage(" Required businessAlertId/search missing in request: ")

  object Invalid_Active extends ResponseMessage("active could be either true or false")

}
