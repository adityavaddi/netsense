package com.vz.nsp.trigger.model.casel

sealed case class ResponseMessage(value: String)

object ResponseMessage {

  object IrrelevantType extends ResponseMessage("Irrelevant type found in the request ")

  object Missing_Triggerprops extends ResponseMessage("Service errror due to missing TriggerProps in the AppRequest")

  object Missing_Searchstring extends ResponseMessage("Service errror due to missing search string in the AppRequest")

  object Missing_TriggerId extends ResponseMessage("Required triggerId missing in request ")

  object Missing_Trigger extends ResponseMessage("Required trigger payload missing in request ")

  object Trigger_ValidationFailed extends ResponseMessage("Validation failed for following trigger variables ")

  object Trigger_ResourceValidationFailed extends ResponseMessage("Trigger resource id/name should not be empty")

  object Trigger_ComparisonVariableValidationFailed extends ResponseMessage("Invalid comparisonVariable/comparisonVariableOperator/triggerVariable")

  object Trigger_ComparisonValueValidationFailed extends ResponseMessage("ComparisonValue not in the valid range or format")

  object Trigger_Name_Exist extends ResponseMessage("Trigger already exist with name ")

  object NotFound_Orgid extends ResponseMessage("Required orgid missing in request ")

  object NotFound_Siteid extends ResponseMessage("Required siteid missing in request ")

  object NotFound_appuserdataprops extends ResponseMessage("Required appuserdataprops missing in request ")

  object NotFound_UserId extends ResponseMessage("Required userId missing in request ")

  object NotFound_AppId extends ResponseMessage("Required appId missing in request ")

  object NotFound_UserDataId extends ResponseMessage("Required userDataId missing in request ")

  object NotFound_UserData extends ResponseMessage("UserData not found for userDataId ")

  object NotFound_DataValue_Template extends ResponseMessage("Required data value/Template data is missing in request ")

  object NotFound_Default extends ResponseMessage("One or many Required fields are missing in request ")

  object No_User_Data_Found extends ResponseMessage("No User Data available for this request ")

  object Missing_DBTriggerId extends ResponseMessage("Trigger not found with id : ")

  object TriggerIdMissingInRequest extends ResponseMessage("Trigger id missing in request ")

  object NotFound_PolicyCat extends ResponseMessage("Trigger not found in the request ")

  object NotFound_Policy extends ResponseMessage("Policy not found in the request ")

  object NotFound_PolicyId extends ResponseMessage("Required policyid missing in request")

  object NotFound_PolicyId_Table extends ResponseMessage("Policy not found with id : ")

  object Does_Not_Exist_OrgSite extends ResponseMessage(" or doesn't belong to orgid: ")

  object Siteid extends ResponseMessage(" siteid: ")

  object InternalError_UpdateTrigger extends ResponseMessage("Internal server error while updating the policy trigger ")

  object InternalError_StorePolicy extends ResponseMessage("Internal server error while saving the policy ")

  object InternalError_UpdatePolicy extends ResponseMessage("Internal server error while updating the policy ")

  object InternalServerError extends ResponseMessage("Internal server error ")

  object ISR_WrongRequestJson extends ResponseMessage("Request Json was corrupted (or) not expected type ")

  object NoFound_policyId_GroupId extends ResponseMessage("Parkinggroupid and policyid not found ")

  object groupLinkObjectMissing extends ResponseMessage("Required groupLink object missing in request ")

  object policyidMissingInReq extends ResponseMessage("Required policyid missing in request ")

  object policyMissingInReq extends ResponseMessage("Required policy object missing in request ")

  object groupAssociatedWithOtherPolicy
      extends ResponseMessage("Specified parking group is already associated with other policy ")

  object policyAssociatedToGroup extends ResponseMessage("Operation failed because policy is linked to some group ")

  object groupNotAssociatedPolicy extends ResponseMessage("Specified parking group is not associated with any policy ")

  object InternalServerError_associating
      extends ResponseMessage("Internal server error while associating parking group to policy")

  object InternalServerError_disassociating
      extends ResponseMessage("Internal server error while disassociating parking group to policy")

  object versionMissingInReq extends ResponseMessage("Required version number missing in request")

  object NotFound_policyId_version extends ResponseMessage("Policy not found with id : ")

  object NotFound_parkinggroupid_version extends ResponseMessage("Policy not found with id : ")

  object timelineRequestMissingFields
      extends ResponseMessage("Required parkinggroupid or fromTime or toTime missing in request : ")

  object NotFound_spotId extends ResponseMessage("ParkingSpot not found with id : ")

  object NotFound_parkingspot extends ResponseMessage("Parkingspot not found in the request")

  object InternalError_StoreSpot extends ResponseMessage("Internal server error while saving the parkingspot")

  object NotFound_parkingspotId extends ResponseMessage("Required parkingspotid missing in request ")

  object InternalError_deletespot extends ResponseMessage("Internal server error while deleting the parkingspot ")

  object InternalError_Updatespot extends ResponseMessage("Internal server error while updating the parkingspot")

  object parkingspot_exist extends ResponseMessage("Parkingspot already exist with this id: ")

  object groupNotAssociatedWithThisPolicy
      extends ResponseMessage("Specified parking group is not associated with this policy id: ")

  object InternalServerError_inactiveState
      extends ResponseMessage("Internal server error while updating the state of policy to inactive")

  object condition_correct
      extends ResponseMessage("Given search criteria is invalid please give like example severity eq CLEAR")

  object invalid_key
    extends ResponseMessage("Search can be performed on any one of these severity, triggerCategory, triggerName")

  object NoContent_RequestBody extends ResponseMessage("Required triggerids missing in request")

  object Does_Not_Exist_appid extends ResponseMessage(" or doesn't belong to appid: ")

  object userid extends ResponseMessage(" userid: ")

}
