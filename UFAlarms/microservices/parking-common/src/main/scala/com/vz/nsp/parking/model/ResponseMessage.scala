package com.vz.nsp.parking.model

sealed case class ResponseMessage(value: String)

object ResponseMessage {

  object IrrelevantType extends ResponseMessage("Irrelevant type found in the request ")

  object NotFound_PolicyCatId extends ResponseMessage("Required tagid missing in request ")

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

  object NotFound_TagId extends ResponseMessage("Tag not found with id : ")

  object TagIdMissingInRequest extends ResponseMessage("Tag id missing in request ")

  object Tag_Assigned extends ResponseMessage("Cannot delete tag because it was assigned to policy ")

  object NotFound_PolicyCat extends ResponseMessage("Tag not found in the request ")

  object NotFound_Policy extends ResponseMessage("Policy not found in the request ")

  object NotFound_PolicyId extends ResponseMessage("Required policyid missing in request")

  object NotFound_PolicyId_Table extends ResponseMessage("Policy not found with id : ")

  object Does_Not_Exist_OrgSite extends ResponseMessage(" or doesn't belong to orgid: ")

  object Siteid extends ResponseMessage(" siteid: ")

  object InternalError_DeleteTag extends ResponseMessage("Internal server error while deleting the policy tag ")

  object InternalError_UpdateTag extends ResponseMessage("Internal server error while updating the policy tag ")

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

  object NotFoundSpaceId extends ResponseMessage("Space attributes not found for these parkingspaceids : ")

  object NotFoundParkingSpace extends ResponseMessage("parkingspaceids not  found in the request")

  object InternalErrorStoreSpace extends ResponseMessage("Internal server error while saving the parkingspaces")

  object NotFoundParkingSpaceId extends ResponseMessage("Required parkingspaceid missing in request ")

  object InternalErrorSeleteSpot extends ResponseMessage("Internal server error while deleting the parkingspace ")

  object groupNotAssociatedWithThisPolicy
    extends ResponseMessage("Specified parking group is not associated with this policy id: ")

  object InternalServerError_inactiveState
    extends ResponseMessage("Internal server error while updating the state of policy to inactive")

  object NoContent_RequestBody extends ResponseMessage("Required tagids missing in request")

  object NotFound_TagIds extends ResponseMessage("Requested TagIds not available in Database")

  object NotAssociated_TagIds extends ResponseMessage("Requested TagIds not associated with policy")

  object Does_Not_Exist_appid extends ResponseMessage(" or doesn't belong to appid: ")

  object userid extends ResponseMessage(" userid: ")

  object TimeRangeConflictedEntries extends  ResponseMessage("Rules of the same priority cannot overlap in time: There are overlaps in timeranges between ")

  object InvalidParkingSpace extends ResponseMessage("Please give valid inputs for the space attributes in the request")

  object TagAlreadyExists extends ResponseMessage("Tag already exists with name ")

  object PolicyAlreadyExists extends ResponseMessage("Policy already exists with name ")

  object policyAssociatedToWhatIfProject extends ResponseMessage("Operation failed because policy is linked to WhatIf projects ")

  object Pending extends ResponseMessage("pending")

  object Running extends ResponseMessage("running")

  object Aborted extends ResponseMessage("aborted")

  object AbnormallyEnded extends ResponseMessage("abnormally-ended")

  object Completed extends ResponseMessage("completed")
}
