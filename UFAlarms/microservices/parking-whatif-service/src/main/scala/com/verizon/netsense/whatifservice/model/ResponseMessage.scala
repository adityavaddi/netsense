package com.verizon.netsense.whatifservice.model

sealed case class ResponseMessage(value: String)

object ResponseMessage {

  object NotFound_PolicyCatId extends ResponseMessage("Falied to create the job ")

  object WhatIfJobIdMissingInRequest extends ResponseMessage("Required jobId missing in request ")

  object IrrelevantType extends ResponseMessage("Irrelevant type found in the request ")

  object NotFound_WhatIfJobId_In_db extends ResponseMessage("What if Job not found with id : ")

  object Does_Not_Exist_OrgSite extends ResponseMessage(" or doesn't belong to orgid: ")

  object Siteid extends ResponseMessage(" siteid: ")

  object Parking_Policies extends ResponseMessage("Parking Policies : ")

  object condition_correct
      extends ResponseMessage("Given search criteria is invalid please give like example severity eq CLEAR")

  object invalid_key extends ResponseMessage("invalid search key")

  object WhatIf_Name_Exist extends ResponseMessage("What If already exist with name ")

  object Abort_Job extends ResponseMessage("Abort Job before you edit what_if job")

  object All_Invalid_Jobs extends ResponseMessage(", do not belong to orgid: ")

  object Inavlid_Job_Status extends ResponseMessage("Job status is invalid")

  object status extends ResponseMessage(" status")

  object BatchId extends ResponseMessage(" batchId: ")

  object Spark_API extends ResponseMessage("Spark API's not available")

  object Spark_Payload extends ResponseMessage("Spark request payload is missing for AppRequest ")

  object Job extends ResponseMessage(" and jobStatus: ")

  object Invalid_Delete_Status extends ResponseMessage("Internal server error while deleting the WhatIf with id : ")

  object Invalid_Arguments extends ResponseMessage("Invalid arguments to writeSparkStatusToCassandra with sparkJobRequest")

  object Job_Current extends ResponseMessage(". Job is currently in ")

  object SearchMissingInRequest extends ResponseMessage("Required search missing in request ")

  object WhatIfJobOrSearchMissing extends ResponseMessage(" Required WhatIfJobId/search missing in request: ")

  object Missing_Searchstring extends ResponseMessage("Service errror due to missing search string in the AppRequest")

  object Missing_WhatIfprops extends ResponseMessage("Service errror due to missing WhatIfprops in the AppRequest")

  object Missing_jobId extends ResponseMessage("Required jobid missing in request ")

  object Missing_WhatIf extends ResponseMessage("Required whatif payload missing in request ")

  object Missing_WhatIfJobId extends ResponseMessage("Whatif not found with id : ")

  object InternalError_UpdateWhatIf extends ResponseMessage("Internal server error while updating WhatIf ")

  object WhatIf_ValidationFailed extends ResponseMessage("What if mode should be as-is or no-violations ")

  object DateFormatError
      extends ResponseMessage(
        "Date Format should be in this format: yyyy-MM-dd'T'HH:mm:ss.SSS'Z' and fromTime should be less than toTime "
      )

  object ParkingGroup extends ResponseMessage("One or more of the provided parking groups are invalid")

  object RequiredFieldsMissing extends ResponseMessage("Required fields missing in the request ")

  object AppIdNotFound extends ResponseMessage("appid not exists with this id: ")

  object SparkFailureAddMessage extends ResponseMessage("Failed to call spark job")

  object SparkTimeOut extends ResponseMessage("Spark API call failed due to timeout")

}
