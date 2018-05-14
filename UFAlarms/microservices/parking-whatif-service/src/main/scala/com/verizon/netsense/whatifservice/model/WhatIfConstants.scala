package com.verizon.netsense.whatifservice.model

/**
  * Created by maleva on 3/18/18.
  */
sealed case class WhatIfConstants(value: String)
object WhatIfConstants {

   object createWhatIfJob extends WhatIfConstants("createWhatIfJob")

   object getAllWhatIfJobs extends WhatIfConstants("getAllWhatIfJobs")

   object getWhatIfJob extends WhatIfConstants("getWhatIfJob")

   object searchWhatIfJob extends WhatIfConstants("searchWhatIfJob")

   object updateWhatIfJob extends WhatIfConstants("updateWhatIfJob")

   object abortWhatIfJob extends WhatIfConstants("abortWhatIfJob")

   object deleteWhatIfJob extends WhatIfConstants("deleteWhatIfJob")

   object Pending extends WhatIfConstants("pending")

   object Running extends WhatIfConstants("running")

   object Aborted extends WhatIfConstants("aborted")

   object AbnormallyEnded extends WhatIfConstants("abnormally-ended")

   object Completed extends WhatIfConstants("completed")

   object EntirePeriod extends WhatIfConstants("entire_period")

   object AsIs extends WhatIfConstants("as-is")

   object NoViolations extends WhatIfConstants("no-violations")

   object JobId extends WhatIfConstants("jobid")

   object name extends WhatIfConstants("name")

   object fromTime extends WhatIfConstants("fromTime")

   object toTime extends WhatIfConstants("toTime")

   object classNameKey extends WhatIfConstants("className")

   object fileKey extends WhatIfConstants("file")

   object driverMemoryKey extends WhatIfConstants("driverMemory")

   object nameKey extends WhatIfConstants("name")

   object proxyUserKey extends WhatIfConstants("proxyUser")

   object confMapKey extends WhatIfConstants("conf")

   object argsKey extends WhatIfConstants("args")


}