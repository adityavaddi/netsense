package com.verizon.netsense.whatifservice.model

import com.fasterxml.jackson.annotation.JsonProperty

/**
 * Created by maleva on 4/29/18.
 */
case class WhatIfSparkRequest(requestSpark: Boolean, @JsonProperty sparkJobRequest: Option[SparkJobRequest])

case class SparkJobRequest(jobId: String,
                           orgId: String,
                           siteId: String,
                           parkingGroupList: List[String],
                           parkingPolicyList: List[String],
                           fromTime: Long,
                           toTime: Long,
                           mode: String,
                           aggregationType: String,
                           userEmail: String)

case class SparkResultToCassandra(@JsonProperty sparkJobRequest: SparkJobRequest,
                                  jobId: Option[Int] = None,
                                  jobStatus: Option[String] = None)

object WhatIfSparkRequest {
  def apply(_requestSpark: Boolean = false,
            _jobId: String = "",
            _orgId: String = "",
            _siteId: String = "",
            _parkingGroupList: List[String] = List(),
            _parkingPolicyList: List[String] = List(),
            _fromTime: Long = 0l,
            _toTime: Long = 0l,
            _mode: String = "",
            _aggregationType: String = "",
            _userEmail: String = ""): WhatIfSparkRequest =
    if (_requestSpark) {
      WhatIfSparkRequest(
        requestSpark = _requestSpark,
        sparkJobRequest = Some(
          SparkJobRequest(
            jobId = _jobId,
            orgId = _orgId,
            siteId = _siteId,
            parkingGroupList = _parkingGroupList,
            parkingPolicyList = _parkingPolicyList,
            fromTime = _fromTime,
            toTime = _toTime,
            mode = _mode,
            aggregationType = _aggregationType,
            userEmail = _userEmail
          )
        )
      )
    } else {
      WhatIfSparkRequest(requestSpark = _requestSpark, sparkJobRequest = None)
    }
}
