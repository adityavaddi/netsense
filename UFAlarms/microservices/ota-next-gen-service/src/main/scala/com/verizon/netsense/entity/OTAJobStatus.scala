package com.verizon.netsense.entity

import com.outworkers.phantom.dsl.UUID

case class OTAJobStatus(jobid: String,
                        status: String,
                        when: UUID)

case class OTAJobStatusConverted(jobid: String,
                                 status: String,
                                 when: String)

