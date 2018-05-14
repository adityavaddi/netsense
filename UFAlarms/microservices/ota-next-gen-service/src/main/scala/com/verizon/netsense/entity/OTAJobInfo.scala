package com.verizon.netsense.entity

import com.outworkers.phantom.dsl.UUID

case class OTAJobInfo(jobid: String,
                      firmwareid: String,
                      targetid: String,
                      targettype: String,
                      count: Int,
                      description: String,
                      when: UUID)

case class OTAJobInfoConverted(jobid: String,
                               firmwareid: String,
                               targetid: String,
                               targettype: String,
                               model: String,
                               count: Int,
                               description: String,
                               when: String)