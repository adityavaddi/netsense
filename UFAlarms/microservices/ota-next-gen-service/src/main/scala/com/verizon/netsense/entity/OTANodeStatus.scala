package com.verizon.netsense.entity

import com.outworkers.phantom.dsl.UUID

case class OTANodeStatus(jobid: String,
                         nodeid: String,
                         status: String,
                         progress: Long = -1,
                         when: UUID)

case class OTANodeStatusConverted(jobid: String,
                                  nodeid: String,
                                  status: String,
                                  progress: Long = -1,
                                  when: String)