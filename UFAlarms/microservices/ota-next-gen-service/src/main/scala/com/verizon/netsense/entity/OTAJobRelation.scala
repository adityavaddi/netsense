package com.verizon.netsense.entity

import com.outworkers.phantom.dsl.UUID

case class OTAJobRelation(orgid: String,
                          siteid: String,
                          jobid: String,
                          when: UUID)

case class OTAJobRelationConverted(orgid: String,
                                   siteid: String,
                                   jobid: String,
                                   when: Long)
