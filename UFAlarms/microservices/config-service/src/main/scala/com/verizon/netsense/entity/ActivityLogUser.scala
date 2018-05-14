package com.verizon.netsense.entity

import com.outworkers.phantom.dsl.UUID

case class ActivityLogUser (userid: String,
                            when: UUID,
                            activity: String,
                            message: String,
                            targetid: String,
                            targettype: String
                           )
