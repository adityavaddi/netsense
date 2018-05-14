package com.verizon.netsense.entity

import java.util.UUID

import com.outworkers.phantom.dsl.UUID
import org.joda.time.DateTime

/*  Scala Representation of sic_status table */
case class SicStatus(
                      id: UUID,
                      msgId: String,
                      msgSrc: String,
                      msgType:String,
                      state: String,
                      ts: DateTime,
                      reason: String
                    )