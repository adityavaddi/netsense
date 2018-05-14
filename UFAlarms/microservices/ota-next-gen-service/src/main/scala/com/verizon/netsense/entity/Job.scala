package com.verizon.netsense.entity

case class Job(jobid: String,
               nodeid: String,
               model: String,
               firmwareid: String) extends Entity
