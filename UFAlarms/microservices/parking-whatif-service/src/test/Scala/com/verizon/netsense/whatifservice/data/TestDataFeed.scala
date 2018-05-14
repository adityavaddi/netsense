package com.verizon.netsense.whatifservice.data

import java.util.{Calendar, UUID}

import com.verizon.netsense.whatifservice.model.{WhatIfJob, WhatIfJobRequest, WhatIfJobResponse}
import com.verizon.netsense.whatifservice.model.casel._

/**
  * Created by maleva on 4/19/18.
  */




object TestDataFeed {



  val parkingGroups=List("")
  val parkingPolicies=List(UUID.randomUUID().toString)
  val batchId =200

  val siteId =UUID.randomUUID().toString
  val orgId =  UUID.randomUUID().toString
  val name = "name"
  val fromTime ="2013-04-30T19:17:11.138Z"
  val toTime ="2014-04-30T19:17:11.138Z"


  val mode = "as-is"



  val instanceId = UUID.randomUUID().toString
  val requestId = UUID.randomUUID().toString
  val timestamp = Calendar.getInstance().toInstant.toString
  val model = "NodeModel"
  val action = "CAN_READ"
  val user = "d48dcd46-da6e-49b0-b76e-e3cbc79c4565"
  val orgProps = OrgProps(orgId)
  val siteProps = SiteProps(siteId)
  val jobId = UUID.randomUUID().toString

  val whatIfRequest = WhatIfJobRequest(Some(jobId),name,fromTime,toTime,parkingPolicies,parkingGroups,mode)

  val whatIfProps = WhatIfProps(Some(jobId),Some(whatIfRequest),None)

  val userProps=UserProps(Some("sensity_user@sensity.com"))

  val messageId = UUID.randomUUID().toString

  def requestBody(requestType: String)= RequestBody(
    instanceId,
    requestId,
    timestamp,
    requestType,
    model,
    action,
    Some(user),
    orgProps,
    siteProps,
    Some(whatIfProps),
    userProps
  )





  def appRequest(requestType: String) =
    AppRequest(messageId, messageId, requestBody(requestType))


  def whatIf = WhatIfJob(
    Option("createdbyuserid"),
    Option("lastupdatedbyuserid"),
    Option(0),
    Option(0),
    orgId,
    siteId,
    jobId,
    Option("name1"),
    Option(0),
    Option(0),
    Option(parkingGroups),
    Option(0),
    Option(0),
    Option(0),
    Option(0),
    Option("Pending"),
    Option(200),
    Option("appid"),
    Option("mode"),
    Option("additionalmessage"),
    Option(parkingGroups),
    Option("email"),
    Option("intervalperiod")
  )

  def whatIf2 = WhatIfJob(
    Option("createdbyuserid"),
    Option("lastupdatedbyuserid"),
    Option(0),
    Option(0),
    orgId,
    siteId,
    "jobid",
    Option("name"),
    Option(0),
    Option(0),
    Option(parkingGroups),
    Option(0),
    Option(0),
    Option(0),
    Option(0),
    Option("jobstatus"),
    Option(200),
    Option("appid"),
    Option("mode"),
    Option("additionalmessage"),
    Option(parkingGroups),
    Option("email"),
    Option("intervalperiod")
  )

  def whatIfResponse = WhatIfJobResponse(
    "createdbyuserid",
    "createdByUserName",
    "lastupdatedbyuserid",
    "LastUpdatedByUserName",
    "createdAt",
    "lastUpdatedAt",
    orgId,
    siteId,
    "jobid",
    "name2",
    "0",
    "0",
    List(""),
    List(""),
    "0",
    "0",
    "0",
    "0",
    "pending",
    mode="mode",
    additionalMessage= "additionalMessgae"
  )
}
