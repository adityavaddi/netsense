package com.vz.nsp.parking.model

/**
 * Created by maleva on 3/22/18.
 */

case class WhatIfJob(createdbyuserid: Option[String] = None,
                     lastupdatedbyuserid: Option[String] = None,
                     createdat: Option[Long] = None,
                     updatedat: Option[Long] = None,
                     orgid: String="",
                     siteid: String="",
                     jobid: String="",
                     name: Option[String] = None,
                     fromtime: Option[Long] = None,
                     totime: Option[Long] = None,
                     parkinggroups: Option[List[String]]=None,
                     starttime: Option[Long] = None,
                     endtime: Option[Long] = None,
                     submittime: Option[Long] = None,
                     aborttime: Option[Long] = None,
                     jobstatus: Option[String] = None,
                     batchid: Option[Int] = None,
                     appid: Option[String] = None,
                     mode: Option[String] = None,
                     additionalmessage: Option[String] = None,
                     policieswithversion: Option[List[String]]=None,
                     email: Option[String] = None,
                     intervalperiod: Option[String] = None)