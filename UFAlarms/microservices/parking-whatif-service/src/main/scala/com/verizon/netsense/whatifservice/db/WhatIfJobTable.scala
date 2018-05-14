package com.verizon.netsense.whatifservice.db

import com.outworkers.phantom.dsl.{ConsistencyLevel, _}
import com.outworkers.phantom.keys.PartitionKey
import com.verizon.netsense.whatifservice.config.WhatIfConfigLoader
import com.verizon.netsense.whatifservice.model.WhatIfJob

import scala.concurrent.Future

abstract class WhatIfJobTable extends Table[WhatIfJobTable, WhatIfJob] {

  override def tableName: String = WhatIfConfigLoader.whatIfJobTableName

  object createdbyuserid extends OptionalCol[String]

  object lastupdatedbyuserid extends OptionalCol[String]

  object createdat extends OptionalCol[Long]

  object updatedat extends OptionalCol[Long]

  object orgid extends StringColumn with PartitionKey

  object siteid extends StringColumn with PartitionKey

  object jobid extends StringColumn with ClusteringOrder

  object name extends OptionalCol[String]

  object fromtime extends OptionalCol[Long]

  object totime extends OptionalCol[Long]

  object parkinggroups extends OptionalCol[List[String]]

  object starttime extends OptionalCol[Long]

  object endtime extends OptionalCol[Long]

  object submittime extends OptionalCol[Long]

  object aborttime extends OptionalCol[Long]

  object jobstatus extends OptionalCol[String]

  object batchid extends OptionalCol[Int]

  object appid extends OptionalCol[String]

  object mode extends OptionalCol[String]

  object additionalmessage extends OptionalCol[String]

  object policieswithversion extends OptionalCol[List[String]]

  object email extends OptionalCol[String]

  object intervalperiod extends OptionalCol[String]
}

abstract class ConcreteWhatIfJob extends WhatIfJobTable with RootConnector {

  def storeJobStatus(jobStatus: WhatIfJob): Future[ResultSet] =
    insert
      .value(_.createdbyuserid, jobStatus.createdbyuserid)
      .value(_.lastupdatedbyuserid, jobStatus.lastupdatedbyuserid)
      .value(_.createdat, jobStatus.createdat)
      .value(_.updatedat, jobStatus.updatedat)
      .value(_.orgid, jobStatus.orgid)
      .value(_.siteid, jobStatus.siteid)
      .value(_.jobid, jobStatus.jobid)
      .value(_.name, jobStatus.name)
      .value(_.fromtime, jobStatus.fromtime)
      .value(_.totime, jobStatus.totime)
      .value(_.parkinggroups, jobStatus.parkinggroups)
      .value(_.starttime, jobStatus.starttime)
      .value(_.endtime, jobStatus.endtime)
      .value(_.submittime, jobStatus.submittime)
      .value(_.aborttime, jobStatus.aborttime)
      .value(_.jobstatus, jobStatus.jobstatus)
      .value(_.batchid, jobStatus.batchid)
      .value(_.appid, jobStatus.appid)
      .value(_.mode, jobStatus.mode)
      .value(_.additionalmessage, jobStatus.additionalmessage)
      .value(_.policieswithversion, jobStatus.policieswithversion)
      .value(_.email, jobStatus.email)
      .value(_.intervalperiod, jobStatus.intervalperiod)
      .consistencyLevel_=(ConsistencyLevel.LOCAL_QUORUM)
      .future()

  def getJobId(orgid: String, siteid: String, jobId: String): Future[Option[WhatIfJob]] =
    select
      .where(_.jobid eqs jobId)
      .and(_.orgid eqs orgid)
      .and(_.siteid eqs siteid)
      .consistencyLevel_=(ConsistencyLevel.LOCAL_QUORUM)
      .one()

  def getAllJobs(orgid: String, siteid: String): Future[List[WhatIfJob]] =
    select
      .where(_.orgid eqs orgid)
      .and(_.siteid eqs siteid)
      .consistencyLevel_=(ConsistencyLevel.LOCAL_QUORUM)
      .fetch()

  def deleteByJobId(orgid: String, siteid: String, jobid: String): Future[ResultSet] =
    delete
      .where(_.orgid eqs orgid)
      .and(_.siteid eqs siteid)
      .and(_.jobid eqs jobid)
      .ifExists
      .consistencyLevel_=(ConsistencyLevel.LOCAL_QUORUM)
      .future()

  def updateByJobId(orgid: String, siteid: String, jobid: String, jobStatus: WhatIfJob): Future[ResultSet] =
    update
      .where(_.jobid eqs jobid)
      .and(_.siteid eqs siteid)
      .and(_.orgid eqs orgid)
      .modify(_.createdbyuserid setTo jobStatus.createdbyuserid)
      .and(_.lastupdatedbyuserid setTo jobStatus.lastupdatedbyuserid)
      .and(_.createdat setTo jobStatus.createdat)
      .and(_.updatedat setTo jobStatus.updatedat)
      .and(_.name setTo jobStatus.name)
      .and(_.fromtime setTo jobStatus.fromtime)
      .and(_.totime setTo jobStatus.totime)
      .and(_.parkinggroups setTo jobStatus.parkinggroups)
      .and(_.starttime setTo jobStatus.starttime)
      .and(_.endtime setTo jobStatus.endtime)
      .and(_.submittime setTo jobStatus.submittime)
      .and(_.aborttime setTo jobStatus.aborttime)
      .and(_.jobstatus setTo jobStatus.jobstatus)
      .and(_.batchid setTo jobStatus.batchid)
      .and(_.appid setTo jobStatus.appid)
      .and(_.mode setTo jobStatus.mode)
      .and(_.additionalmessage setTo jobStatus.additionalmessage)
      .and(_.policieswithversion setTo jobStatus.policieswithversion)
      .and(_.email setTo jobStatus.email)
      .and(_.intervalperiod setTo jobStatus.intervalperiod)
      .ifExists
      .consistencyLevel_=(ConsistencyLevel.LOCAL_QUORUM)
      .future()

  def updateWhatIfBatchId(orgid: String,
                          siteid: String,
                          jobid: String,
                          batchid: Option[Int],
                          updatedat: Option[Long]): Future[ResultSet] =
    update
      .where(_.jobid eqs jobid)
      .and(_.siteid eqs siteid)
      .and(_.orgid eqs orgid)
      .modify(_.batchid setTo batchid)
      .and(_.updatedat setTo updatedat)
      .ifExists
      .consistencyLevel_=(ConsistencyLevel.LOCAL_QUORUM)
      .future()

  def updateWhatIfJobStatus(orgid: String,
                            siteid: String,
                            jobid: String,
                            jobstatus: Option[String],
                            updatedat: Option[Long],
                            additionalmessage: Option[String]): Future[ResultSet] =
    update
      .where(_.jobid eqs jobid)
      .and(_.siteid eqs siteid)
      .and(_.orgid eqs orgid)
      .modify(_.jobstatus setTo jobstatus)
      .and(_.additionalmessage setTo additionalmessage)
      .and(_.updatedat setTo updatedat)
      .ifExists
      .consistencyLevel_=(ConsistencyLevel.LOCAL_QUORUM)
      .future()
}
