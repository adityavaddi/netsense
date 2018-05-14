package com.vz.nsp.parking.db

import com.outworkers.phantom.dsl.{ConsistencyLevel, _}
import com.outworkers.phantom.keys.PartitionKey
import com.vz.nsp.parking.config.CassandraConfig
import com.vz.nsp.parking.model.WhatIfJob

import scala.concurrent.Future

abstract class WhatIfJobTable extends Table[WhatIfJobTable, WhatIfJob] {

  override def tableName: String = CassandraConfig.whatIfTableName

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

}
