package com.verizon.netsense.whatifservice.db

import com.verizon.netsense.metrics.Instrumented
import nl.grons.metrics.scala.Timer

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import com.outworkers.phantom.dsl.ResultSet
import com.verizon.netsense.whatifservice.exceptions.CustomExceptions.WhatifCassandraDBException
import com.verizon.netsense.whatifservice.model.{Policy, WhatIfJob}

/**
 * Created by maleva on 3/17/18.
 */
trait PhantomService extends MyDbProvider with Instrumented {

  implicit val ex: ExecutionContextExecutor = ExecutionContext.global

  implicit private val phantomConnector     = ProductionDb.connector
  private[this] val cassandraPersist: Timer = metrics.timer("cassandra-async-persist-timer")

  def storeJobStatus(jobStatus: WhatIfJob): Future[ResultSet] = cassandraPersist.time {
    database.WhatIfJobTable.storeJobStatus(jobStatus).recover {
      case ex: Exception =>
        throw new WhatifCassandraDBException("Failed to insert data in What_If table: "
                                             + jobStatus + ex.getMessage,
                                             ex.getCause) {
          override val underlyingMessage = ""
        }
    }
  }

  def getJobById(orgid: String, siteid: String, jobid: String): Future[Option[WhatIfJob]] =
    database.WhatIfJobTable.getJobId(orgid, siteid, jobid).recover {
      case ex: Exception =>
        throw new WhatifCassandraDBException("Failed to get data from What_If table: "
                                             + ex.getMessage,
                                             ex.getCause) {
          override val underlyingMessage = ""
        }
    }

  def getAllJobs(orgid: String, siteid: String): Future[List[WhatIfJob]] = {
    val result = database.WhatIfJobTable.getAllJobs(orgid, siteid)
    result.map(e => print(e))
    result
    result.recover {
      case ex: Exception =>
        throw new WhatifCassandraDBException("Failed to get all data from What_If table: "
                                             + ex.getMessage,
                                             ex.getCause) {
          override val underlyingMessage = ""
        }
    }
  }

  def updateByJobId(orgid: String, siteid: String, jobId: String, jobStatus: WhatIfJob): Future[ResultSet] =
    cassandraPersist.time {
      database.WhatIfJobTable.updateByJobId(orgid, siteid, jobId, jobStatus).recover {
        case ex: Exception =>
          throw new WhatifCassandraDBException("Failed to update data in What_If table: "
                                               + jobStatus + ex.getMessage,
                                               ex.getCause) {
            override val underlyingMessage = ""
          }
      }
    }

  def updateWhatIfBatchId(orgid: String,
                          siteid: String,
                          jobId: String,
                          batchId: Option[Int],
                          updatedat: Option[Long]): Future[ResultSet] =
    cassandraPersist.time {
      database.WhatIfJobTable.updateWhatIfBatchId(orgid, siteid, jobId, batchId, updatedat).recover {
        case ex: Exception =>
          throw new WhatifCassandraDBException(
            s"Failed to update batchId: $batchId in What_If table for orgid: $orgid, " +
            s"siteid: $siteid and jobId: $jobId with message: "
            + ex.getMessage,
            ex.getCause
          ) {
            override val underlyingMessage = ""
          }
      }
    }

  def updateWhatIfJobStatus(orgid: String,
                            siteid: String,
                            jobId: String,
                            jobStatus: String,
                            updatedat: Option[Long],
                            additionalmessage: String): Future[ResultSet] =
    cassandraPersist.time {
      database.WhatIfJobTable
        .updateWhatIfJobStatus(orgid, siteid, jobId, Option(jobStatus), updatedat, Option(additionalmessage))
        .recover {
          case ex: Exception =>
            throw new WhatifCassandraDBException(
              s"Failed to update jobStatus: $jobStatus in What_If table for orgid: $orgid, " +
              s"siteid: $siteid and jobId: $jobId with message: "
              + jobStatus + ex.getMessage,
              ex.getCause
            ) {
              override val underlyingMessage = ""
            }
        }
    }

  def deleteByJobId(orgid: String, siteid: String, jobId: String): Future[ResultSet] = cassandraPersist.time {
    database.WhatIfJobTable.deleteByJobId(orgid, siteid, jobId).recover {
      case ex: Exception =>
        throw new WhatifCassandraDBException("Failed to delete data from What_If table: "
                                             + ex.getMessage,
                                             ex.getCause) {
          override val underlyingMessage = ""
        }
    }
  }

  def getAllPolicies(orgid: String, siteid: String): Future[List[Policy]] =
    database.policyMappingTable.getAllPolicies(orgid, siteid)

  def storePolicy(deviceMapping: Policy): Future[ResultSet] = cassandraPersist.time {
    database.policyMappingTable.storePolicy(deviceMapping)
  }
}
