package com.verizon.netsense.helper

import com.outworkers.phantom.connectors.CassandraConnection
import com.verizon.netsense.database._
import com.verizon.netsense.entity.Constants

import scala.concurrent.{ExecutionContext, Future}

case class JobHelper(cassConn: CassandraConnection) extends Constants {

  implicit val ec = ExecutionContext.Implicits.global

  def jobLastStatus(jobId: String): Future[String] = {
    val result = OTAJobStatusDB(cassConn).model.get(jobId)

    result.map(data => {
      if (data.nonEmpty) {
        data.sortBy(_.when).last.status
      } else {
        JOB_404
      }
    })
  }

  def terminatedState(state: String): Boolean = {
    state match {
      case JOB_STOPPED => true
      case JOB_DONE => true
      case JOB_404 => true
      case JOB_TIMEOUT => true
      case JOB_DS_ERR => true
      case FW_NOT_FOUND => true
      case _ => false
    }
  }
}
