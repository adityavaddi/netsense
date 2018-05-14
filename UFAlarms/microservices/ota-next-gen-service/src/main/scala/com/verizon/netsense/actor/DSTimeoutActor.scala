package com.verizon.netsense.actor

import akka.actor.{Actor, Props}
import com.datastax.driver.core.utils.UUIDs
import com.outworkers.phantom.connectors.CassandraConnection
import com.verizon.netsense.database._
import com.verizon.netsense.entity.{APIDataFlow, Constants, OTAJobStatus}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import nl.grons.metrics.scala.Counter

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object DSTimeoutActor {
  def props(data: APIDataFlow, cassConn: CassandraConnection): Props = Props(new DSTimeoutActor(data, cassConn))
}

class DSTimeoutActor(data: APIDataFlow, cassConn: CassandraConnection)
  extends Actor
    with Logging
    with Constants
    with Instrumented {

  implicit val ec = ExecutionContext.Implicits.global

  val errCounter: Counter = metrics.counter("errors")

  override def preStart(): Unit = {
    context.system.scheduler.scheduleOnce(60 seconds, self, "timeout")
    log.info(s"DSTimeoutActor started with ${data.toString()} will check in 60s")
  }

  override def postStop(): Unit = log.info("DSTimeoutActor stopped")

  override def receive: Receive = {
    case "timeout" =>
      log.info("Timeout reached")

      val result = OTAJobStatusDB(cassConn).model.get(data.id)

      result.onComplete({
        case Success(status) => {
          val latest = status.last.status

          latest match {
            case JOB_STARTED => log.info("Job was started")
            case JOB_DONE => log.info("Job already done")
            case other =>
              errCounter += 1
              log.warn(s"job has not started yet (${other})")

              val status = OTAJobStatus(
                jobid = data.id,
                when = UUIDs.timeBased(),
                status = JOB_TIMEOUT
              )
              log.info(s"writing ${status.toString} to OTAJobStatus")
              OTAJobStatusDB(cassConn).model.store(status)
          }
        }
        case Failure(exception) => {
          log.error(exception.toString)
        }
      })

      context.stop(self)
  }

}
