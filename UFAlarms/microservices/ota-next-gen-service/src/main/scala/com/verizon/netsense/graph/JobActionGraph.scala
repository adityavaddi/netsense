package com.verizon.netsense.graph

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.{Broadcast, Flow, GraphDSL, Partition, Sink}
import akka.stream.{ActorAttributes, ActorMaterializer, ActorMaterializerSettings, FlowShape}
import com.datastax.driver.core.utils.UUIDs
import com.outworkers.phantom.connectors.CassandraConnection
import com.verizon.netsense.database._
import com.verizon.netsense.entity._
import com.verizon.netsense.helper.{JobHelper, StreamHelper}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import nl.grons.metrics.scala.Counter

import scala.concurrent.ExecutionContext

object JobActionGraphData {

  case class DataFlow(state: String, data: APIDataFlow)

}

case class JobActionGraph(cassConn: CassandraConnection)
  extends Logging
    with StreamHelper
    with Constants
    with Instrumented {

  import JobActionGraphData._

  implicit val system = ActorSystem.create("Job-Action-Graph-system")
  implicit val materializer = ActorMaterializer(ActorMaterializerSettings(system))
  implicit val ec = ExecutionContext.Implicits.global

  val parallelism = 1000

  val errCounter: Counter = metrics.counter("errors")

  val types: List[String] = List("stop")

  val ReqTypePartition: Partition[APIDataFlow] =
    Partition[APIDataFlow](types.length, data => {
      val req: OTAReq = data.req

      log.info(s"got request ${req.otaprops.action}")

      types.indexOf(req.otaprops.action)
    })

  val IsJobTerminatedFlow: Flow[APIDataFlow, DataFlow, NotUsed] =
    Flow[APIDataFlow]
      .mapAsync(parallelism) {
        data => {
          val jobId: String = data.req.otaprops.jobid

          JobHelper(cassConn).jobLastStatus(jobId).map(state => {

            log.info(s"Job ${jobId} is ${state}")

            DataFlow(state = state, data = data)
          })
        }
      }
      .withAttributes(ActorAttributes.supervisionStrategy(cassandraDecider))


  val APIRespFlow: Flow[DataFlow, APIDataFlow, NotUsed] =
    Flow[DataFlow]
      .map(data => {
        val apiData: APIDataFlow = data.data
        val jobId: String = apiData.req.otaprops.jobid
        val res: OTARes = data.state match {
          case JOB_STOPPED =>
            errCounter += 1
            OTARes(
              success = false,
              error = Some(OTAResError(status = 400, message = s"Job ${jobId} is already stopped"))
            )
          case JOB_DONE =>
            errCounter += 1
            OTARes(
              success = false,
              error = Some(OTAResError(status = 400, message = s"Job ${jobId} is already terminated"))
            )
          case JOB_404 =>
            errCounter += 1
            OTARes(
              success = false,
              error = Some(OTAResError(status = 404, message = s"Job ${jobId} not found"))
            )
          case _ =>
            OTARes(
              success = true
            )
        }

        apiData.copy(res = Some(res))
      })
      .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val JobStatusDB = Flow[DataFlow]
    .filterNot(data => {
      JobHelper(cassConn).terminatedState(data.state)
    })
    .mapAsync(parallelism) {
      data => {
        val apiData: APIDataFlow = data.data
        val jobId: String = apiData.req.otaprops.jobid
        val status = OTAJobStatus(
          jobid = jobId,
          when = UUIDs.timeBased(),
          status = JOB_STOPPED
        )

        log.info(s"writing ${status.toString} to OTAJobStatus")
        OTAJobStatusDB(cassConn).model.store(status)
      }
    }
    .withAttributes(ActorAttributes.supervisionStrategy(cassandraDecider))

  val graph = Flow.fromGraph(GraphDSL.create() {
    implicit builder: GraphDSL.Builder[NotUsed] =>

      import GraphDSL.Implicits._

      val reqType = builder.add(ReqTypePartition)

      val isJobTerminated = builder.add(IsJobTerminatedFlow)
      val bcast = builder.add(Broadcast[DataFlow](2))
      val jobStatusDb = builder.add(JobStatusDB)
      val apiResp = builder.add(APIRespFlow)

      val ignore = builder.add(Sink.ignore).in

      reqType.in
      reqType.out(0) ~> isJobTerminated ~> bcast ~> apiResp
      bcast ~> jobStatusDb ~> ignore

      FlowShape(reqType.in, apiResp.out)
  })


}
