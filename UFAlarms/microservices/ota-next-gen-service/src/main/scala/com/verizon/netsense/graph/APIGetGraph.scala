package com.verizon.netsense.graph

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.{Broadcast, Flow, GraphDSL, Merge, Partition}
import akka.stream.{ActorAttributes, ActorMaterializer, ActorMaterializerSettings, FlowShape}
import com.datastax.driver.core.utils.UUIDs
import com.outworkers.phantom.connectors.CassandraConnection
import com.verizon.netsense.database._
import com.verizon.netsense.entity._
import com.verizon.netsense.helper.{OTAFirmware, StreamHelper}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import nl.grons.metrics.scala.{Counter, Meter}
import org.joda.time.{DateTime, DateTimeZone}

import scala.concurrent.{ExecutionContext, Future}

object APIGetGraphData {
  val parallelism = 1000

  val types: List[String] = List("otaStatusForSite", "otaStatusForJob", "getAllFirmwares", "getFirmware")

  case class GetJobsFlowData(apiData: APIDataFlow, relations: List[OTAJobRelation])

  case class GetJobFlowData(apiData: APIDataFlow,
                            info: Option[OTAJobInfoConverted] = None,
                            status: List[OTAJobStatusConverted] = List())

}

case class APIGetGraph(otaFW: OTAFirmware, cassConn: CassandraConnection)
  extends Logging
    with StreamHelper
    with Constants
    with Instrumented {

  import APIGetGraphData._

  implicit val system = ActorSystem.create("API-Assign-Get-system")
  implicit val materializer = ActorMaterializer(ActorMaterializerSettings(system))
  implicit val ec = ExecutionContext.Implicits.global

  val meters: List[Meter] = types.map(t => {
    metrics.meter(t, "api-request")
  })
  val errCounter: Counter = metrics.counter("errors")

  val ReqTypePartition: Partition[APIDataFlow] =
    Partition[APIDataFlow](types.length, data => {
      val req: OTAReq = data.req

      log.info(s"got request ${req.`type`}")

      val index = types.indexOf(req.`type`)

      meters(index).mark()

      index
    })

  val GetJobsForSiteFlow: Flow[APIDataFlow, APIDataFlow, NotUsed] =
    Flow[APIDataFlow]
      .mapAsync(parallelism) {
        data => {
          val req: OTAReq = data.req
          val orgid: String = req.orgprops.orgid
          val siteid: String = req.siteprops.siteid

          log.info(s"getting jobs for $orgid -> $siteid")
          val result = OTAJobRelationDB(cassConn).model.get(orgid, siteid)

          result.map(relations => {
            GetJobsFlowData(apiData = data, relations = relations)
          })
        }
      }
      .mapAsync(parallelism) {
        data => {
          val apiData: APIDataFlow = data.apiData

          if (data.relations.isEmpty) {
            Future {
              val res: OTARes = OTARes(success = true, data = Some(List()))
              apiData.copy(res = Some(res))
            }

          } else {
            val result = Future.traverse(data.relations)(relation => {
              OTAJobInfoDB(cassConn).model.get(relation.jobid)
            })

            result.map(infos => {
              val infos1: List[OTAJobInfoConverted] =
                infos.flatten.map(info => {
                  val fwID: String = info.firmwareid
                  val model: String = otaFW.getTypeFromFwID(fwID)

                  OTAJobInfoConverted(
                    jobid = info.jobid,
                    firmwareid = fwID,
                    targetid = info.targetid,
                    targettype = info.targettype,
                    model = model,
                    count = info.count,
                    description = info.description,
                    when = new DateTime(UUIDs.unixTimestamp(info.when)).toDateTime(DateTimeZone.UTC).toString()
                  )
                })

              val res: OTARes = OTARes(success = true, data = Some(infos1))
              apiData.copy(res = Some(res))
            })
          }
        }
      }
      .withAttributes(ActorAttributes.supervisionStrategy(cassandraDecider))

  val GetJobFlow: Flow[APIDataFlow, GetJobFlowData, NotUsed] =
    Flow[APIDataFlow]
      .mapAsync(parallelism) {
        data => {
          val req: OTAReq = data.req
          val jobid: String = req.otaprops.jobid

          log.info(s"getting job ($jobid)")

          val result = OTAJobInfoDB(cassConn).model.get(jobid)

          result.map({
            r => {
              val r1 = r.headOption

              val info: Option[OTAJobInfoConverted] = if (r1.isDefined) {
                val fwID: String = r1.get.firmwareid
                val model: String = otaFW.getTypeFromFwID(fwID)

                Some(OTAJobInfoConverted(
                  jobid = r1.get.jobid,
                  firmwareid = fwID,
                  targetid = r1.get.targetid,
                  targettype = r1.get.targettype,
                  model = model,
                  count = r1.get.count,
                  description = r1.get.description,
                  when = new DateTime(UUIDs.unixTimestamp(r1.get.when)).toDateTime(DateTimeZone.UTC).toString()
                ))
              } else {
                None
              }

              GetJobFlowData(apiData = data, info = info)
            }
          })
        }
      }
      .withAttributes(ActorAttributes.supervisionStrategy(cassandraDecider))

  val GetJobErrorFlow: Flow[GetJobFlowData, APIDataFlow, NotUsed] =
    Flow[GetJobFlowData]
      .filter(_.info.isEmpty)
      .map(data => {
        errCounter += 1
        val req: OTAReq = data.apiData.req
        val jobID: String = req.otaprops.jobid
        val res: OTARes = OTARes(
          success = false,
          error = Some(OTAResError(status = 404, message = s"Job ID ${jobID} not found"))
        )
        data.apiData.copy(res = Some(res))
      })
      .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val GetJobInfoFlow: Flow[GetJobFlowData, APIDataFlow, NotUsed] =
    Flow[GetJobFlowData]
      .filter(_.info.isDefined)
      .mapAsync(parallelism) {
        data => {
          val req: OTAReq = data.apiData.req
          val jobid: String = req.otaprops.jobid

          log.info(s"getting job status for $jobid")

          val result = OTAJobStatusDB(cassConn).model.get(jobid)

          result.map({
            r => {
              val r1 = r.map(jobStatus => {
                OTAJobStatusConverted(
                  jobid = jobStatus.jobid,
                  status = jobStatus.status,
                  when = new DateTime(UUIDs.unixTimestamp(jobStatus.when)).toDateTime(DateTimeZone.UTC).toString()
                )
              })
              data.copy(status = r1)
            }
          })
        }
      }
      .mapAsync(parallelism) {
        data => {
          val req: OTAReq = data.apiData.req
          val jobid: String = req.otaprops.jobid

          log.info(s"getting nodes status for ($jobid)")

          val result = OTANodeStatusDB(cassConn).model.get(jobid)

          result.map({
            r => {
              val r1 = r.map(nodeStatus => {
                OTANodeStatusConverted(
                  jobid = nodeStatus.jobid,
                  nodeid = nodeStatus.nodeid,
                  status = nodeStatus.status,
                  progress = nodeStatus.progress,
                  when = new DateTime(UUIDs.unixTimestamp(nodeStatus.when)).toDateTime(DateTimeZone.UTC).toString()
                )
              }).groupBy(_.nodeid)

              val resMap: Map[String, Any] = Map(
                "job_info" -> data.info.get,
                "job_status" -> data.status,
                "nodes_status" -> r1
              )

              val res = OTARes(success = true, data = Some(resMap))
              data.apiData.copy(res = Some(res))
            }
          })
        }
      }
      .withAttributes(ActorAttributes.supervisionStrategy(cassandraDecider))

  val GetFirmwares: Flow[APIDataFlow, APIDataFlow, NotUsed] =
    Flow[APIDataFlow]
      .mapAsync(parallelism) {
        data => {
          Future {
            val results = otaFW.getFirmwares().map(f => {
              Map(
                "firmwareid" -> f.firmwareid,
                "release" -> f.version,
                "type" -> f.`type`,
                "when" -> new DateTime(f.when).toDateTime(DateTimeZone.UTC).toString(),
                "image_size" -> f.size,
                "name" -> s"${f.`type`}_${f.version}_${f.commitid}"
              )
            })

            val res = OTARes(success = true, data = Some(results))
            data.copy(res = Some(res))
          }
        }
      }
      .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val GetFirmware: Flow[APIDataFlow, APIDataFlow, NotUsed] =
    Flow[APIDataFlow]
      .mapAsync(parallelism) {
        data => {
          Future {
            val fwID: String = data.req.firmwareprops.firmwareid

            val results = otaFW.getFirmwares()
              .filter(_.firmwareid == fwID)
              .map(f => {
                Map(
                  "firmwareid" -> f.firmwareid,
                  "release" -> f.version,
                  "type" -> f.`type`,
                  "when" -> new DateTime(f.when).toDateTime(DateTimeZone.UTC).toString(),
                  "image_size" -> f.size,
                  "name" -> s"${f.`type`}_${f.version}_${f.commitid}"
                )
              })

            val res = results.headOption match {
              case Some(firmware: Map[String, any]) =>
                OTARes(success = true, data = Some(firmware))
              case _ =>
                OTARes(
                  success = false,
                  error = Some(OTAResError(status = 404, message = s"Firmware ID ${fwID} not found"))
                )
            }

            data.copy(res = Some(res))
          }
        }
      }
      .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))


  val graph = Flow.fromGraph(GraphDSL.create() {
    implicit builder: GraphDSL.Builder[NotUsed] =>

      import GraphDSL.Implicits._

      val reqType = builder.add(ReqTypePartition)
      val mergeOut = builder.add(Merge[APIDataFlow](types.length + 1))
      val getJobsForSite = builder.add(GetJobsForSiteFlow)
      val getJob = builder.add(GetJobFlow)
      val bcast = builder.add(Broadcast[GetJobFlowData](2))
      val getJobError = builder.add(GetJobErrorFlow)
      val getJobInfo = builder.add(GetJobInfoFlow)
      val getFirmwares = builder.add(GetFirmwares)
      val getFirmware = builder.add(GetFirmware)

      reqType.in

      reqType.out(0) ~> getJobsForSite ~> mergeOut
      reqType.out(1) ~> getJob ~> bcast ~> getJobInfo ~> mergeOut
      bcast ~> getJobError ~> mergeOut
      reqType.out(2) ~> getFirmwares ~> mergeOut
      reqType.out(3) ~> getFirmware ~> mergeOut

      FlowShape(reqType.in, mergeOut.out)
  })


}
