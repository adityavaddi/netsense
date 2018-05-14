package com.verizon.netsense.graph

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.{Broadcast, Flow, GraphDSL, Keep, Sink}
import akka.stream.{ActorAttributes, ActorMaterializer, ActorMaterializerSettings, FlowShape}
import com.datastax.driver.core.utils.UUIDs
import com.outworkers.phantom.connectors.CassandraConnection
import com.verizon.netsense.actor.DSTimeoutActor
import com.verizon.netsense.config.OTAKafkaConfig
import com.verizon.netsense.database._
import com.verizon.netsense.entity._
import com.verizon.netsense.helper.{OTAFirmware, StreamHelper}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import nl.grons.metrics.scala.Counter
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.ByteArraySerializer

import scala.concurrent.{ExecutionContext, Future}

case class APIAssignGraph(otaFW: OTAFirmware, cassConn: CassandraConnection)
  extends Logging
    with StreamHelper
    with Constants
    with Instrumented {

  implicit val system = ActorSystem.create("API-Assign-Graph-system")
  implicit val materializer = ActorMaterializer(ActorMaterializerSettings(system))
  implicit val ec = ExecutionContext.Implicits.global

  val parallelism = 1000

  val errCounter: Counter = metrics.counter("errors")

  val setupProducerSettings = configKafkaProducerSettings(
    OTAKafkaConfig.bootStrapServers,
    new ByteArraySerializer,
    new ByteArraySerializer)

  val CheckFirmware: Flow[APIDataFlow, APIDataFlow, NotUsed] =
    Flow[APIDataFlow]
      .mapAsync(parallelism) {
        data => {
          Future {
            val firmwareId: String = data.req.firmwareprops.firmwareid
            val results = otaFW.getFirmwares()

            val firmwares = results.filter(r => {
              r.firmwareid == firmwareId
            })

            if (firmwares.isEmpty) {
              errCounter += 1
              log.warn(s"no firmware found for commitid=${firmwareId}")
              val err: OTAResError = OTAResError(
                status = 404,
                message = FW_NOT_FOUND
              )
              data.copy(error = Some(err))
            } else {
              data
            }
          }
        }
      }
      .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val CreateResp: Flow[APIDataFlow, APIDataFlow, NotUsed] =
    Flow[APIDataFlow]
      .map(data => {
        if (data.error.isDefined) {
          val res: OTARes = OTARes(
            success = false,
            error = Some(data.error.get)
          )
          data.copy(res = Some(res))
        } else {
          val res: OTARes = OTARes(
            success = true,
            data = Some(Map("jobid" -> data.id))
          )
          data.copy(res = Some(res))
        }

      })
      .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val FilterError: Flow[APIDataFlow, APIDataFlow, NotUsed] =
    Flow[APIDataFlow]
      .filterNot(_.error.isDefined)

  val JobInfoSink = Flow[APIDataFlow]
    .mapAsync(parallelism) {
      data => {
        val req = data.req

        val targettype: String = req.`type`.split("assignFirmwareTo").last.toLowerCase
        val targetid: String = targettype match {
          case "group" => req.groupprops.groupid
          case "site" => req.siteprops.siteid
          case "node" => req.nodeprops.nodeid
        }

        val description: String = if (req.otaprops.description == null) "" else req.otaprops.description

        val info = OTAJobInfo(
          jobid = data.id,
          firmwareid = req.firmwareprops.firmwareid,
          targetid = targetid,
          targettype = targettype,
          count = 0,
          description = description,
          when = UUIDs.timeBased()
        )
        log.info(s"writing ${info.toString} to OTAJobInfo")
        OTAJobInfoDB(cassConn).model.store(info)
      }
    }
    .toMat(Sink.ignore)(Keep.right)
    .withAttributes(ActorAttributes.supervisionStrategy(cassandraDecider))

  val JobStatusSink = Flow[APIDataFlow]
    .mapAsync(parallelism) {
      data => {
        val status = OTAJobStatus(
          jobid = data.id,
          when = UUIDs.timeBased(),
          status = JOB_INIT
        )
        log.info(s"writing ${status.toString} to OTAJobStatus")
        OTAJobStatusDB(cassConn).model.store(status)
      }
    }
    .toMat(Sink.ignore)(Keep.right)
    .withAttributes(ActorAttributes.supervisionStrategy(cassandraDecider))

  val JobRelationSink = Flow[APIDataFlow]
    .mapAsync(parallelism) {
      data => {
        val r = OTAJobRelation(
          orgid = data.req.orgprops.orgid,
          siteid = data.req.siteprops.siteid,
          jobid = data.id,
          when = UUIDs.timeBased()
        )
        log.info(s"writing ${r.toString} to OTAJobRelation")
        OTAJobRelationDB(cassConn).model.store(r)
      }
    }
    .toMat(Sink.ignore)(Keep.right)
    .withAttributes(ActorAttributes.supervisionStrategy(cassandraDecider))

  val DeviceServiceReq = Flow[APIDataFlow]
    .mapAsync(parallelism) {
      data =>
        Future {

          val targettype: String = data.req.`type`.split("assignFirmwareTo").last.toLowerCase

          val req: OTAReq = targettype match {
            case "site" => data.req.copy(`type` = "getAllNodesForSite", model = "SiteModel", action = "CAN_READ")
            case "group" => data.req.copy(`type` = "getGroup", model = "GroupModel", action = "CAN_READ")
            case "node" => data.req.copy(`type` = "getNode", model = "NodeModel", action = "CAN_READ")
          }

          val env: Envelope = Envelope(messageid = data.id, responsetopic = OTAKafkaConfig.deviceServiceResTopic, request = req)

          new ProducerRecord[Array[Byte], Array[Byte]](
            OTAKafkaConfig.deviceServiceReqTopic,
            data.id.getBytes,
            env.toJSONArray
          )
        }
    }
    .toMat(configKafkaSink(_producerSettings = setupProducerSettings))(Keep.right)
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val TimeoutSink = Flow[APIDataFlow]
    .mapAsync(parallelism) {
      data => {
        Future {
          val actor = system.actorOf(DSTimeoutActor.props(data, cassConn), data.id)
        }
      }
    }
    .toMat(Sink.ignore)(Keep.right)
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))


  val graph = Flow.fromGraph(GraphDSL.create() { implicit builder: GraphDSL.Builder[NotUsed] =>
    import GraphDSL.Implicits._

    val checkFirmware = builder.add(CheckFirmware)
    val bcast = builder.add(Broadcast[APIDataFlow](2))
    val bcastSucess = builder.add(Broadcast[APIDataFlow](5))
    val createResp = builder.add(CreateResp)
    val filterError = builder.add(FilterError)
    val jobInfoDb = builder.add(JobInfoSink).in
    val jobStatusDb = builder.add(JobStatusSink).in
    val jobRelationDb = builder.add(JobRelationSink).in
    val deviceServiceReq = builder.add(DeviceServiceReq).in
    val timeout = builder.add(TimeoutSink).in

    checkFirmware ~> bcast ~> createResp
    bcast ~> filterError ~> bcastSucess
    bcastSucess ~> deviceServiceReq
    bcastSucess ~> jobInfoDb
    bcastSucess ~> jobStatusDb
    bcastSucess ~> jobRelationDb
    bcastSucess ~> timeout

    FlowShape(checkFirmware.in, createResp.out)
  })


}
