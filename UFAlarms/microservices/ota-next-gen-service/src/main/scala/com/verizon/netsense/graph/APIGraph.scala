package com.verizon.netsense.graph

import java.util.UUID

import akka.NotUsed
import akka.actor.ActorSystem
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer}
import akka.stream.scaladsl.{Broadcast, Flow, GraphDSL, Keep, Merge, Partition, RunnableGraph, Sink}
import akka.stream.{ActorAttributes, ActorMaterializer, ActorMaterializerSettings, ClosedShape}
import com.fasterxml.jackson.core.JsonParseException
import com.outworkers.phantom.connectors.CassandraConnection
import com.verizon.netsense.config.OTAKafkaConfig
import com.verizon.netsense.entity._
import com.verizon.netsense.helper.{OTAFirmware, StreamHelper}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import nl.grons.metrics.scala.{Counter, Meter}
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.ProducerRecord

import scala.concurrent.{ExecutionContext, Future}

case class APIGraph(otaFW: OTAFirmware, cassConn: CassandraConnection)
  extends Logging
    with StreamHelper
    with Constants
    with Instrumented {

  implicit val system = ActorSystem.create("API-Graph-system")
  implicit val materializer = ActorMaterializer(ActorMaterializerSettings(system))
  implicit val ec = ExecutionContext.Implicits.global

  val parallelism = 1000
  val reqTypes = 4

  val reqMeter: Meter = metrics.meter("api-request")
  val errCounter: Counter = metrics.counter("errors")

  val setupConsumerSettings = configKafkaConsumerSettings(
    OTAKafkaConfig.bootStrapServers,
    OTAKafkaConfig.apiGroupId,
    new ByteArrayDeserializer,
    new ByteArrayDeserializer
  )

  val setupProducerSettings = configKafkaProducerSettings(
    OTAKafkaConfig.bootStrapServers,
    new ByteArraySerializer,
    new ByteArraySerializer)

  val KafkaAPIReqSrc = configKafkaSource(setupConsumerSettings, Set(OTAKafkaConfig.apiReqTopic))

  val FromJSONFlow: Flow[ConsumerRecord[Array[Byte], Array[Byte]], APIDataFlow, NotUsed] =
    Flow[ConsumerRecord[Array[Byte], Array[Byte]]]
      .mapAsync(parallelism) {
        record => {
          reqMeter.mark()

          Future {
            val env: Envelope = fromJSONByteArrayToObj[Envelope](record.value())

            APIDataFlow(
              id = UUID.randomUUID().toString,
              envelope = env,
              req = env.request
            )
          }.recover {
            case ex@(_: JsonParseException) =>
              log.error("Unable to parse the element: " + new String(record.value(), "UTF-8") + " " + ex)
              throw ex
          }
        }
      }
      .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val ReqTypePartition: Partition[APIDataFlow] =
    Partition[APIDataFlow](reqTypes, data => {
      val req: OTAReq = data.req

      req.`type` match {
        case "assignFirmwareToSite" => 1
        case "assignFirmwareToGroup" => 1
        case "assignFirmwareToNode" => 1
        case "otaStatusForSite" => 2
        case "otaStatusForJob" => 2
        case "getAllFirmwares" => 2
        case "getFirmware" => 2
        case "otaStatusJobUpdate" => 3
        case _ => 0
      }
    })

  val ReqErrorFlow: Flow[APIDataFlow, APIDataFlow, NotUsed] =
    Flow[APIDataFlow]
      .map(data => {
        errCounter += 1
        val req: OTAReq = data.req
        val res: OTARes = OTARes(
          success = false,
          error = Some(OTAResError(status = 400, message = s"Unsupported req type ${req.`type`}"))
        )

        data.copy(res = Some(res))
      })
      .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val PrintFlow: Flow[Any, Any, NotUsed] =
    Flow[Any]
      .map(x => {
        log.info(s"Received req ${x.toString()}")
        x
      })
      .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val APIResSink = Flow[APIDataFlow]
    .mapAsync(parallelism) {
      data =>
        Future {
          val resp: RespEnvelope = RespEnvelope(
            messageid = data.envelope.messageid,
            response = data.res.get
          )

          log.info(s"sending ${resp} to ${data.envelope.responsetopic}")

          new ProducerRecord[Array[Byte], Array[Byte]](
            data.envelope.responsetopic,
            data.envelope.messageid.getBytes,
            resp.toJSONArray
          )
        }
    }
    .toMat(configKafkaSink(_producerSettings = setupProducerSettings))(Keep.right)
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))


  val graph = RunnableGraph.fromGraph(GraphDSL.create() { implicit builder: GraphDSL.Builder[NotUsed] =>
    import GraphDSL.Implicits._

    val kafkaAPIReq = builder.add(KafkaAPIReqSrc).out
    val fromJSON = builder.add(FromJSONFlow)
    val bcast = builder.add(Broadcast[APIDataFlow](2))
    val print = builder.add(PrintFlow)
    val reqType = builder.add(ReqTypePartition)
    val reqError = builder.add(ReqErrorFlow)
    val mergeToAPIRes = builder.add(Merge[APIDataFlow](reqTypes))
    val APIRes = builder.add(APIResSink).in
    val ignore = builder.add(Sink.ignore).in


    kafkaAPIReq ~> fromJSON ~> bcast ~> reqType.in
    bcast ~> print ~> ignore

    reqType.out(0) ~> reqError ~> mergeToAPIRes ~> APIRes
    reqType.out(1) ~> APIAssignGraph(otaFW, cassConn).graph ~> mergeToAPIRes
    reqType.out(2) ~> APIGetGraph(otaFW, cassConn).graph ~> mergeToAPIRes
    reqType.out(3) ~> JobActionGraph(cassConn).graph ~> mergeToAPIRes

    ClosedShape
  })
}
