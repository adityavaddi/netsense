package com.verizon.netsense.graph

import akka.NotUsed
import akka.actor.ActorSystem
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer}
import akka.stream.scaladsl.{Broadcast, Flow, GraphDSL, Keep, Partition, RunnableGraph, Sink}
import akka.stream.{ActorAttributes, ActorMaterializer, ActorMaterializerSettings, ClosedShape}
import com.datastax.driver.core.utils.UUIDs
import com.fasterxml.jackson.core.JsonParseException
import com.outworkers.phantom.connectors.CassandraConnection
import com.verizon.netsense.config.OTAKafkaConfig
import com.verizon.netsense.database._
import com.verizon.netsense.entity._
import com.verizon.netsense.helper.StreamHelper
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import nl.grons.metrics.scala.Counter
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.ProducerRecord
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods

import scala.concurrent.{ExecutionContext, Future}

object DSResGraphData {

  case class DataFlow(job: Option[OTAJobInfo],
                      nodes: List[Node],
                      status: Option[String] = None,
                      error: Boolean = false)

  case class JobList(jobs: List[Job])
}

case class DSResGraph(cassConn: CassandraConnection)
  extends Logging
    with StreamHelper
    with Constants
    with JsonMethods
    with Instrumented {

  import DSResGraphData._

  implicit val system = ActorSystem.create("Device-Service-Res-Graph-system")
  implicit val materializer = ActorMaterializer(ActorMaterializerSettings(system))
  implicit val ec = ExecutionContext.Implicits.global
  implicit val formats = DefaultFormats

  val parallelism = 1000

  val errCounter: Counter = metrics.counter("errors")

  val setupConsumerSettings = configKafkaConsumerSettings(
    OTAKafkaConfig.bootStrapServers,
    OTAKafkaConfig.deviceServiceGroupId,
    new ByteArrayDeserializer,
    new ByteArrayDeserializer
  )

  val setupProducerSettings = configKafkaProducerSettings(
    OTAKafkaConfig.bootStrapServers,
    new ByteArraySerializer,
    new ByteArraySerializer)

  val KafkaAPIReqSrc = configKafkaSource(setupConsumerSettings, Set(OTAKafkaConfig.deviceServiceResTopic))

  val FromJSONFlow: Flow[ConsumerRecord[Array[Byte], Array[Byte]], DSRes, NotUsed] =
    Flow[ConsumerRecord[Array[Byte], Array[Byte]]]
      .mapAsync(parallelism) {
        record => {
          Future {
            val raw = fromJSONByteArrayToObj[DSResRaw](record.value())
            DSRes(messageid = raw.messageid, response = parse(raw.response))
          }.recover {
            case ex@(_: JsonParseException) =>
              log.error("Unable to parse the element: " + new String(record.value(), "UTF-8") + " " + ex)
              throw ex
          }
        }
      }
      .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val GetJobInfoFlow: Flow[DSRes, DataFlow, NotUsed] =
    Flow[DSRes]
      .mapAsync(parallelism) {
        res => {
          val jobid: String = res.messageid

          log.info(s"getting job $jobid")

          val result = OTAJobInfoDB(cassConn).model.get(jobid)

          val siteResult: List[Map[String, Any]] = (res.response \ "items").extractOrElse[List[Map[String, Any]]](List())
          val groupResult: List[Map[String, Any]] = (res.response \ "group" \ "nodes").extractOrElse[List[Map[String, Any]]](List())
          val nodeResult = (res.response \ "node" \ "nodeid").extractOrElse("").nonEmpty

          val nodes: List[Node] = if (siteResult.nonEmpty) {
            siteResult.map(n => {
              Node(nodeid = n.getOrElse("nodeid", "unknown").toString, model = n.getOrElse("model", "unknown").toString)
            })
          } else if (groupResult.nonEmpty) {
            groupResult.map(n => {
              Node(nodeid = n.getOrElse("nodeid", "unknown").toString, model = n.getOrElse("model", "unknown").toString)
            })
          } else if (nodeResult) {
            val nodeid: String = (res.response \ "node" \ "nodeid").extractOrElse("unknown")
            val model: String = (res.response \ "node" \ "model").extractOrElse("unknown")

            List(Node(nodeid = nodeid, model = model))
          } else {
            log.warn(s"No nodes found in response ${res.response}")
            List()
          }

          val success: Boolean = if (nodes.isEmpty) false else (res.response \ "success").extractOrElse[Boolean](false)

          result.map({
            r => {
              DataFlow(job = r.headOption, nodes = nodes, error = !success)
            }
          })
        }
      }
      .withAttributes(ActorAttributes.supervisionStrategy(cassandraDecider))

  val ResPartition: Partition[DataFlow] =
    Partition[DataFlow](3, data => {
      if (data.job.isEmpty) {
        0
      } else if (data.error) {
        1
      } else {
        2
      }
    })

  val FilterResError: Flow[DataFlow, DataFlow, NotUsed] =
    Flow[DataFlow]
      .filter(_.error)
      .map(data => {
        errCounter += 1
        data.copy(status = Some(JOB_DS_ERR))
      })
      .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val FilterJobStatusFlow: Flow[DataFlow, DataFlow, NotUsed] =
    Flow[DataFlow]
      .mapAsync(parallelism) {
        data => {
          val job = data.job.get
          val jobid: String = job.jobid
          val result = OTAJobStatusDB(cassConn).model.get(jobid)

          result.map({
            r => {
              val status = r.last.status

              log.info(s"checking status for ${jobid}")

              status match {
                case JOB_INIT => log.info(s"job ${jobid} is in good state continuing")
                case _ => log.warn(s"job ${jobid} is in wrong state (${status}) stopping here")
              }

              data.copy(status = Some(status))
            }
          })
        }
      }
      .filter(_.status.get == JOB_INIT)
      .map(data => {
        data.copy(status = Some(JOB_STARTED))
      })
      .withAttributes(ActorAttributes.supervisionStrategy(cassandraDecider))

  val DivideJobFlow: Flow[DataFlow, Job, NotUsed] =
    Flow[DataFlow]
      .filter(_.nodes.nonEmpty)
      .map(data => {
        val nodeids = data.nodes
        val job = data.job.get

        val jobs: List[Job] =
          nodeids.map(node => {
            Job(jobid = job.jobid, nodeid = node.nodeid, model = node.model, firmwareid = job.firmwareid)
          })

        JobList(jobs)
      })
      .mapConcat(l => l.jobs)

  val JobKafkaSink = Flow[Job]
    .mapAsync(parallelism) {
      job =>
        Future {
          log.info(s"sending ${job} to ${OTAKafkaConfig.otaTopic}")

          new ProducerRecord[Array[Byte], Array[Byte]](
            OTAKafkaConfig.otaTopic,
            job.jobid.getBytes,
            job.toJSONArray
          )
        }
    }
    .toMat(configKafkaSink(_producerSettings = setupProducerSettings))(Keep.right)
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val JobStatusDB = Flow[DataFlow]
    .mapAsync(parallelism) {
      data => {
        val job = data.job.get

        val status = OTAJobStatus(
          jobid = job.jobid,
          when = UUIDs.timeBased(),
          status = data.status.getOrElse(JOB_STARTED)
        )

        log.info(s"writing ${status.toString} to OTAJobStatus")

        OTAJobStatusDB(cassConn).model.store(status)
      }
    }
    .withAttributes(ActorAttributes.supervisionStrategy(cassandraDecider))

  val JobInfoDB = Flow[DataFlow]
    .filter(_.nodes.nonEmpty)
    .mapAsync(parallelism) {
      data => {
        val job = data.job.get
        val count: Int = data.nodes.size

        log.info(s"updating node count to ${count.toString} for job ${job.jobid}")

        OTAJobInfoDB(cassConn).model.updateCount(job.jobid, job.when, count)
      }
    }
    .withAttributes(ActorAttributes.supervisionStrategy(cassandraDecider))

  val graph = RunnableGraph.fromGraph(GraphDSL.create() { implicit builder: GraphDSL.Builder[NotUsed] =>
    import GraphDSL.Implicits._

    val kafkaAPIReq = builder.add(KafkaAPIReqSrc).out

    val fromJSON = builder.add(FromJSONFlow)
    val getJobInfo = builder.add(GetJobInfoFlow)
    val resPartition = builder.add(ResPartition)
    val filterResError = builder.add(FilterResError)
    val filterJobStatus = builder.add(FilterJobStatusFlow)
    val bcast = builder.add(Broadcast[DataFlow](3))
    val divideJob = builder.add(DivideJobFlow)
    val jobStatusErrDb = builder.add(JobStatusDB)
    val jobStatusSucDb = builder.add(JobStatusDB)
    val jobInfoDb = builder.add(JobInfoDB)

    val jobKafka = builder.add(JobKafkaSink).in
    val ignore0 = builder.add(Sink.ignore).in
    val ignore1 = builder.add(Sink.ignore).in
    val ignore2 = builder.add(Sink.ignore).in
    val ignore3 = builder.add(Sink.ignore).in

    kafkaAPIReq ~> fromJSON ~> getJobInfo ~> resPartition.in
    resPartition.out(0) ~> ignore0
    resPartition.out(1) ~> filterResError ~> jobStatusErrDb ~> ignore1
    resPartition.out(2) ~> filterJobStatus ~> bcast ~> divideJob ~> jobKafka
    bcast ~> jobStatusSucDb ~> ignore2
    bcast ~> jobInfoDb ~> ignore3

    ClosedShape
  })
}
