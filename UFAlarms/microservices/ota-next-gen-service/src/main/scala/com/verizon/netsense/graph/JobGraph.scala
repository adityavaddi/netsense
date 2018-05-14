package com.verizon.netsense.graph

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.{ActorAttributes, ActorMaterializer, ActorMaterializerSettings, ClosedShape}
import akka.stream.scaladsl.{Broadcast, Flow, GraphDSL, Keep, RunnableGraph, Sink}
import com.datastax.driver.core.utils.UUIDs
import com.fasterxml.jackson.core.JsonParseException
import com.outworkers.phantom.connectors.CassandraConnection
import com.verizon.netsense.config.OTAKafkaConfig
import com.verizon.netsense.database._
import com.verizon.netsense.entity._
import com.verizon.netsense.helper.{JobHelper, OTAFirmware, StreamHelper}
import com.verizon.netsense.utils.{Deserializer, Logging}
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer}

import scala.concurrent.{ExecutionContext, Future}

object JobGraphData {

  case class DataFlow(job: Job,
                      error: Option[String] = None,
                      firmware: Option[Firmware] = None,
                      isTerminated: Boolean = false)

  case class DataFlowWrapper(data: DataFlow,
                             info: OTAJobInfo,
                             done: Boolean = false)

}


case class JobGraph(otaFW: OTAFirmware, cassConn: CassandraConnection) extends Logging
  with StreamHelper
  with Constants {

  import JobGraphData._

  implicit val system = ActorSystem.create("Job-Graph-system")
  implicit val materializer = ActorMaterializer(ActorMaterializerSettings(system))
  implicit val ec = ExecutionContext.Implicits.global

  val parallelism = 1000

  val setupConsumerSettings = configKafkaConsumerSettings(
    OTAKafkaConfig.bootStrapServers,
    OTAKafkaConfig.jobGroupId,
    new ByteArrayDeserializer,
    new ByteArrayDeserializer
  )

  val setupProducerSettings = configKafkaProducerSettings(
    OTAKafkaConfig.bootStrapServers,
    new ByteArraySerializer,
    new ByteArraySerializer)

  val KafkaOTAJobSrc = configKafkaSource(setupConsumerSettings, Set(OTAKafkaConfig.otaTopic))

  val JSONToJobFlow = Flow[ConsumerRecord[Array[Byte], Array[Byte]]]
    .mapAsync(parallelism) {
      record => {
        Future {
          DataFlow(job = fromJSONByteArrayToObj[Job](record.value()))
        }.recover {
          case ex@(_: JsonParseException) =>
            log.error("Unable to parse the element: " + new String(record.value(), "UTF-8") + " " + ex)
            throw ex
        }
      }
    }
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val CheckJobFlow: Flow[DataFlow, DataFlow, NotUsed] =
    Flow[DataFlow]
      .mapAsync(parallelism) {
        data => {
          val jobId: String = data.job.jobid

          JobHelper(cassConn).jobLastStatus(jobId).map(state => {
            val terminated: Boolean = JobHelper(cassConn).terminatedState(state)

            if (terminated) {
              log.warn(s"Job ${jobId} is ${state}")
            }

            data.copy(isTerminated = terminated)
          })
        }
      }
      .filterNot(_.isTerminated)
      .withAttributes(ActorAttributes.supervisionStrategy(cassandraDecider))

  val CheckFirmwareFlow: Flow[DataFlow, DataFlow, NotUsed] =
    Flow[DataFlow]
      .mapAsync(parallelism) {
        data => {
          Future {
            val job: Job = data.job
            val results = otaFW.getFirmwares()

            val firmwares = results.filter(r => {
              r.`type` == job.model && r.firmwareid == job.firmwareid
            })

            if (firmwares.isEmpty) {
              log.warn(s"no firmware found for commitid=${job.firmwareid} / model=${job.model}")
              data.copy(error = Some(FW_NOT_FOUND))
            } else {
              data.copy(firmware = Some(firmwares.head))
            }
          }
        }
      }
      .withAttributes(ActorAttributes.supervisionStrategy(cassandraDecider))

  val PrintFlow = Flow[DataFlow]
    .map(data => {
      val job: Job = data.job

      if (data.error.isDefined) {
        log.warn(s"got job ${job.jobid} for node ${job.nodeid} (${job.model}) with firmware ${job.firmwareid}")
      } else {
        log.info(s"got job ${job.jobid} for node ${job.nodeid} (${job.model}) with firmware ${job.firmwareid}")
      }
    })
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val MQTTFlow: Flow[DataFlow, ProducerRecord[Array[Byte], Array[Byte]], NotUsed] =
    Flow[DataFlow]
      .filter(_.error.isEmpty)
      .filter(_.firmware.isDefined)
      .mapAsync(parallelism) {
        data =>
          Future {
            val job: Job = data.job

            log.info(s"create mqtt request for node ${job.nodeid} job ${job.jobid}")
            val url = otaFW.getTmpUrl(data.firmware.get.s3)

            val req = MQTTReq(
              a = "EXEC",
              p = s"v1/${job.nodeid}/in/EXEC/ota/software/download",
              uuid = job.jobid,
              l = Deserializer.msgpackMapper.writeValueAsBytes(Map(
                "url" -> url,
                "fwId" -> data.firmware.get.commitid
              ))
            )

            log.info(s"sending ${req.toString} to mqtt")
            new ProducerRecord[Array[Byte], Array[Byte]](
              OTAKafkaConfig.mqttReqTopic,
              job.jobid.getBytes(),
              req.toMsgPack
            )
          }
      }
      .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val NodeStatusFlow = Flow[DataFlow]
    .mapAsync(parallelism) {
      data => {
        val job: Job = data.job
        val status = OTANodeStatus(
          jobid = job.jobid,
          nodeid = job.nodeid,
          status = data.error.getOrElse(DOWNLOAD_SENT),
          when = UUIDs.timeBased()
        )

        log.info("writing to OTANodeStatusDb")
        OTANodeStatusDB(cassConn).model.store(status)
      }
    }
    .withAttributes(ActorAttributes.supervisionStrategy(cassandraDecider))

  val ShouldUpdateJobStatusFlow: Flow[DataFlow, DataFlowWrapper, NotUsed] =
    Flow[DataFlow]
      .filter(_.error.isDefined)
      .mapAsync(parallelism) {
        data => {
          val jobId: String = data.job.jobid
          val result = OTAJobInfoDB(cassConn).model.get(jobId)

          result.map({
            r => {
              DataFlowWrapper(data = data, info = r.head)
            }
          })
        }
      }
      .mapAsync(parallelism) {
        dataWrapper => {
          val jobId: String = dataWrapper.info.jobid
          val result = OTANodeStatusDB(cassConn).model.get(jobId)

          result.map({
            l => {
              val count: Int = dataWrapper.info.count
              val countDone: Int = l
                .groupBy(_.nodeid)
                .map(map => {
                  val latestStatus = if (map._1 == dataWrapper.data.job.nodeid) {
                    dataWrapper.data.error.get
                  } else {
                    map._2.sortBy(_.when).last.status
                  }

                  log.debug(s"found ${latestStatus} for ${map._1}")
                  MQTTMessages.isFinalStatus(latestStatus)
                })
                .count(b => {
                  b
                })

              log.info(s"found ${countDone.toString} done out of ${count.toString}")
              dataWrapper.copy(done = countDone.equals(count))
            }
          })
        }
      }
      .withAttributes(ActorAttributes.supervisionStrategy(cassandraDecider))

  val JobStatusFlow = Flow[DataFlowWrapper]
    .filter(_.done)
    .mapAsync(parallelism) {
      dataWrapper => {
        val status = OTAJobStatus(
          jobid = dataWrapper.info.jobid,
          when = UUIDs.timeBased(),
          status = JOB_DONE
        )

        log.info(s"writing ${status.toString} to OTAJobStatus")
        OTAJobStatusDB(cassConn).model.store(status)
      }
    }
    .withAttributes(ActorAttributes.supervisionStrategy(cassandraDecider))

  val KafkaSink = Flow[ProducerRecord[Array[Byte], Array[Byte]]]
    .toMat(configKafkaSink(_producerSettings = setupProducerSettings))(Keep.right)
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))


  val graph = RunnableGraph.fromGraph(GraphDSL.create() { implicit builder: GraphDSL.Builder[NotUsed] =>
    import GraphDSL.Implicits._

    val bcastTo = 4

    val kafkaOTAJob = builder.add(KafkaOTAJobSrc).out

    val jsonToJob = builder.add(JSONToJobFlow)
    val print = builder.add(PrintFlow)
    val bcastJob = builder.add(Broadcast[DataFlow](bcastTo))
    val checkJob = builder.add(CheckJobFlow)
    val checkFirmware = builder.add(CheckFirmwareFlow)
    val shouldUpdateJobStatus = builder.add(ShouldUpdateJobStatusFlow)
    val MQTT = builder.add(MQTTFlow)
    val nodeStatusDb = builder.add(NodeStatusFlow)
    val jobStatusDb = builder.add(JobStatusFlow)

    val kafka = builder.add(KafkaSink).in
    val ignore0 = builder.add(Sink.ignore).in
    val ignore1 = builder.add(Sink.ignore).in
    val ignore2 = builder.add(Sink.ignore).in

    kafkaOTAJob ~> jsonToJob ~> checkJob ~> checkFirmware ~> bcastJob ~> print ~> ignore0
    bcastJob ~> MQTT ~> kafka
    bcastJob ~> nodeStatusDb ~> ignore1
    bcastJob ~> shouldUpdateJobStatus ~> jobStatusDb ~> ignore2
    ClosedShape
  })
}
