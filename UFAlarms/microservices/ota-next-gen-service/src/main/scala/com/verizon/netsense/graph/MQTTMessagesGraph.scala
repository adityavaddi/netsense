package com.verizon.netsense.graph

import akka.NotUsed
import akka.actor.ActorSystem
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer}
import akka.stream.scaladsl.{Broadcast, Flow, GraphDSL, Keep, Merge, Partition, RunnableGraph, Sink}
import akka.stream.{ActorAttributes, ActorMaterializer, ActorMaterializerSettings, ClosedShape}
import com.datastax.driver.core.utils.UUIDs
import com.fasterxml.jackson.core.JsonParseException
import com.outworkers.phantom.connectors.CassandraConnection
import com.verizon.netsense.config.OTAKafkaConfig
import com.verizon.netsense.database._
import com.verizon.netsense.entity._
import com.verizon.netsense.helper.{JobHelper, OTAFirmware, StreamHelper}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.{Deserializer, Logging}
import nl.grons.metrics.scala.Meter
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.ProducerRecord

import scala.concurrent.{ExecutionContext, Future}

object MQTTMessagesGraphData {

  case class DataFlow(res: MQTTRes,
                      status: Option[String] = None,
                      progress: Long = -1,
                      nodeid: Option[String] = None,
                      info: Option[OTAJobInfo] = None,
                      isTerminated: Boolean = false,
                      done: Boolean = false)

}

case class MQTTMessagesGraph(otaFW: OTAFirmware, cassConn: CassandraConnection)
  extends Logging
    with StreamHelper
    with Constants
    with Instrumented {

  import MQTTMessagesGraphData._

  implicit val system = ActorSystem.create("MQTT-Messsages-Graph-system")
  implicit val materializer = ActorMaterializer(ActorMaterializerSettings(system))
  implicit val ec = ExecutionContext.Implicits.global

  val parallelism = 1000

  val dwInvMeter: Meter = metrics.meter(DOWNLOAD_INVALID)
  val dwFailMeter: Meter = metrics.meter(DOWNLOAD_FAILED)
  val inInvMeter: Meter = metrics.meter(INSTALL_INVALID)
  val inFailMeter: Meter = metrics.meter(INSTALL_FAILED)
  val msgMeter: Meter = metrics.meter("mqtt-messages")

  val setupConsumerSettings = configKafkaConsumerSettings(
    OTAKafkaConfig.bootStrapServers,
    OTAKafkaConfig.mqttGroupId,
    new ByteArrayDeserializer,
    new ByteArrayDeserializer
  )

  val setupProducerSettings = configKafkaProducerSettings(
    OTAKafkaConfig.bootStrapServers,
    new ByteArraySerializer,
    new ByteArraySerializer)

  val KafkaMQTTResSrc = configKafkaSource(setupConsumerSettings, Set(OTAKafkaConfig.mqttResTopic))

  val FromJSONFlow: Flow[ConsumerRecord[Array[Byte], Array[Byte]], MQTTRes, NotUsed] =
    Flow[ConsumerRecord[Array[Byte], Array[Byte]]]
      .mapAsync(parallelism) {
        record => {
          Future {
            fromJSONByteArrayToObj[MQTTRes](record.value())
          }.recover {
            case ex@(_: JsonParseException) =>
              log.error("Unable to parse the element: " + new String(record.value(), "UTF-8") + " " + ex)
              throw ex
          }
        }
      }
      .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val CheckTopicFlow: Flow[MQTTRes, MQTTRes, NotUsed] =
    Flow[MQTTRes]
      .filter(res => {
        msgMeter.mark()

        val topic: String = res.p

        topic match {
          case x if x.contains("/ota/software/download") => true
          case x if x.contains("/ota/software/install") => true
          case _ =>
            log.warn(s"dropping ${topic}")
            false
        }
      })
      .withAttributes(ActorAttributes.supervisionStrategy(cassandraDecider))

  val CheckJobFlow: Flow[MQTTRes, DataFlow, NotUsed] =
    Flow[MQTTRes]
      .mapAsync(parallelism) {
        res => {
          val jobId: String = res.uuid

          JobHelper(cassConn).jobLastStatus(jobId).map(state => {
            val terminated: Boolean = JobHelper(cassConn).terminatedState(state)

            if (terminated) {
              log.warn(s"Job ${jobId} is ${state}")
            }

            DataFlow(res = res, isTerminated = terminated)
          })
        }
      }
      .filterNot(_.isTerminated)
      .withAttributes(ActorAttributes.supervisionStrategy(cassandraDecider))

  val MsgTypePartition: Partition[DataFlow] =
    Partition[DataFlow](3, data => {
      val topic: String = data.res.p

      topic match {
        case x if x.contains("/ota/software/download") => 1
        case x if x.contains("/ota/software/install") => 2
        case _ => 0
      }
    })


  val SoftwareDownloadFlow: Flow[DataFlow, DataFlow, NotUsed] =
    Flow[DataFlow]
      .map(data => {
        val res: MQTTRes = data.res
        val default = -1

        val status: String = res.l.getOrElse("s", default) match {
          case 0 =>
            dwInvMeter.mark()
            DOWNLOAD_INVALID
          case 1 => DOWNLOAD_IN_PROGRESS
          case 2 => DOWNLOAD_SUCCESSFUL
          case 3 =>
            dwFailMeter.mark()
            DOWNLOAD_FAILED
          case _ => DOWNLOAD_STARTED
        }

        data.copy(
          status = Some(status),
          nodeid = Some(res.sid),
          progress = res.l.getOrElse("p", -1).asInstanceOf[Int].asInstanceOf[Long]
        )
      })
      .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val SoftwareInstallFlow: Flow[DataFlow, DataFlow, NotUsed] =
    Flow[DataFlow]
      .map(data => {
        val res: MQTTRes = data.res
        val default = -1

        val status: String = res.l.getOrElse("s", default) match {
          case 0 =>
            inInvMeter.mark()
            INSTALL_INVALID
          case 1 => INSTALL_IN_PROGRESS
          case 2 => INSTALL_SUCCESSFUL
          case 3 =>
            inFailMeter.mark()
            INSTALL_FAILED
          case _ => INSTALL_STARTED
        }

        data.copy(status = Some(status), nodeid = Some(res.sid))
      })
      .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val GetJobInfoIfSuccessFlow: Flow[DataFlow, DataFlow, NotUsed] =
    Flow[DataFlow]
      .filter(_.status.get == DOWNLOAD_SUCCESSFUL)
      .mapAsync(parallelism) {
        data => {
          val res: MQTTRes = data.res
          val uuid: String = res.uuid
          val result = OTAJobInfoDB(cassConn).model.get(uuid)

          result.map({
            r => {
              data.copy(info = Some(r.head))
            }
          })
        }
      }
      .withAttributes(ActorAttributes.supervisionStrategy(cassandraDecider))

  val InstallSoftwareFlow = Flow[DataFlow]
    .mapAsync(parallelism) {
      data =>
        Future {
          val res: MQTTRes = data.res
          val uuid: String = res.uuid
          val fwID: String =  data.info.get.firmwareid
          val commitID: String = otaFW.getCommitFromFwID(fwID)

          log.info(s"create mqtt request for node ${data.nodeid} job ${uuid}")



          val req = MQTTReq(
            a = "EXEC",
            p = s"v1/${data.nodeid.get}/in/EXEC/ota/software/install",
            uuid = uuid,
            l = Deserializer.msgpackMapper.writeValueAsBytes(
              Map("fwId" -> commitID)
            ))

          log.info(s"sending ${req.toString} to mqtt")
          new ProducerRecord[Array[Byte], Array[Byte]](
            OTAKafkaConfig.mqttReqTopic,
            uuid.getBytes(),
            req.toMsgPack)
        }
    }
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val KafkaSink = Flow[ProducerRecord[Array[Byte], Array[Byte]]]
    .toMat(configKafkaSink(_producerSettings = setupProducerSettings))(Keep.right)
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val NodeStatusDB = Flow[DataFlow]
    .mapAsync(parallelism) {
      data => {
        val res: MQTTRes = data.res
        val nodeStatus = OTANodeStatus(
          jobid = res.uuid,
          nodeid = data.nodeid.get,
          status = data.status.get,
          progress = data.progress,
          when = UUIDs.timeBased()
        )

        log.info(s"writing ${nodeStatus} to OTANodeStatusDb")
        OTANodeStatusDB(cassConn).model.store(nodeStatus)
      }
    }
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val ShouldUpdateJobStatusFlow: Flow[DataFlow, DataFlow, NotUsed] =
    Flow[DataFlow]
      .filter(data => {
        MQTTMessages.isFinalStatus(data.status.get)
      })
      .mapAsync(parallelism) {
        data => {
          val res: MQTTRes = data.res
          val uuid: String = res.uuid
          val result = OTAJobInfoDB(cassConn).model.get(uuid)

          result.map({
            r => {
              data.copy(info = Some(r.head))
            }
          })
        }
      }
      .mapAsync(parallelism) {
        data => {
          val result = OTANodeStatusDB(cassConn).model.get(data.res.uuid)

          result.map({
            l => {
              val count: Int = data.info.get.count
              val countDone: Int = l
                .groupBy(_.nodeid)
                .map(map => {
                  val latestStatus = if (map._1 == data.nodeid.get) {
                    data.status.get
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
              data.copy(done = countDone.equals(count))
            }
          })
        }
      }
      .withAttributes(ActorAttributes.supervisionStrategy(cassandraDecider))

  val JobStatusDB = Flow[DataFlow]
    .filter(_.done)
    .mapAsync(parallelism) {
      data => {
        val status = OTAJobStatus(
          jobid = data.res.uuid,
          when = UUIDs.timeBased(),
          status = JOB_DONE
        )

        log.info(s"writing ${status.toString} to OTAJobStatus")

        OTAJobStatusDB(cassConn).model.store(status)
      }
    }
    .withAttributes(ActorAttributes.supervisionStrategy(cassandraDecider))

  val graph = RunnableGraph.fromGraph(GraphDSL.create() { implicit builder: GraphDSL.Builder[NotUsed] =>
    import GraphDSL.Implicits._

    val kafkaMQTTRes = builder.add(KafkaMQTTResSrc).out

    val fromJSON = builder.add(FromJSONFlow)
    val checkTopic = builder.add(CheckTopicFlow)
    val checkJob = builder.add(CheckJobFlow)
    val msgType = builder.add(MsgTypePartition)
    val softwareDownload = builder.add(SoftwareDownloadFlow)
    val softwareInstall = builder.add(SoftwareInstallFlow)
    val bcastStatus = builder.add(Broadcast[DataFlow](2))
    val bcastSD = builder.add(Broadcast[DataFlow](2))
    val getJobInfoIfSuccess = builder.add(GetJobInfoIfSuccessFlow)
    val mergeToNodeStatus = builder.add(Merge[DataFlow](2))
    val installSoftware = builder.add(InstallSoftwareFlow)
    val shouldUpdateJobStatus = builder.add(ShouldUpdateJobStatusFlow)
    val nodeStatusDB = builder.add(NodeStatusDB)
    val JobStatusDb = builder.add(JobStatusDB)

    val kafka = builder.add(KafkaSink).in
    val ignore = builder.add(Sink.ignore).in
    val ignore1 = builder.add(Sink.ignore).in
    val ignore2 = builder.add(Sink.ignore).in

    kafkaMQTTRes ~> fromJSON ~> checkTopic ~> checkJob ~> msgType.in
    msgType.out(0) ~> ignore
    msgType.out(1) ~> softwareDownload ~> bcastSD ~> mergeToNodeStatus ~> bcastStatus ~> nodeStatusDB ~> ignore1
    bcastSD ~> getJobInfoIfSuccess ~> installSoftware ~> kafka
    msgType.out(2) ~> softwareInstall ~> mergeToNodeStatus
    bcastStatus ~> shouldUpdateJobStatus ~> JobStatusDb ~> ignore2

    ClosedShape
  })
}
