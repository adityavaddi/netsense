package com.verizon.netsense.service

import java.io._

import akka.actor.{Actor, ActorSystem, Props}
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Supervision}
import com.verizon.netsense.util.{Logging, ObjectMapperUtil}
import akka.stream._
import akka.stream.scaladsl.{Broadcast, FileIO, Flow, GraphDSL, Keep, RestartSink, RestartSource, RunnableGraph, Sink, Source}
import java.nio.file.{Files, Paths}

import akka.NotUsed
import akka.util.ByteString
import com.verizon.netsense.actors.SicStatusManager
import com.verizon.netsense.config.KafkaConfig
import com.verizon.netsense.helper.Common
import com.verizon.netsense.config.KafkaConfig._ //{bootStrapServers, groupId}
import com.verizon.netsense.config.KafkaConnector
import com.verizon.netsense.connector.CassandraConnector
import com.verizon.netsense.model._
import com.verizon.netsense.service.SICService.{ImgCaptureInit, StatusTimeOut}
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer, StringDeserializer, StringSerializer}
import org.joda.time.DateTime

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import scala.util.control.NonFatal


class SICService extends Actor with Logging with Common  {

  implicit val system = ActorSystem.create("SicAppService")
  implicit val ec = ExecutionContext.Implicits.global

  lazy val decider: Supervision.Decider = {
    case NonFatal(ex)  => log.error("Exception found in the Stream " + ex.printStackTrace()); Supervision.Restart
    case ex: Exception => log.error("Unhandled Exception seen " + ex.printStackTrace()); Supervision.stop;
  }

  implicit val mat = ActorMaterializer(
    ActorMaterializerSettings(system)
      .withInputBuffer(Math.pow(2, 10).toInt, Math.pow(2, 20).toInt)
      .withSupervisionStrategy(decider)
  )

  var isSourceRetryCount: Long = 1
  var sicSourceRetryCount: Long = 1
  var statusSourceRetryCount: Long = 1

  val statusManager = system.actorOf(Props(new SicStatusManager(CassandraConnector.connector)))

  lazy val setupISConsumerSettings = KafkaConnector.configKafkaConsumerSettings(bootStrapServers,
    groupId,
    new StringDeserializer,
    new StringDeserializer)

  lazy val setupCmdConsumerSettings = KafkaConnector.configKafkaConsumerSettings(bootStrapServers,
    groupId,
    new ByteArrayDeserializer,
    new ByteArrayDeserializer)

  lazy val setupProducerSettings = KafkaConnector.configKafkaProducerSettings(bootStrapServers,
    _keySerializer = new ByteArraySerializer,
    _valueSerializer = new ByteArraySerializer)

  lazy val setupISProducerSettings = KafkaConnector.configKafkaProducerSettings(bootStrapServers,
    _keySerializer = new StringSerializer,
    _valueSerializer = new StringSerializer)

  lazy val (statusSource, statusPub) = Source.queue[Map[String, String]](100, akka.stream.OverflowStrategy.fail).toMat(Sink.asPublisher(true))(Keep.both).run()

  lazy val kafkaISConsumerSource = configKafkaSource(setupISConsumerSettings, Set(requestTopic))
  kafkaISConsumerSource

  lazy val kafkaSicConsumerSource = configKafkaSource(setupCmdConsumerSettings, Set(sicResponseTopic))

  lazy val kafkaSicStatusSource = configKafkaSource(setupCmdConsumerSettings, Set(sicStatusTopic))

  lazy val restartableKafkaISConsumerSource: Source[ConsumerRecord[String, String], NotUsed] =
    RestartSource.withBackoff(
      minBackoff = 1.seconds,
      maxBackoff = 5.seconds,
      randomFactor = 0.2
    ) { () =>
      if(isSourceRetryCount == 1) {
        isSourceRetryCount = isSourceRetryCount + 1
        log.info("Starting kafkaISConsumerSource for Media Service " + bootStrapServers)
      }
      else {
        isSourceRetryCount = isSourceRetryCount  + 1
        log.error("Restarting kafkaISConsumerSource Consumer on failure " + isSourceRetryCount)
      }
      kafkaISConsumerSource
    }

  lazy val restartableKafkaSicConsumerSource: Source[ConsumerRecord[Array[Byte], Array[Byte]], NotUsed] =
    RestartSource.withBackoff(
      minBackoff = 1.seconds,
      maxBackoff = 5.seconds,
      randomFactor = 0.2
    ) { () =>
      if(sicSourceRetryCount == 1) {
        sicSourceRetryCount = sicSourceRetryCount + 1
        log.info("Starting kafkaSicConsumerSource Consumer for Media Service " + bootStrapServers)
      }
      else {
        sicSourceRetryCount = sicSourceRetryCount + 1
        log.error("Restarting kafkaSicConsumerSource Consumer on failure " + sicSourceRetryCount)
      }
      kafkaSicConsumerSource
    }

  lazy val restartableKafkaSicStatusSource: Source[ConsumerRecord[Array[Byte], Array[Byte]], NotUsed] =
    RestartSource.withBackoff(
      minBackoff = 1.seconds,
      maxBackoff = 5.seconds,
      randomFactor = 0.2
    ) { () =>
      if(statusSourceRetryCount == 1) {
        statusSourceRetryCount = statusSourceRetryCount + 1
        log.info("Starting kafkaSicStatusSource Consumer for Media Service " + bootStrapServers)
      }
      else {
        statusSourceRetryCount = statusSourceRetryCount + 1
        log.error("Restarting kafkaSicStatusSource Consumer on failure " + statusSourceRetryCount)
      }
      kafkaSicStatusSource
    }

  var sicImg = "sic"
  
  // Kafka ms.request.media -> SicISQuery Envelope
  val ISReqUnmarshallFlow : Flow[ConsumerRecord[String, String], SicISQueryEnvelope, NotUsed] =
    Flow[ConsumerRecord[String, String]].mapAsync (parallelism) { x =>
      log.debug(s"consumer record: ${x.key()} : ${x.value()}")
      val res = ObjectMapperUtil.fromJsonAsync[SicISQueryEnvelope](x.value)
      res.map {
        qEnv => statusManager ! SicStatusManager.ISReqReceived(Map("msgId" -> s"${qEnv.messageid}", "topic" -> requestTopic))
      }
      res
    }

  // SicISQueryEnvelope -> SicReqEvent -> Kafka.Command.Topic
  lazy val sicCommandFlow = Flow[SicISQueryEnvelope]
    .map(convertCASELtoRestPack)
    .map(e => (e._1, e._2))
    .map(m => {
      log.debug("Sending sic-request MS => Bridge " + m._2.toJSON)
      new ProducerRecord(KafkaConfig.sicCommandTopic, m._1.messageid.getBytes(), m._2.toMsgPack)
    })

  lazy val sicKafkaSink = sicCommandFlow.toMat(configKafkaProducer(_producerSettings =  setupProducerSettings))(Keep.right)

  lazy val sicStatusUpdateSink = sicCommandFlow.map ( x =>
    statusManager ! SicStatusManager.SicReqSent(Map("msgId" -> s"${x.key.toString}", "topic" -> sicResponseTopic))
  ).toMat(Sink.ignore)(Keep.right)

  // Kafka.media.topic -> SicResponseEvent
  lazy val sicMsgUnmarshallingFlow = Flow[ConsumerRecord[Array[Byte], Array[Byte]]].mapAsync(parallelism) { x =>
    log.debug(s"SIC Received =  topic: ${x.topic()} , key: ${x.key()} ,  msg: ${x.value()}")
    val res = ObjectMapperUtil.fromJsonAsync[SicResponseEvent](x.value())
    res.map {
     rEnv => {
       statusManager ! SicStatusManager.SicRespReceived(Map("msgId" -> s"${rEnv.uuid}", "topic" -> sicResponseTopic))
       log.debug(s"${rEnv.toString}")
       log.debug(s"${rEnv.l.toString}")
     }
    }
    res
  }

  //Kafka SIC Response  -> IS Response Payload
  lazy val messagePackTransformFlow = Flow[SicResponseEvent]
    .mapAsync(parallelism) { x =>
      Future {
        log.debug(s"sicResponsePayload: ${x.l}")
        var status = 400
        var isSuccess = false
        x.l.e match {
          case x: String => if (x.length == 0 || x.isEmpty) {
              status = 200
              isSuccess = true
            }
          case _ => // Do Nothing
        }
        val isResponsePayload = SicISResponsePayload(x.f, isSuccess, x.l.ts, x.l.e, status, x.l.c )
        isResponsePayload
        //(x.uuid, x.l.c)
      }
    }
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  lazy val sicFileSink  =
    Flow[SicISResponsePayload]
      .map { x =>
        log.debug(s"image: $sicImg")
        ByteString(x.message)
      }.toMat(FileIO.toPath(Paths.get(sicImg)))(Keep.right)


  val consoleSink = Sink.foreach[SicISResponsePayload] { x =>
    sicImg = s"${x.requestid}.jpg"
    log.debug(s"image: $sicImg")
    val bos = new BufferedOutputStream(new FileOutputStream(sicImg))
    bos.write(x.message)
    bos.close()
  }

  lazy val ISResponseFlow = Flow[SicISResponsePayload].map { x =>
    val isResponse = SicISResponseEnvelope(x.requestid, x)
    log.debug(s"Sending IS Response: ${isResponse.messageid} : ${isResponse.toJSON}")
    new ProducerRecord(KafkaConfig.responseTopic, isResponse.messageid, isResponse.toJSON)
    }

  lazy val ISKafkaSink = ISResponseFlow.toMat(configKafkaProducer(_producerSettings =  setupISProducerSettings))(Keep.right)
  lazy val ISTimeOutKafkaSink = ISResponseFlow.toMat(configKafkaProducer(_producerSettings =  setupISProducerSettings))(Keep.right)

  lazy val ISStatusUpdateSink = ISResponseFlow.map ( x =>
    statusManager ! SicStatusManager.ISRespSent(Map("msgId" -> s"${x.key.toString}", "topic" -> "ms.api.reply"))
  ).toMat(Sink.ignore)(Keep.right)

  val SicTimeoutFlow = Flow[Map[String,String]].map { x =>
    // Generate appropriate IS Timeout message
    SicISResponsePayload(x.get("msgId").get, false, DateTime.now().toString , x.get("status").get, 404, Array.emptyByteArray )
  }

  val SicStillImageCaptureGraph = RunnableGraph.fromGraph(g = GraphDSL.create(sicFileSink, consoleSink)((fileSink, _) => fileSink) {
    implicit builder => (fileSink, console) =>
      import GraphDSL.Implicits._

      val isKafkaSource = builder.add(restartableKafkaISConsumerSource).out
      val isUnmarshallFlow = builder.add(ISReqUnmarshallFlow)
      val sicKSink = builder.add(sicKafkaSink)
      val sicStatusSink = builder.add(sicStatusUpdateSink)
      val broadcast1 = builder.add(Broadcast[SicISQueryEnvelope](2))

      val sicKafkaSource = builder.add(restartableKafkaSicConsumerSource).out
      val sicFileUnmarshallFlow = builder.add(sicMsgUnmarshallingFlow)
      val msgTransformFlow = builder.add(messagePackTransformFlow)
      val isKafkaSink = builder.add(ISKafkaSink)
      val isTimeoutSink = builder.add(ISTimeOutKafkaSink)
      val isStatusUpdateSink = builder.add(ISStatusUpdateSink)
      val broadcast2 = builder.add(Broadcast[SicISResponsePayload](4))

      val sicStatusSource = builder.add(Source.fromPublisher(statusPub)).out
      val sicTimeoutFlow = builder.add(SicTimeoutFlow)

      isKafkaSource ~> isUnmarshallFlow ~> broadcast1 ~> sicKSink.in
                                           broadcast1 ~> sicStatusSink.in

      sicKafkaSource ~>  sicFileUnmarshallFlow ~> msgTransformFlow  ~> broadcast2 ~> fileSink
                                                                       broadcast2 ~> console
                                                                       broadcast2 ~> isKafkaSink
                                                                       broadcast2 ~> isStatusUpdateSink

      sicStatusSource ~> sicTimeoutFlow ~> isTimeoutSink // Todo: Change to restartable source with Backoff

      // Todo:  broadcast ~> S3.Sink - post Granola.

      ClosedShape

  })

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = super.preRestart(reason, message)

  override def receive: Receive = {
    case ImgCaptureInit => SicStillImageCaptureGraph.run()

    case StatusTimeOut(msg) => statusSource.offer(msg)

  }

  override def postStop(): Unit = super.postStop()

}
object SICService {
  val props = Props[SICService]
  case object ImgCaptureInit
  case class StatusTimeOut(msg: Map[String, String])
}




