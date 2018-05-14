package com.vz.nsp.trigger.service

import java.util.concurrent.atomic.AtomicLong

import akka.NotUsed
import akka.actor.ActorSystem
import akka.kafka.scaladsl.Producer
import akka.stream.{ActorMaterializerSettings, _}
import akka.stream.scaladsl.{Flow, GraphDSL, Keep, RestartSource, RunnableGraph, Source}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import com.vz.nsp.util.AkkaStreamUtil._
import com.vz.nsp.util.ObjectMapperUtil
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{ByteArraySerializer, StringDeserializer}
import com.vz.nsp.trigger.model.casel.AppRequest
import com.vz.nsp.trigger.config.ServiceConfig._
import com.vz.nsp.trigger.config.KafkaConnector
import com.vz.nsp.trigger.config.KafkaConfig._

import scala.concurrent.Future
import scala.util.control.NonFatal


class TriggerService(streamHelper: StreamService) extends Instrumented with Logging {

  implicit val queryServiceActorSystem = ActorSystem("Trigger-service")

  import queryServiceActorSystem.dispatcher

  implicit val mat = ActorMaterializer(
    ActorMaterializerSettings(queryServiceActorSystem)
      .withInputBuffer(Math.pow(2, 10).toInt, Math.pow(2, 20).toInt)
      .withSupervisionStrategy(akkaGraphDecider)
  )

    val kafkaConnector = new KafkaConnector()


  private[this] val triggerKafkaSourceTimer: Timer = metrics.timer("trigger-kafka-source-consumer")
  private[this] val triggerKafkaSinkTimer: Timer = metrics.timer("trigger-kafka-sink-producer")
  private[this] val triggerUnMarshallingFlowTimer: Timer = metrics.timer("trigger-json-unmarshall-flow-timer")

  lazy val msgUnmarshallingFlow: Flow[ConsumerRecord[String, String], AppRequest, NotUsed] =
    Flow[ConsumerRecord[String, String]]
      .mapAsync(parallelism) {
        consumerRecord =>
          triggerUnMarshallingFlowTimer.time {
            ObjectMapperUtil.jsonToAppRequest(consumerRecord.value)
          }
      }
      .withAttributes(ActorAttributes.supervisionStrategy(msgUnmarshallFlowDecider))

  lazy val setupConsumerSettings =
    kafkaConnector.configKafkaConsumerSettings(bootStrapServers, groupId, new StringDeserializer, new StringDeserializer)

  lazy val kafkaConsumerSource = configKafkaSource(setupConsumerSettings, Set(requestTopicName)).map { e =>
    triggerKafkaSourceTimer.time(e)
  }

  var sourceRetryCount = new AtomicLong(0L)

  import scala.concurrent.duration._

  lazy val kafkaRestartableConsumerSource: Source[ConsumerRecord[String, String], NotUsed] = RestartSource.withBackoff(
    minBackoff = minBackoffSource.seconds,
    maxBackoff = maxBackoffSource.seconds,
    randomFactor = randomFactorSource
  ) {
    () =>
      if (sourceRetryCount.get == 0) {
        log.info(s"##### Starting Consumer for Trigger Service with sourceRetryCount ${
          sourceRetryCount.getAndIncrement
        }")
        log.info(s"##### minBackoffSource: $minBackoffSource maxBackoffSource: $maxBackoffSource randomFactorSource: $randomFactorSource")
      }
      else {
        log.error("Restarting Consumer on failure " + sourceRetryCount.getAndIncrement)
      }
      kafkaConsumerSource
  }


  lazy val triggerQueryFlow: Flow[AppRequest, (AppRequest, String), NotUsed] = Flow[AppRequest]
    .map(appRequest => (appRequest, streamHelper.processRequest(appRequest)))
    .mapAsync(parallelism) {
      case (appRequest, futureOfAppResponse) => futureOfAppResponse.map(appRes => (appRequest, ObjectMapperUtil.toJson(appRes)))
    }
    .withAttributes(ActorAttributes.supervisionStrategy(queryFlowDecider))

  lazy val setupProducerSettings = kafkaConnector.configKafkaProducerSettings(bootStrapServers,
    _keySerializer = new ByteArraySerializer,
    _valueSerializer = new ByteArraySerializer)

  println(setupProducerSettings)

  val responseKafkaSink = Flow[(AppRequest, String)]
    .mapAsync(parallelism) { x =>
      triggerKafkaSinkTimer.time {
        log.debug(s"Sending response to topic ${x._1.responsetopic} and response is: ${x._2}")
        Future(new ProducerRecord[Array[Byte], Array[Byte]](x._1.responsetopic, x._1.messageid.getBytes, x._2.getBytes))
      }
    }
    .toMat(Producer.plainSink(setupProducerSettings))(Keep.right)


  def queryServiceInit = queryServiceGraph.run()


  val queryServiceGraph = RunnableGraph.fromGraph(g = GraphDSL.create() {
    implicit b =>

      import GraphDSL.Implicits._

      // Source
      val Source = b.add(kafkaRestartableConsumerSource).out

      // Flows
      val messageUnpackFlow = b.add(msgUnmarshallingFlow)
      val TriggerQuery = b.add(triggerQueryFlow)

      // Sink
      val kafkaProducerSink = b.add(responseKafkaSink).in

      Source ~> messageUnpackFlow ~> TriggerQuery ~> kafkaProducerSink

      ClosedShape
  })
}

object TriggerService {
  def apply(streamHelper: StreamService): TriggerService = new TriggerService(streamHelper)
}
