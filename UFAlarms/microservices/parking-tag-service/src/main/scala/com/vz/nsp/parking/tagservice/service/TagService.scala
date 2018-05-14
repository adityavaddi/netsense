package com.vz.nsp.parking.tagservice.service

import java.util.concurrent.atomic.AtomicLong

import akka.NotUsed
import akka.actor.ActorSystem
import akka.kafka.{ConsumerSettings, ProducerSettings}
import akka.kafka.scaladsl.Consumer.Control
import akka.kafka.scaladsl.Producer
import akka.stream._
import akka.stream.scaladsl.{Flow, GraphDSL, Keep, RestartSource, RunnableGraph, Source}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import com.vz.nsp.parking.config.KafkaConfig._
import com.vz.nsp.parking.config.KafkaConnector._
import com.vz.nsp.parking.config.ServiceConfig.{maxBackoffSource, minBackoffSource, parallelism, randomFactorSource}
import com.vz.nsp.parking.model._
import com.vz.nsp.parking.tagservice.helper.StreamHelper
import com.vz.nsp.parking.util.AkkaStreamUtil._
import com.vz.nsp.parking.util.ObjectMapperUtil
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{ByteArraySerializer, StringDeserializer}

import scala.concurrent.Future

class TagService(streamHelper: StreamHelper) extends Instrumented with Logging {
  implicit val queryServiceActorSystem = ActorSystem("Policy-Tag-system")

  import queryServiceActorSystem.dispatcher

  implicit val mat = ActorMaterializer(
    ActorMaterializerSettings(queryServiceActorSystem)
      .withInputBuffer(Math.pow(2, 10).toInt, Math.pow(2, 20).toInt)
      .withSupervisionStrategy(akkaGraphDecider)
  )

  private[this] val tagKafkaSourceTimer: Timer = metrics.timer("tag-kafka-source-consumer")
  private[this] val tagKafkaSinkTimer: Timer = metrics.timer("tag-kafka-sink-producer")
  private[this] val tagUnMarshallingFlowTimer: Timer = metrics.timer("tag-json-unmarshall-flow-timer")

  lazy val msgUnmarshallingFlow: Flow[ConsumerRecord[String, String], AppRequest, NotUsed] =
    Flow[ConsumerRecord[String, String]]
      .mapAsync(parallelism) { consumerRecord =>
        tagUnMarshallingFlowTimer.time {
          ObjectMapperUtil.jsonToAppRequest(consumerRecord.value)
        }
      }
      .withAttributes(ActorAttributes.supervisionStrategy(msgUnmarshallFlowDecider))

  lazy val setupConsumerSettings: ConsumerSettings[String, String] =
    configKafkaConsumerSettings(bootStrapServers, groupId, new StringDeserializer, new StringDeserializer)

  lazy val kafkaConsumerSource: Source[ConsumerRecord[String, String], Control] = configKafkaSource(setupConsumerSettings, Set(requestTopicName)).map { e =>
    tagKafkaSourceTimer.time(e)
  }

  var sourceRetryCount = new AtomicLong(0L)

  import scala.concurrent.duration._

  lazy val kafkaRestartableConsumerSource: Source[ConsumerRecord[String, String], NotUsed] = RestartSource.withBackoff(
    minBackoff = minBackoffSource.seconds,
    maxBackoff = maxBackoffSource.seconds,
    randomFactor = randomFactorSource
  ) { () =>
    if (sourceRetryCount.get == 0) {
      log.info(s"##### Starting Consumer for Tag Service with sourceRetryCount ${sourceRetryCount.getAndIncrement}")
      log.info(s"##### minBackoffSource: $minBackoffSource maxBackoffSource: $maxBackoffSource randomFactorSource: $randomFactorSource")
    }
    else {
      log.error("Restarting Consumer on failure " + sourceRetryCount.getAndIncrement)
    }
    kafkaConsumerSource
  }

  lazy val tagQueryFlow: Flow[AppRequest, (AppRequest, String), NotUsed] = Flow[AppRequest]
    .map(appRequest => (appRequest, streamHelper.processRequest(appRequest)))
    .mapAsync(parallelism) {
      case (appRequest, futureOfAppResponse) => futureOfAppResponse.map(appRes => (appRequest, ObjectMapperUtil.toJson(appRes)))
    }
    .withAttributes(ActorAttributes.supervisionStrategy(queryFlowDecider))


  lazy val setupProducerSettings: ProducerSettings[Array[Byte], Array[Byte]] = configKafkaProducerSettings(bootStrapServers,
    _keySerializer = new ByteArraySerializer,
    _valueSerializer = new ByteArraySerializer)

  val responseKafkaSink = Flow[(AppRequest, String)]
    .mapAsync(parallelism) { x =>
      tagKafkaSinkTimer.time {
        Future(new ProducerRecord[Array[Byte], Array[Byte]](x._1.responsetopic, x._1.messageid.getBytes, x._2.getBytes))
      }
    }
    .toMat(Producer.plainSink(setupProducerSettings))(Keep.right)

  def queryServiceInit = queryServiceGraph.run()

  val queryServiceGraph = RunnableGraph.fromGraph(g = GraphDSL.create() { implicit b =>
    import GraphDSL.Implicits._

    // Source
    val Source = b.add(kafkaRestartableConsumerSource).out

    // Flows
    val messageUnpackFlow = b.add(msgUnmarshallingFlow)
    val TagQuery = b.add(tagQueryFlow)

    // Sink
    val kafkaProducerSink = b.add(responseKafkaSink).in

    Source ~> messageUnpackFlow ~> TagQuery ~> kafkaProducerSink

    ClosedShape
  })

}

object TagService {
  def apply(streamHelper: StreamHelper): TagService = new TagService(streamHelper)
}
