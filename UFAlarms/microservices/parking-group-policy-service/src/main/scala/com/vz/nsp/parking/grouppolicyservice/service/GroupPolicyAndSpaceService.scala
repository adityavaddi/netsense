package com.vz.nsp.parking.grouppolicyservice.service

import java.util.concurrent.atomic.AtomicLong

import akka.NotUsed
import akka.actor.ActorSystem
import akka.kafka.scaladsl.Producer
import akka.stream._
import akka.stream.scaladsl.{Flow, GraphDSL, Keep, RestartSource, RunnableGraph, Source}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import com.vz.nsp.parking.config.KafkaConfig._
import com.vz.nsp.parking.config.KafkaConnector._
import com.vz.nsp.parking.config.ServiceConfig.{maxBackoffSource, minBackoffSource, parallelism, randomFactorSource}
import com.vz.nsp.parking.grouppolicyservice.helper.StreamHelper
import com.vz.nsp.parking.model._
import com.vz.nsp.parking.util.AkkaStreamUtil._
import com.vz.nsp.parking.util.ObjectMapperUtil
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{ByteArraySerializer, StringDeserializer}

import scala.concurrent.Future

class GroupPolicyServiceAndSpaceService(streamHelper: StreamHelper) extends Instrumented with Logging {
  implicit val queryServiceActorSystem = ActorSystem("Group-Policy-System")

  import queryServiceActorSystem.dispatcher

  implicit val mat = ActorMaterializer(
    ActorMaterializerSettings(queryServiceActorSystem)
      .withInputBuffer(Math.pow(2, 10).toInt, Math.pow(2, 20).toInt)
      .withSupervisionStrategy(akkaGraphDecider)
  )

  private[this] val groupKafkaSourceTimer: Timer       = metrics.timer("policygroup-kafka-source-consumer")
  private[this] val groupKafkaSinkTimer: Timer         = metrics.timer("policygroup-kafka-sink-producer")
  private[this] val groupUnMarshallingFlowTimer: Timer = metrics.timer("policygroup-json-unmarshall-flow-timer")


  lazy val msgUnmarshallingFlow: Flow[ConsumerRecord[String, String], AppRequest, NotUsed] =
    Flow[ConsumerRecord[String, String]]
      .mapAsync(parallelism) { consumerRecord =>
        groupUnMarshallingFlowTimer.time {
          ObjectMapperUtil.jsonToAppRequest(consumerRecord.value)
        }
      }
      .withAttributes(ActorAttributes.supervisionStrategy(msgUnmarshallFlowDecider))

  lazy val setupConsumerSettings =
    configKafkaConsumerSettings(bootStrapServers, groupId, new StringDeserializer, new StringDeserializer)


  lazy val kafkaConsumerSource = configKafkaSource(setupConsumerSettings, Set(requestTopicName)).map { consumerRecord =>
    groupKafkaSourceTimer.time(consumerRecord)
  }

  var sourceRetryCount = new AtomicLong(0L)

  import scala.concurrent.duration._

  lazy val kafkaRestartableConsumerSource: Source[ConsumerRecord[String, String], NotUsed] = RestartSource.withBackoff(
    minBackoff = minBackoffSource.seconds,
    maxBackoff = maxBackoffSource.seconds,
    randomFactor = randomFactorSource
  ) { () =>
    if (sourceRetryCount.get == 0) {
      log.info(s"##### Starting Consumer for Group Policy Service with sourceRetryCount ${sourceRetryCount.getAndIncrement}")
      log.info(s"##### minBackoffSource: $minBackoffSource maxBackoffSource: $maxBackoffSource randomFactorSource: $randomFactorSource")
    }
    else {
      log.error("Restarting Consumer on failure " + sourceRetryCount.getAndIncrement)
    }
    kafkaConsumerSource
  }

  lazy val groupPolicyQueryFlow: Flow[AppRequest, (AppRequest, String), NotUsed] = Flow[AppRequest]
    .map(appRequest => (appRequest, streamHelper.processRequest(appRequest)))
    .mapAsync(parallelism) {
      case (appRequest, futureOfAppResponse) => futureOfAppResponse.map(appRes => (appRequest, ObjectMapperUtil.toJson(appRes)))
    }
    .withAttributes(ActorAttributes.supervisionStrategy(queryFlowDecider))

  lazy val setupProducerSettings = configKafkaProducerSettings(bootStrapServers,
                                                               _keySerializer = new ByteArraySerializer,
                                                               _valueSerializer = new ByteArraySerializer)


  val responseKafkaSink = Flow[(AppRequest, String)]
    .mapAsync(parallelism) { x =>
      groupKafkaSinkTimer.time {
        Future(new ProducerRecord[Array[Byte], Array[Byte]](x._1.responsetopic, x._1.messageid.getBytes, x._2.getBytes))
      }
    }
    .toMat(Producer.plainSink(setupProducerSettings))(Keep.right)

  def groupPolicyServiceInit = queryServiceGraph.run()

  val queryServiceGraph = RunnableGraph.fromGraph(g = GraphDSL.create() { implicit b =>
    import GraphDSL.Implicits._

    // Source
    val Source = b.add(kafkaRestartableConsumerSource).out

    // Flows
    val messageUnpackFlow = b.add(msgUnmarshallingFlow)
    val groupPolicyQuery = b.add(groupPolicyQueryFlow)

    // Sink
    val kafkaProducerSink = b.add(responseKafkaSink).in

    Source ~> messageUnpackFlow ~> groupPolicyQuery ~> kafkaProducerSink

    ClosedShape
  })

}

object GroupPolicyServiceAndSpaceService {
  def apply(streamHelper: StreamHelper): GroupPolicyServiceAndSpaceService = new GroupPolicyServiceAndSpaceService(streamHelper)
}
