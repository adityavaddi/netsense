package com.verizon.netsense.sse.app

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream._
import akka.stream.scaladsl.{Flow, GraphDSL, RunnableGraph}
import com.verizon.netsense.connector.KafkaSettings
import com.verizon.netsense.sse.config.SSEConfigLoader._
import com.verizon.netsense.sse.model.{SSESubscribeRequest, SSESubscribeResponse}
import com.verizon.netsense.sse.service.{BaseSSEService, SubscribeProcess}
import com.verizon.netsense.sse.util.ObjectMapperUtil
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.ProducerRecord

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

/**
 * Created by muppasw on 8/22/17.
 */
object SSESubscribeService extends BaseSSEService with KafkaSettings {

  implicit def actorSystem: ActorSystem = ActorSystem.create("SSESubscribeService")

  def decider(): Supervision.Decider = {

    case NonFatal(ex) =>
      log.error("Got non fatal exception in SSEActor flow", ex)
      Supervision.Restart
    case ex =>
      log.error("Got fatal exception in SSEActor flow, stream will be stopped", ex)
      Supervision.Stop
  }
  implicit val materializer: ActorMaterializer = ActorMaterializer(
    ActorMaterializerSettings(actorSystem).withSupervisionStrategy(decider())
  )

  implicit val parallel: Int = parallellismParam

  def graphiteHost: String = grapHostParam
  def graphitePort: Int    = grapPortParam

  override def kafkaBootstrapServers: String = kafkaServersParam
  override def kafkaClientId: String         = kafkaClientIdParam
  override def kafkaGroupId: String          = kafkaGroupIdParam
  override def kafkaOffsetReset: String      = kafkaOffsetResetParam

  override implicit val ec = ExecutionContext.Implicits.global

  lazy val msgUnmarshallingFlow = Flow[ConsumerRecord[Array[Byte], Array[Byte]]]
    .mapAsync(parallel) { e =>
      ObjectMapperUtil.fromJsonByteArrayAsync[SSESubscribeRequest](e.value)
    }
    .map { x =>
      x
    }
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  lazy val subscribeProcess = Flow[Option[SSESubscribeRequest]]
    .mapAsync(parallel) { e =>
      SubscribeProcess.validateAndProcess(e.get)
    }
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  lazy val subscribeKafkaSink = Flow[(SSESubscribeResponse)]
    .mapAsync(parallel) { x =>
      Future(
        new ProducerRecord[Array[Byte], Array[Byte]](x.responsetopic, ObjectMapperUtil.toJson(x).getBytes())
      )
    }
    .to(sink())

  /*
   * SSESubscribe Process Graph
   */

  def sseSubscribe: RunnableGraph[NotUsed] =
    RunnableGraph.fromGraph(g = GraphDSL.create() { implicit b =>
      import GraphDSL.Implicits._
      /*
       * Interface service will POST message to Kafka topic ms.request.sse
       */
      val Source = b.add(restartableSubscriptionSource(sseFilterTopicParam)).out
      /*
       * Unmarshalling the data
       */
      val UnMarshall = b.add(msgUnmarshallingFlow)

      /*
       * Based on request `type` SSE Service validates the process and responds the response to IS
       */
      val ValidateAndProcess = b.add(subscribeProcess)
      val sink               = b.add(subscribeKafkaSink).in

      Source ~> UnMarshall ~> ValidateAndProcess ~> sink

      ClosedShape
    })

}
