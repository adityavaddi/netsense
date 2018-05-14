package com.verizon.netsense.sse.app

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream._
import akka.stream.scaladsl.{Flow, GraphDSL, RunnableGraph}
import com.verizon.netsense.connector.KafkaSettings
import com.verizon.netsense.sse.config.SSEConfigLoader._
import com.verizon.netsense.sse.model.{SSESubscribeBusinessAlertRequest, SSESubscribeBusinessAlertResponse}
import com.verizon.netsense.sse.service.{BaseSSEService, SubscribeProcess}
import com.verizon.netsense.sse.util.ObjectMapperUtil
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.ProducerRecord

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

/**
 * Created by muppasw on 2/12/18.
 */
object SSEBusinessSubscribeService extends BaseSSEService with KafkaSettings {
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

  lazy val msgUnmarshallingBusinessAlertFlow = Flow[ConsumerRecord[Array[Byte], Array[Byte]]]
    .mapAsync(parallel) { e =>
      ObjectMapperUtil.fromJsonByteArrayAsync[SSESubscribeBusinessAlertRequest](e.value)
    }
    .map { x =>
      x
    }
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  lazy val subscribeBusinessAlertProcess = Flow[Option[SSESubscribeBusinessAlertRequest]]
    .mapAsync(parallel) { e =>
      SubscribeProcess.validateAndProcessBusinessAlert(e.get)
    }
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  lazy val subscribeBusinessAlertKafkaSink = Flow[(SSESubscribeBusinessAlertResponse)]
    .mapAsync(parallel) { x =>
      Future(
        new ProducerRecord[Array[Byte], Array[Byte]](x.responsetopic, ObjectMapperUtil.toJson(x).getBytes())
      )
    }
    .to(sink())

  /*
   * SSEBusinessAlertSubscribe Process Graph
   */

  def sseBusinessAlertSubscribe: RunnableGraph[NotUsed] =
    RunnableGraph.fromGraph(g = GraphDSL.create() { implicit b =>
      import GraphDSL.Implicits._
      val Source             = b.add(restartableSubscriptionSource(sseBusinessFilterTopicParam)).out
      val UnMarshall         = b.add(msgUnmarshallingBusinessAlertFlow)
      val ValidateAndProcess = b.add(subscribeBusinessAlertProcess)
      val sink               = b.add(subscribeBusinessAlertKafkaSink).in

      Source ~> UnMarshall ~> ValidateAndProcess ~> sink

      ClosedShape
    })

}
