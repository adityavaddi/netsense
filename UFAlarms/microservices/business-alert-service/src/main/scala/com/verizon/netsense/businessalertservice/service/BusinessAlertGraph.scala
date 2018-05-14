package com.verizon.netsense.businessalertservice.service

import akka.actor.ActorSystem
import akka.stream.scaladsl.{GraphDSL, RunnableGraph}
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, _}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import com.verizon.netsense.businessalertservice.flow.{BusinessAlertFlow, KafkaConnector}
import com.verizon.netsense.businessalertservice.model.AppRequest
import com.verizon.netsense.businessalertservice.util.AkkaStreamUtil.akkaGraphDecider
import org.apache.kafka.clients.consumer.ConsumerRecord

class BusinessAlertGraph(kafkaConnector: KafkaConnector, businessAlertFlow: BusinessAlertFlow)
    extends Instrumented
    with Logging {

  implicit val queryServiceActorSystem = ActorSystem()
  implicit val mat = ActorMaterializer(
    ActorMaterializerSettings(queryServiceActorSystem)
      .withInputBuffer(Math.pow(2, 10).toInt, Math.pow(2, 20).toInt)
      .withSupervisionStrategy(akkaGraphDecider)
  )
  def queryServiceInit = queryServiceGraph.run()

  val queryServiceGraph = RunnableGraph.fromGraph(g = GraphDSL.create() { implicit b =>
    import GraphDSL.Implicits._

    // Source
    val Source = b.add(kafkaConnector.kafkaRestartableConsumerSource).out

    // Flows
    val messageUnpackFlow: FlowShape[ConsumerRecord[String, String], AppRequest] =
      b.add(businessAlertFlow.msgUnmarshallingFlow)
    val requestValidationFlow: FlowShape[AppRequest, AppRequest] = b.add(businessAlertFlow.messageValidationFlow)
    val businessAlertQuery                                       = b.add(businessAlertFlow.businessAlertProcessRequestFlow)

    // Sink
    val kafkaProducerSink = b.add(kafkaConnector.restartableProducerSinkBusinessAlert).in

    Source ~> messageUnpackFlow ~> requestValidationFlow ~> businessAlertQuery ~> kafkaProducerSink

    ClosedShape
  })

}

object BusinessAlertGraph {
  def apply(kafkaConnector: KafkaConnector, businessAlertUnMarshalling: BusinessAlertFlow): BusinessAlertGraph =
    new BusinessAlertGraph(kafkaConnector, businessAlertUnMarshalling)
}
