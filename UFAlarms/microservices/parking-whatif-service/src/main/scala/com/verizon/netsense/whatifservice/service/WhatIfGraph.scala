package com.verizon.netsense.whatifservice.service

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Broadcast, GraphDSL, Merge, RunnableGraph}
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, _}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import com.verizon.netsense.whatifservice.flow.{KafkaConnector, WhatIfFlow}
import com.verizon.netsense.whatifservice.model.WhatIfSparkRequest
import com.verizon.netsense.whatifservice.model.casel.AppRequest
import org.apache.kafka.clients.consumer.ConsumerRecord
import com.verizon.netsense.whatifservice.util.AkkaStreamUtil.akkaGraphDecider

class WhatIfGraph(kafkaConnector: KafkaConnector, whatIfFlow: WhatIfFlow) extends Instrumented with Logging {

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
      b.add(whatIfFlow.msgUnmarshallingFlow)
    val requestValidationFlow: FlowShape[AppRequest, AppRequest] = b.add(whatIfFlow.messageValidationFlow)
    val whatIfQuery                                              = b.add(whatIfFlow.whatIfProcessRequestFlow)
    val sparkRequestFlow                                         = b.add(whatIfFlow.sparkRequestFlow)

    // Broadcast
    val broadcast = b.add(Broadcast[(AppRequest, String, WhatIfSparkRequest)](2))

    // Sink
    val kafkaProducerSink = b.add(kafkaConnector.restartableProducerSinkWhatIf).in

    val sparkResultToCassandraSink = b.add(whatIfFlow.sparkResultToCassandraSink).in

    Source ~> messageUnpackFlow ~> requestValidationFlow ~> whatIfQuery ~>
    broadcast ~> kafkaProducerSink
    broadcast ~> sparkRequestFlow ~> sparkResultToCassandraSink

    ClosedShape
  })

}

object WhatIfGraph {
  def apply(kafkaConnector: KafkaConnector, whatIfUnMarshalling: WhatIfFlow): WhatIfGraph =
    new WhatIfGraph(kafkaConnector, whatIfUnMarshalling)
}
