package com.verizon.netsense.services.bridge

import java.net.InetAddress

import akka.stream.ClosedShape
import akka.stream.scaladsl.{GraphDSL, RunnableGraph}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.services.bridge.flow.{Common, Kafka, Rabbit}
import com.verizon.netsense.utils.Logging
import nl.grons.metrics.scala.MetricName

class ToKafka(kafka: Kafka) extends /*App with*/ Instrumented with Logging {

  val localhost     = InetAddress.getLocalHost
  val metricsPrefix = localhost + ".bridge.to-kafka."

  override lazy val metricBaseName = MetricName(metricsPrefix)

  lazy val toKafkaFlow = RunnableGraph.fromGraph(g = GraphDSL.create() { implicit b =>
    import GraphDSL.Implicits._

    //Source
    val Source = b.add(Rabbit.restartableRabbitMQSource).out
    //Flows
    val D2M = b.add(Common.messageDeliveryForCoreNode)
    //Sink
    val Sink = b.add(kafka.kafkaRestartableSinkWithTopicName).in

    Source ~> D2M  ~> Sink
    ClosedShape
  })
}

object ToKafka{
  def apply(kafka: Kafka): ToKafka = new ToKafka(kafka)
}
