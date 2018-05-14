package com.verizon.netsense.services.bridge

import java.net.InetAddress

import akka.stream.ClosedShape
import akka.stream.scaladsl.{GraphDSL, RunnableGraph}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.services.bridge.flow.{Common, Kafka, MQTT}
import com.verizon.netsense.utils.Logging
import nl.grons.metrics.scala.MetricName

/**
 * Created by wswaney on 6/5/17.
 */
class FromMQTT(kafka: Kafka) extends /*App with*/ Instrumented with Logging {

  val localhost     = InetAddress.getLocalHost
  val metricsPrefix = localhost + ".bridge.from-mqtt."

  //log.error("Hostname: " + localhost)
  override lazy val metricBaseName = MetricName(metricsPrefix)

  lazy val multipleTopicMqttGraph = RunnableGraph.fromGraph(g = GraphDSL.create() { implicit b =>
    import GraphDSL.Implicits._
    //Source
    val Source = b.add(MQTT.restartableMultipleMqttSources).out
    //Flows
    val MD = b.add(Common.messageDelivery)
    //Sink
    val outSink = b.add(kafka.kafkaRestartableSinkWithTopicName).in

    Source ~> MD ~> outSink

    ClosedShape

  })

}

object FromMQTT{
  def apply(kafka: Kafka): FromMQTT = new FromMQTT(kafka)
}
