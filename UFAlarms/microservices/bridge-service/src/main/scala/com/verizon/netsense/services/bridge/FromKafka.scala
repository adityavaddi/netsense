package com.verizon.netsense.services.bridge

import java.net.InetAddress

import akka.stream.scaladsl.{Flow, GraphDSL, RunnableGraph}
import akka.stream.{ActorAttributes, ClosedShape, Supervision}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.services.bridge.flow.{Kafka, Rabbit}
import com.verizon.netsense.utils.Logging
import io.scalac.amqp.Message
import msgpack4z.{JawnMsgpack, JawnUnpackOptions, MsgpackJavaPacker}
import nl.grons.metrics.scala.{Meter, MetricName, Timer}
import org.apache.kafka.clients.consumer.ConsumerRecord

/**
 * Created by wswaney on 5/8/17.
 */
class FromKafka(kafka: Kafka) extends /*App with*/ Instrumented with Logging {

  val localhost     = InetAddress.getLocalHost
  val metricsPrefix = localhost + ".bridge.from-kafka."

  //log.error("Hostname: " + localhost)
  override lazy val metricBaseName = MetricName(metricsPrefix)

  private[this] val rabbitSinkMeter: Meter = metrics.meter("toRabbit-meter")

  private[this] val toRabbitMSGTimer: Timer = metrics.timer("to-rabbit-message-timer")

  val parallelism = 1000
  var n           = 0

  private val codec = JawnMsgpack.jValueCodec(JawnUnpackOptions.default)
  def json2bytes(jsonString: String): Array[Byte] = {
    val json = jawn.Parser.parseUnsafe[jawn.ast.JValue](jsonString)
    codec.toBytes(json, new MsgpackJavaPacker)
  }

  val flowDecider: Supervision.Decider = {
    case ex: NullPointerException => log.error("Null element in stream " + ex.getMessage); Supervision.Restart
    case ex =>
      log.error("Unhandled exception in stream flow: " + ex.getClass.getName + ": " + ex.getMessage);
      Supervision.Restart
  }

  val toMessageFlow = Flow[ConsumerRecord[Array[Byte], Array[Byte]]]
    .map { msg =>
      n = n + 1
      if (n % 25 == 0) log.info(new String(msg.value))
      val result =
        toRabbitMSGTimer.time {
          //val b = json2bytes(msg.value)
          Message(msg.value)
        }
      rabbitSinkMeter.mark
      result
    }
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  lazy val fromKafkaFlow = RunnableGraph.fromGraph(g = GraphDSL.create() { implicit b =>
    import GraphDSL.Implicits._

    //Source
    val Source = b.add(kafka.kafkaRestartableConsumerSource).out
    //Flows
    val J2MPack = b.add(toMessageFlow)
    //Sink
    val Sink = b.add(Rabbit.restartableRabbitMQSink).in

    Source ~> J2MPack ~> Sink
    ClosedShape
  })

}

object FromKafka{
  def apply(kafka: Kafka): FromKafka = new FromKafka(kafka)
}
