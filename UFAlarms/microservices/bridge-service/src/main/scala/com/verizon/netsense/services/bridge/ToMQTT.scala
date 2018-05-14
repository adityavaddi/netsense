package com.verizon.netsense.services.bridge

import java.net.InetAddress

import akka.stream.alpakka.mqtt.MqttMessage
import akka.stream.scaladsl.{Flow, GraphDSL, Partition, RunnableGraph, Sink}
import akka.stream.{ActorAttributes, ClosedShape, Supervision}
import akka.util.ByteString
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.services.bridge.flow.{Kafka, MQTT}
import com.verizon.netsense.services.bridge.utils.ConfigLoader._
import com.verizon.netsense.utils.{Deserializer, Logging}
import msgpack4z.{JawnMsgpack, JawnUnpackOptions, MsgpackJavaPacker}
import nl.grons.metrics.scala.{Meter, MetricName, Timer}
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.common.record.TimestampType

/**
 * Created by wswaney on 6/6/17.
 */
class ToMQTT(kafka: Kafka) extends /*App with*/ Instrumented with Logging {

  val localhost     = InetAddress.getLocalHost
  val metricsPrefix = localhost + ".bridge.to-mqtt."
  type msgType = ConsumerRecord[Array[Byte], Array[Byte]]

  //log.error("Hostname: " + localhost)
  override lazy val metricBaseName = MetricName(metricsPrefix)

  private[this] val mqttSinkMeter: Meter = metrics.meter("toMQTT-meter")

  private[this] val toMQTTMSGTimer: Timer = metrics.timer("to-mqtt-message-timer")

  val parallelism = 1000

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

  val toMqttMessageFlow = Flow[msgType]
    .map { msg =>
      log.info("ToMqttMessage sent:" + new String(msg.value))
      mqttSinkMeter.mark
      // The messages sent to the device include the topic in the payload
      val message = msg.value()
      val decoded = Deserializer.msgpackMapper.readValue(message, classOf[Map[String, Object]])
      val topic   = decoded("p").toString

      new MqttMessage(topic, ByteString.fromArray(message))
    }
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

    //Function to decide if we should process/ignore this message based on the timestamp of the message.
    //The physical node should not receive the expired messages
    private def ignoreExpiredMessage(msg: msgType) = {
      var streamPartitionValue = 0
      if ((msg.timestampType() != TimestampType.CREATE_TIME)
        || (System.currentTimeMillis - msg.timestamp() > kafkaToMqttMsgExpiryTimeInSec * 1000)) {
        log.warn("ToMqttMessage discarded:" + new String(msg.value))
        streamPartitionValue = 1
      }
      streamPartitionValue
    }

  lazy val fromKafkaFlow = RunnableGraph.fromGraph(g = GraphDSL.create() { implicit b =>
    import GraphDSL.Implicits._
    //Source
    val kafkaSource = b.add(kafka.kafkaRestartableConsumerSource).out
    //Flow
    val J2MPack = b.add(toMqttMessageFlow)
    //Sink
    val sinkKafka = b.add(MQTT.restartableMqttsink).in
    val sinkIgnore = Sink.ignore
    //Partition
    val partition = b.add(Partition[msgType](2, ignoreExpiredMessage))

    kafkaSource ~> partition.in
                   partition.out(0) ~> J2MPack ~> sinkKafka
                   partition.out(1) ~> sinkIgnore
    ClosedShape
  })

}


object ToMQTT{
  def apply(kafka: Kafka): ToMQTT = new ToMQTT(kafka)
}
