package com.verizon.netsense.services.bridge.flow

import akka.actor.ActorSystem
import akka.kafka.scaladsl.Consumer
import akka.kafka.{ConsumerSettings, Subscriptions}
import akka.stream.alpakka.mqtt.MqttMessage
import akka.stream.scaladsl.{Sink, Source}
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import akka.util.ByteString
import com.verizon.netsense.services.bridge.Bridge
import com.verizon.netsense.services.bridge.utils.ConfigLoader._
import com.verizon.netsense.services.data.TestData
import com.verizon.netsense.services.utils.BaseSpec
import com.verizon.netsense.utils.Logging
import net.manub.embeddedkafka.{EmbeddedKafka, EmbeddedKafkaConfig}
import org.apache.kafka.clients.consumer.ConsumerConfig
import org.apache.kafka.common.serialization.ByteArrayDeserializer
import org.scalatest.BeforeAndAfterAll

import scala.concurrent.Await
import scala.concurrent.duration._

class FromMQTTSpec extends BaseSpec with TestData with BeforeAndAfterAll with Logging {

  implicit val actorSystem  = ActorSystem.create("MQTT-Spec")
  implicit val materializer = ActorMaterializer(ActorMaterializerSettings(actorSystem))(actorSystem)

  implicit val embeddedKafkaConfig = EmbeddedKafkaConfig(kafkaPort = 8767, zooKeeperPort = 8768)

  val bootstrapServers = s"localhost:${embeddedKafkaConfig.kafkaPort}"

  val consumerSettings = ConsumerSettings(actorSystem, new ByteArrayDeserializer, new ByteArrayDeserializer)
    .withBootstrapServers(bootstrapServers)
    .withGroupId("test")
    .withProperty(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest")

  override def beforeAll(): Unit = {
    EmbeddedKafka.start()
    if (EmbeddedKafka.isRunning) {
      println("Kafka still running");
    } else {
      println("Not running")
    }
  }

  /**
   * Consume message from Embedded MQTT topic
   */
//  "MQTT Flow"
  ignore should "be able to Produce and Consume MQTT message from the MQTT topic" in {
    val messageCount = 10
    val messages     = 1 to messageCount

    //Run the MQTT to kafka flow
    Bridge.fromMQTT.multipleTopicMqttGraph.run()

    Source(messages)
      .map(x => MqttMessage("v1/n100/out/va-v1/evt/dpark", ByteString(jsonPaylodwithChildByte)))
      .runWith(MQTT.mqttSink)

    //Read from kafka and validate it
    val kafkaMessagesFuture =
      Consumer.plainSource(consumerSettings, Subscriptions.topics(toKafka_topic)).take(messageCount).runWith(Sink.seq)
    val kafkaMessages = Await.result(kafkaMessagesFuture, 2.seconds)

    //Assert the value
    kafkaMessages.size must be(messageCount)
  }

}
