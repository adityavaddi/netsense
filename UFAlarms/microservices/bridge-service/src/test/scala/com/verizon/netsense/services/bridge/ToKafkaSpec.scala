//package com.verizon.netsense.services.bridge
//
//import akka.NotUsed
//import akka.actor.ActorSystem
//import akka.kafka.scaladsl.Consumer
//import akka.kafka.{ConsumerSettings, Subscriptions}
//import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
//import akka.stream.scaladsl.{Sink, Source}
//import akka.util.ByteString
//import com.verizon.netsense.services.bridge.flow.Rabbit
//import com.verizon.netsense.services.data.TestData
//import com.verizon.netsense.services.utils.BaseSpec
//import io.scalac.amqp.Message
//import net.manub.embeddedkafka.{EmbeddedKafka, EmbeddedKafkaConfig}
//import org.apache.kafka.clients.consumer.ConsumerConfig
//import org.apache.kafka.common.serialization.ByteArrayDeserializer
//import org.scalatest.BeforeAndAfterAll
//
//import scala.concurrent.Await
//import scala.concurrent.duration._
//
//class ToKafkaSpec extends BaseSpec with TestData with BeforeAndAfterAll {
//  private val kafkaPort     = 8767
//  private val zooKeeperPort = 8768
//
//  implicit val actorSystem = ActorSystem.create("MQTT-Spec")
//  implicit val materializer = ActorMaterializer(ActorMaterializerSettings(actorSystem))(actorSystem)
//
//  implicit val embeddedKafkaConfig = EmbeddedKafkaConfig(kafkaPort = kafkaPort, zooKeeperPort = zooKeeperPort)
//  val bootstrapServers             = s"localhost:${embeddedKafkaConfig.kafkaPort}"
//
//  val consumerSettings = ConsumerSettings(actorSystem, new ByteArrayDeserializer, new ByteArrayDeserializer)
//    .withBootstrapServers(bootstrapServers)
//    .withGroupId("test")
//    .withProperty(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest")
//
//  override def beforeAll(): Unit =
//    EmbeddedKafka.start()
//
////  "ToKafka bridge stream processing"
//  ignore should "consume messages from rabbitmq, process it and publish to kakfa broker" in {
//    //Publish messages to rmq broker
//    val count                        = 10
//    val messages                     = (1 to count)
//    val sink: Sink[Message, NotUsed] = Sink.fromSubscriber(Rabbit.fromExchange)
//
//    Source(messages).map(x => Message(ByteString(jsonPaylodwithChildByte).toArray)).runWith(sink)
//
//    //Run the bridge stream processing
//    Bridge.toKafka.toKafkaFlow.run()
//
//    //Read the messages from the kafka broker and validate it
//    val resultFuture =
//      Consumer.plainSource(consumerSettings, Subscriptions.topics("node.events.sensor")).take(count).runWith(Sink.seq)
//    val result = Await.result(resultFuture, 2.seconds)
//
//    //Assert the value
//    result.size must be(count)
//  }
//}
