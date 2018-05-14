package com.verizon.netsense.services

import akka.Done
import akka.actor.ActorSystem
import akka.kafka.{ConsumerSettings, ProducerSettings, Subscriptions}
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import akka.stream.testkit.TestSubscriber
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestKit
import com.verizon.netsense.config.OTAKafkaConfig
import net.manub.embeddedkafka.{EmbeddedKafka, EmbeddedKafkaConfig}
import org.apache.kafka.clients.consumer.ConsumerConfig
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer}
import org.scalatest._

import scala.concurrent.{Await, Future}

class KafkaSpec extends TestKit(ActorSystem("KafkaSpec-System"))
  with FunSuiteLike
  with MustMatchers
  with BeforeAndAfterAll
  with BeforeAndAfterEach {

  implicit val embeddedKafkaConfig = EmbeddedKafkaConfig(kafkaPort = 8777, zooKeeperPort = 8778)
  implicit val mat = ActorMaterializer()(system)
  implicit val ec = system.dispatcher

  val bootstrapServers = s"localhost:${embeddedKafkaConfig.kafkaPort}"
  val groupId = OTAKafkaConfig.apiGroupId
  val topic = OTAKafkaConfig.otaTopic

  //  trait KafkaConf {
  val consumerSettings = ConsumerSettings(system, new ByteArrayDeserializer, new ByteArrayDeserializer)
    .withBootstrapServers(bootstrapServers)
    .withGroupId(groupId)
    .withProperty(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest")


  val producerSettings = ProducerSettings(system, new ByteArraySerializer, new ByteArraySerializer)
    .withBootstrapServers(bootstrapServers)

  def createProbe(): TestSubscriber.Probe[Array[Byte]] = {
    Consumer
      .plainSource(consumerSettings, Subscriptions.topics(topic))
      .map(_.value)
      .runWith(TestSink.probe)
  }

  def produceEvent(event: String): Future[Done] = {
    Source
      .single(event)
      .map(e => {
        new ProducerRecord(topic, e.getBytes(), e.getBytes())
      })
      .runWith(Producer.plainSink(producerSettings))
  }

  //  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    EmbeddedKafka.start()
  }

  override def afterAll(): Unit = {
    super.afterAll()
    EmbeddedKafka.stop()
    system.terminate()
  }

  test("it should do something") {
    Await.result(produceEvent("MARKER"), remainingOrDefault)

    val probe = createProbe()

    probe.request(1)
    val msg = probe.expectNext()

    assert(new String(msg) == "MARKER")

    probe.cancel()

  }

}
