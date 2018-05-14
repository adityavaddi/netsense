package com.verizon.netsense.test.kafka

/*
 * Copyright (C) 2014 - 2016 Softwaremill <http://softwaremill.com>
 * Copyright (C) 2016 Lightbend Inc. <http://www.lightbend.com>
 */

import java.util.UUID

import akka.actor.ActorSystem
import akka.kafka.ProducerMessage.Message
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.kafka.{ConsumerSettings, ProducerSettings, Subscriptions}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.stream.testkit.TestSubscriber
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestKit
import akka.{Done, NotUsed}
import net.manub.embeddedkafka.{EmbeddedKafka, EmbeddedKafkaConfig}
import org.apache.kafka.clients.consumer.ConsumerConfig
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, ByteArraySerializer, StringDeserializer}
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest._

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.language.postfixOps

abstract class BaseKafkaTest
    extends TestKit(ActorSystem("IntegrationSpec"))
    with WordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with BeforeAndAfterEach
    with TypeCheckedTripleEquals {

  implicit val embeddedKafkaConfig = EmbeddedKafkaConfig(9099, 2188)

  val bootstrapServers = s"localhost:${embeddedKafkaConfig.kafkaPort}"
  val InitialMsg       = "initial msg in topic, required to create the topic before any consumer subscribes to it"

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    EmbeddedKafka.start()
  }

  override def afterAll(): Unit = {
    shutdown(system, 30.seconds)
    EmbeddedKafka.stop()
    super.afterAll()
  }

  def uuid = UUID.randomUUID().toString

  def createTopic1: String = "login"
  def createTopic2: String = "sensor"
  def createTopic3: String = "alert"
  def createTopic4: String = "corenode.sensor"
  def createTopic5: String = "gps"
  def createTopic6: String = "businessalert"
  def createTopic7: String = "connectionstatus"

  def createGroup: String = "group-" + UUID.randomUUID().toString
  val partition0          = 0

  val producerSettings = ProducerSettings(system, new ByteArraySerializer, new ByteArraySerializer)
    .withBootstrapServers(bootstrapServers)

  /**
   * Produce messages to topic using specified range and return
   * a Future so the caller can synchronize consumption.
   */
  def produce(topic: String, range: Range, event: Array[Byte])(implicit mat: ActorMaterializer): Future[Done] = {
    val source = Source(range)
      .map(n => {
        val record = new ProducerRecord(topic, partition0, null: Array[Byte], event)

        Message(record, NotUsed)
      })
      .viaMat(Producer.flow(producerSettings))(Keep.right)

    source.runWith(Sink.ignore)
  }
  def sink = Producer.plainSink(producerSettings)

  def consumerSetting(group: String) =
    ConsumerSettings(system, new ByteArrayDeserializer, new StringDeserializer)
      .withBootstrapServers(bootstrapServers)
      .withGroupId(group)
      .withProperty(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest")
      .withWakeupTimeout(10 seconds)
      .withMaxWakeups(10)

  def createConsumer(group: String, topicName: String*) =
    Consumer.plainSource(consumerSetting("group1"), Subscriptions.topics(topicName.toSet))

  def createProbe(
      consumerSettings: ConsumerSettings[Array[Byte], String],
      topic: String
  )(implicit mat: ActorMaterializer): TestSubscriber.Probe[String] =
    Consumer
      .plainSource(consumerSettings, Subscriptions.topics(topic))
      .filterNot(_.value == InitialMsg)
      .map(_.value)
      .runWith(TestSink.probe)

}
