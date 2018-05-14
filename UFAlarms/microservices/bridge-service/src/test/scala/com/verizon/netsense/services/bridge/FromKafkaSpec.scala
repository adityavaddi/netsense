package com.verizon.netsense.services.bridge

import akka.actor.ActorSystem
import akka.kafka.scaladsl.Producer
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import akka.util.ByteString
import com.verizon.netsense.services.bridge.flow.Rabbit
import com.verizon.netsense.services.utils.BaseSpec
import net.manub.embeddedkafka.{EmbeddedKafka, EmbeddedKafkaConfig}
import org.apache.kafka.clients.producer.ProducerRecord
import org.scalatest.BeforeAndAfterAll

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}

class FromKafkaSpec extends BaseSpec with BeforeAndAfterAll {
  private val kafkaPort     = 8767
  private val zooKeeperPort = 8768

  implicit val embeddedKafkaConfig = EmbeddedKafkaConfig(kafkaPort = kafkaPort, zooKeeperPort = zooKeeperPort)
  val bootstrapServers             = s"localhost:${embeddedKafkaConfig.kafkaPort}"

  override def beforeAll(): Unit =
    EmbeddedKafka.start()

//  "FromKafka stream processing"
  ignore should "consume messages from kafka, process it and publish to rabbitmq broker" in {
    implicit val system = ActorSystem()
    implicit val mat    = ActorMaterializer()
    implicit val ec     = ExecutionContext.Implicits.global

    val count = 10
    Source(1 to count)
      .map(_.toString)
      .map { elem =>
        new ProducerRecord[Array[Byte], Array[Byte]]("node.commands.sensor", ByteString(s"${elem}").toArray)
      }
      .runWith(Producer.plainSink(Bridge.kafka.producerSettings))

    Bridge.fromKafka.fromKafkaFlow.run()

    Rabbit.ampqConnection.queueBind("sensor", "node.commands", "*")
    val eventualMessages = Rabbit.rabbitSource.take(count).runWith(Sink.seq)
    val resultFuture     = Await.result(eventualMessages, 2.seconds)
    val result           = resultFuture.map(x => new String(x._2))
    result.size must be(count)
  }

}
