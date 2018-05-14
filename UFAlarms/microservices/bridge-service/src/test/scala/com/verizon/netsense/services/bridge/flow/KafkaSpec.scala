package com.verizon.netsense.services.bridge.flow

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import akka.util.ByteString
import com.verizon.netsense.services.bridge.Bridge
import com.verizon.netsense.services.utils.BaseSpec
import net.manub.embeddedkafka.{EmbeddedKafka, EmbeddedKafkaConfig}
import org.scalatest.BeforeAndAfterAll

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._

/**
 * Created by thimija on 8/8/17.
 */
class KafkaSpec extends BaseSpec with BeforeAndAfterAll {
  private val kafkaPort     = 8767
  private val zooKeeperPort = 8768

  implicit val system = ActorSystem()
  implicit val mat    = ActorMaterializer()
  implicit val ec     = ExecutionContext.Implicits.global

  implicit val embeddedKafkaConfig = EmbeddedKafkaConfig(kafkaPort = kafkaPort, zooKeeperPort = zooKeeperPort)
  val bootstrapServers             = s"localhost:${embeddedKafkaConfig.kafkaPort}"

  override def beforeAll(): Unit =
    EmbeddedKafka.start()

//  "Kafka Flow"
  ignore should "be able to write to a topic and read from the topic" in {
    val count    = 10
    val messages = 1 to count

    //Write messages to the kafka sink
    Source(1 to count)
      .map(_.toString)
      .map { elem =>
        ByteString(s"${elem}").toArray
      }
      .runWith(Bridge.kafka.kafkaSink)

    //Read messages to the kafka source
    val resultFuture = Bridge.kafka.kafkaSource.take(count).runWith(Sink.seq)

    //Get the result from the future
    val result = Await.result(resultFuture, 2.seconds)

    //Assert the value
    result.size must be(count)
  }

}
