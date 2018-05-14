package com.verizon.netsense.helper

import akka.Done
import akka.actor.ActorSystem
import akka.kafka.{ConsumerSettings, ProducerSettings, Subscriptions}
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.kafka.scaladsl.Consumer.Control
import akka.stream.Supervision
import akka.stream.scaladsl.{Sink, Source}
import com.datastax.driver.core.exceptions.{NoHostAvailableException, ReadTimeoutException, WriteTimeoutException}
import com.fasterxml.jackson.annotation.JsonInclude.Include
import com.fasterxml.jackson.core.{JsonParseException, JsonProcessingException}
import com.fasterxml.jackson.databind.{DeserializationFeature, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import com.verizon.netsense.config.OTAKafkaConfig
import com.verizon.netsense.utils.Logging
import org.apache.kafka.clients.consumer.{ConsumerConfig, ConsumerRecord}
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{Deserializer, Serializer}
import scala.concurrent.duration._
import scala.concurrent.Future

trait StreamHelper extends Logging {

  //  Supervision
  val cassandraDecider: Supervision.Decider = {
    case ex: NoHostAvailableException => log.error("Unable to establish cassandra connection ", ex); Supervision.Restart
    case ex: WriteTimeoutException => log.error("Unable to write to cassandra", ex); Supervision.Resume
    case ex: ReadTimeoutException => log.error("Unable to read from cassandra", ex); Supervision.Resume
    case ex: NullPointerException => log.error("Null element in stream " + ex); Supervision.Resume
    case ex: Exception =>
      log.error("Unhandled exception in stream " + ex)
      ex.printStackTrace()
      Supervision.Restart
  }

  val flowDecider: Supervision.Decider = {
    case ex: JsonParseException => Supervision.Resume
    case ex: NullPointerException => log.error("Null element in stream " + ex.getMessage); Supervision.Restart
    case ex: Exception =>
      log.error("Unhandled exception in stream " + ex)
      ex.printStackTrace()
      Supervision.Restart
  }


  //  Kafka Stuff
  def configKafkaSource[K, V](_consumerSettings: ConsumerSettings[K, V],
                              _topics: Set[String]): Source[ConsumerRecord[K, V], Control] =
    Consumer.plainSource(_consumerSettings, Subscriptions.topics(_topics))

  def configKafkaSink[K, V](_producerSettings: ProducerSettings[K, V]): Sink[ProducerRecord[K, V], Future[Done]] =
    Producer.plainSink(_producerSettings)

  def configKafkaConsumerSettings[K, V](_bootstrapServer: String = OTAKafkaConfig.bootStrapServers,
                                        _groupId: String,
                                        _keySerializer: Deserializer[K],
                                        _valueSerializer: Deserializer[V]
                                       )(implicit _system: ActorSystem): ConsumerSettings[K, V] =
    ConsumerSettings(system = _system, _keySerializer, _valueSerializer)
      .withBootstrapServers(_bootstrapServer)
      .withGroupId(_groupId)
      .withProperty(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "latest")
      .withProperty(ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG, "true")
      .withProperty(ConsumerConfig.AUTO_COMMIT_INTERVAL_MS_CONFIG, "5000")
      .withProperty(ConsumerConfig.MAX_POLL_INTERVAL_MS_CONFIG, "30000")
      .withProperty(ConsumerConfig.MAX_POLL_RECORDS_CONFIG, "2000")
      .withProperty(ConsumerConfig.SESSION_TIMEOUT_MS_CONFIG, "10000")
      .withProperty(ConsumerConfig.HEARTBEAT_INTERVAL_MS_CONFIG, "3000")
      .withProperty(ConsumerConfig.METADATA_MAX_AGE_CONFIG, "60000")
      .withProperty(ConsumerConfig.RECONNECT_BACKOFF_MS_CONFIG, "10000")
      .withProperty(ConsumerConfig.PARTITION_ASSIGNMENT_STRATEGY_CONFIG,
        "org.apache.kafka.clients.consumer.RoundRobinAssignor")
      .withProperty(ConsumerConfig.CONNECTIONS_MAX_IDLE_MS_CONFIG, "604800000")
      .withWakeupTimeout(30 seconds)
      .withMaxWakeups(60000)

  def configKafkaProducerSettings[K, V](_bootStrapServers: String = OTAKafkaConfig.bootStrapServers,
                                        _keySerializer: Serializer[K],
                                        _valueSerializer: Serializer[V]
                                       )(implicit _system: ActorSystem): ProducerSettings[K, V] =
    ProducerSettings(system = _system, _keySerializer, _valueSerializer)
      .withBootstrapServers(_bootStrapServers)
      .withParallelism(10000)
      .withCloseTimeout(60 seconds)

  //  JSON Stuff

  private val mapper = new ObjectMapper() with ScalaObjectMapper
  mapper.registerModule(DefaultScalaModule)
  mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
  mapper.setSerializationInclusion(Include.NON_ABSENT)

  def fromJSONByteArrayToObj[T](json: Array[Byte])(implicit m: Manifest[T]): T =
    mapper.readValue[T](json)

}
