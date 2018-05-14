package com.verizon.netsense.services.gps.helper

import java.net.ConnectException

import akka.actor.ActorSystem
import akka.kafka.scaladsl.Consumer.Control
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.kafka.{ConsumerSettings, ProducerSettings, Subscriptions}
import akka.stream.Supervision
import akka.stream.scaladsl.{RestartSource, Sink, Source}
import akka.{Done, NotUsed}
import com.datastax.driver.core.exceptions.{NoHostAvailableException, ReadTimeoutException, WriteTimeoutException}
import com.fasterxml.jackson.core.JsonParseException
import com.fasterxml.jackson.databind.exc.MismatchedInputException
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.services.gps.exceptions.{CassandraDBException, OrgDataNotFoundException}
import com.verizon.netsense.services.gps.helper.KafkaConfig._
import com.verizon.netsense.utils.Logging
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.StringDeserializer

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

/**
 * Created by maidapr on 7/10/17.
 */
trait Common extends Instrumented with Logging {

  lazy val parallelism = 1000
  var sourceRetryCount =1

  private[this] val kafkaSourceTimer: Timer = metrics.timer("kafka-source-consumer")

  lazy val systemDecider: Supervision.Decider = {
    case ex: ConnectException         => log.error("Unable to connect to Graphite " + ex.getMessage); Supervision.Resume
    case ex: NoHostAvailableException => log.error("Unable to connect to DB " + ex.getMessage); Supervision.Resume
    case NonFatal(ex)                 => log.error("Exception found in the Stream " + ex.getMessage);ex.printStackTrace(); Supervision.Resume
    case ex: Exception                => log.error("Unhandled Exception seen " + ex.getMessage);ex.printStackTrace(); Supervision.Resume;
  }

  val dbDecider: Supervision.Decider = {
    case ex:CassandraDBException => log.error("Database Operation Failed: " + ex.getMessage); ex.printStackTrace(); Supervision.Resume
    case ex: OrgDataNotFoundException =>
      log.warn("No org data found for the node. Enriching gps event failed.\n" + ex.getMessage); Supervision.Resume
    case ex: WriteTimeoutException    => log.error("Unable to write the event to cassandra", ex); Supervision.Resume
    case ex: NoHostAvailableException =>
      log.error("Unable to establish cassandra connection ", ex); Supervision.Restart
    case ex: ReadTimeoutException => log.error("Unable to read the events from cassandra", ex); Supervision.Resume
    case ex: NullPointerException => log.error("Null element in stream " + ex); ex.printStackTrace();Supervision.Resume
    case ex: Exception            => log.error("Unhandled exception in stream " + ex); ex.printStackTrace();Supervision.Resume
  }

  lazy val flowDecider: Supervision.Decider = {
    case ex: JsonParseException => log.error("Unable to parse the JSON", ex.getMessage); Supervision.Resume
    case ex: MismatchedInputException =>
      log.error("Unable to construct from JSON payload", ex.getMessage); Supervision.Resume
    case ex: NullPointerException => log.error("Null element in stream " + ex.getMessage );ex.printStackTrace(); Supervision.Restart
    case ex: Exception            => log.error("Unhandled exception in stream" + ex);ex.printStackTrace(); Supervision.Resume
  }

  def configKafkaSource[K, V](_consumerSettings: ConsumerSettings[K, V],
                              _topics: Set[String]): Source[ConsumerRecord[K, V], Control] =
    Consumer.plainSource(_consumerSettings, Subscriptions.topics(_topics))

  def configKafkaProducer[K, V](_producerSettings: ProducerSettings[K, V]): Sink[ProducerRecord[K, V], Future[Done]] =
    Producer.plainSink(_producerSettings)

  def setupConsumerSettings(implicit _system: ActorSystem) =
    KafkaConnector.configKafkaConsumerSettings(bootStrapServers,
                                               groupId,
                                               new StringDeserializer,
                                               new StringDeserializer)

  def kafkaConsumerSource(
      topics: Set[String]
  )(implicit _system: ActorSystem): Source[ConsumerRecord[String, String], Control] =
    configKafkaSource(setupConsumerSettings, topics).map { e =>
      kafkaSourceTimer.time(e)
    }

  def createRestartKafkaSource(minBackOff: FiniteDuration, maxBackOff: FiniteDuration, randomFactor: Double,
                             topic: String)
                         (implicit _system: ActorSystem): Source[ConsumerRecord[String, String], NotUsed] = {
    RestartSource.withBackoff(minBackOff,maxBackOff,randomFactor) {
      () => {
        if(sourceRetryCount ==1) {
          sourceRetryCount = sourceRetryCount +1
          log.info("Starting Gps Consumer Source")
        }
        else {
          sourceRetryCount = sourceRetryCount + 1
          log.info("Restarting Gps Consumer Source with Failure Attempts: " + sourceRetryCount)
        }

        kafkaConsumerSource(Set(topic))
      }
    }
  }

}
