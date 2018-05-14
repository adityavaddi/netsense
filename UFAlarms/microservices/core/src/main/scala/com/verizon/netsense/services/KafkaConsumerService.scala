package com.verizon.netsense.services

import akka.event.{Logging => ALogging}
import akka.actor.ActorSystem
import akka.kafka.{ConsumerSettings, Subscriptions}
import akka.kafka.scaladsl.{Consumer}
import akka.stream.{ActorAttributes, ActorMaterializer, ActorMaterializerSettings, Supervision}
import akka.stream.scaladsl.{Keep, RestartSource, Sink, Source}
import com.verizon.netsense.connector.KafkaConnection
import com.verizon.netsense.metrics.{Instrumented, MetricsActor}
import com.verizon.netsense.utils.Logging
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.consumer.{ConsumerConfig, ConsumerRecord}
import org.apache.kafka.common.KafkaException
import org.apache.kafka.common.serialization.ByteArrayDeserializer
import org.json4s.DefaultFormats

import scala.util.{Failure, Success}
import scala.util.control.NonFatal
import scala.concurrent.duration._

/**
 * Created by davor on 5/24/17.
 */
class KafkaConsumerService(val connection: KafkaConnection,
                           val kafkaTopic: String,
                           val handler: KafkaConsumerMessageHandler)(implicit val system: ActorSystem)
  extends Instrumented
    with Logging
{


  log.debug(s"Kafka topic: $kafkaTopic")

  private[this] val isKafkaConsumerTimer = metrics.timer("kafka-consumer-timer")

  //private[this] val log = Logging(system.eventStream, "kafka-consumer-service")

  lazy val mainSupervisor: Supervision.Decider = {
    case NonFatal(ex) =>
      ex.getStackTrace.foreach(x => log.error(x.toString))
      log.error("Exception found in the Stream " + ex.getMessage); Supervision.Restart
    //case ex: Exception => log.error("Unhandled Exception seen " + ex.getMessage); Supervision.stop;
    case tr: Throwable =>
      tr.getStackTrace.foreach(x => log.error(x.toString))
      log.error("Unhandled Exception seen " + tr.getMessage); Supervision.stop;
  }
  implicit val materializer = ActorMaterializer(
    ActorMaterializerSettings(system)
      .withSupervisionStrategy(mainSupervisor)
  )(system)

  lazy val sourceDecider: Supervision.Decider = {
    case ex: NullPointerException => log.error("Null element in stream " + ex.getMessage); Supervision.Restart
    case ex: KafkaException =>
      log.error("KafkaException in Consumer: " + ex.getClass.getName + ": " + ex.getMessage); Supervision.Restart
    case ex =>
      log.error("Unhandled exception in stream source: " + ex.getClass.getName + ": " + ex.getMessage)
      Supervision.Restart
  }

  lazy val attributes =
    ActorAttributes
      .logLevels(onElement = ALogging.DebugLevel, onFinish = ALogging.DebugLevel, onFailure = ALogging.DebugLevel)
      .and(
        ActorAttributes
          .supervisionStrategy(sourceDecider)
      )


  implicit val formats = DefaultFormats

  implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  val parallelism = 1000

  private[this] val kafkaSourceTimer: Timer = metrics.timer("kafka-source-consumer")

  val consumerSettings = ConsumerSettings(system = system,
    keyDeserializer = new ByteArrayDeserializer,
    valueDeserializer = new ByteArrayDeserializer)
    .withBootstrapServers(connection.bootStrapServers)
    .withGroupId(connection.groupId)
    .withProperty(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "latest")
    .withProperty(ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG, "true")
    .withProperty(ConsumerConfig.AUTO_COMMIT_INTERVAL_MS_CONFIG, "5000")
    .withProperty(ConsumerConfig.MAX_POLL_INTERVAL_MS_CONFIG, "30000")
    .withProperty(ConsumerConfig.MAX_POLL_RECORDS_CONFIG, "2000")
    .withProperty(ConsumerConfig.SESSION_TIMEOUT_MS_CONFIG, "10000")
    .withProperty(ConsumerConfig.HEARTBEAT_INTERVAL_MS_CONFIG, "3000")
    .withProperty(ConsumerConfig.METADATA_MAX_AGE_CONFIG, "60000")
    .withProperty(ConsumerConfig.RECONNECT_BACKOFF_MS_CONFIG, "10000")
    .withProperty(ConsumerConfig.PARTITION_ASSIGNMENT_STRATEGY_CONFIG, "org.apache.kafka.clients.consumer.RoundRobinAssignor")
    .withProperty(ConsumerConfig.CONNECTIONS_MAX_IDLE_MS_CONFIG, "604800000")
    .withWakeupTimeout(30 seconds)
    .withMaxWakeups(60000)

  lazy val kafkaSource = Consumer
    .plainSource(consumerSettings, Subscriptions.topics(kafkaTopic))
    .map { msg =>
      kafkaSourceTimer.time()
      msg
    }
    .withAttributes(ActorAttributes.supervisionStrategy(sourceDecider))

  var sourceRetryCount: Long = 0

  def incRetryCounter(): Unit = sourceRetryCount = sourceRetryCount + 1

  lazy val kafkaRestartableConsumerSource = RestartSource.withBackoff(
    minBackoff = 1.seconds,
    maxBackoff = 5.seconds,
    randomFactor = 0.2
  ) { () =>
    if (sourceRetryCount == 0) {
      incRetryCounter()
      log.info("Starting Kafka Consumer fromKafka: " + connection.bootStrapServers)
    }
    else {
      {
        incRetryCounter()
        log.error("Reconnecting kafka consumer on failure " + sourceRetryCount + " broker: " + connection.bootStrapServers)
      }
    }
    kafkaSource
  }.withAttributes(attributes)

  val messageReceiverSink = kafkaRestartableConsumerSource
    .mapAsync(parallelism) { elem =>
      isKafkaConsumerTimer.time()
      val msg = new String(elem.value)
      log.info(s"message received: $msg")
      handler.messageReceive(msg)
    }
    .toMat(Sink.ignore)(Keep.right)

  var done = messageReceiverSink.run()

  done.onComplete {
    case Failure(ex) =>
      log.debug(s"Stream stopped with FAILURE: ${ex.toString}")
//      Thread.sleep(60000)
      new KafkaConsumerService(connection, kafkaTopic, handler)
      this.finalize()
    case Success(ex) => log.debug(s"Message Received SUCCESS!!!!!!!!!!!!! ${ex.toString}")
  }
}
