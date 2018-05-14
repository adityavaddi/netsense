package com.verizon.netsense.services.bridge.flow

import java.net.InetAddress

import akka.NotUsed
import akka.stream.scaladsl.{RestartSink, RestartSource, Sink, Source}
import akka.stream.{ActorAttributes, Supervision}
import com.verizon.netsense.connector.{AmqpConnection, ConnectionRetry}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.services.bridge.Bridge._
import com.verizon.netsense.services.bridge.utils.ConfigLoader._
import com.verizon.netsense.utils.Logging
import io.scalac.amqp.Queue.DeclareOk
import io.scalac.amqp.{Message, Queue}
import nl.grons.metrics.scala.{Meter, MetricName}

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

/**
 * Created by wswaney on 6/6/17.
 */
object Rabbit extends /*App with*/ Instrumented with Logging {

  lazy val localhost     = InetAddress.getLocalHost
  lazy val metricsPrefix = localhost + ".bridge.rabbit-flow."
  implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  log.debug("FromRabbit Prefetch Count:" + fromRabbitPrefetchCount)

  val topicMatcher = """(?<=\.)(.*?)(?=\.)""".r

  override lazy val metricBaseName = MetricName(metricsPrefix)

  lazy val ampqConnection = AmqpConnection.getConnection
  private val declare: Future[DeclareOk] = ampqConnection.queueDeclare(Queue(fromRabbit_queue, autoDelete = false))
  lazy val fromQueue    = ampqConnection.consume(fromRabbit_queue, fromRabbitPrefetchCount)
  lazy val toExchange   = ampqConnection.publish(exchange = toRabbit_exchange, routingKey = toRabbit_routing)
  declare.onComplete {
    case Success(s) => log.info(s"Creating queue: $fromRabbit_queue was complete")
      fromRabbit_routing.foreach {
        x => ampqConnection.queueBind(fromRabbit_queue, fromRabbit_exchange, x).onComplete {
          case Success(ss) => log.info(s"Binding routing key: $x to $fromRabbit_exchange is complete")
          case Failure(ex) => log.error(s"Unable to bind $x to $fromRabbit_exchange exchange." + ex)
        }
      }
    case Failure(ex) => log.error(s"Unable to create queue $fromRabbit_queue "  + ex)
  }

  //Delete unused Sensor Queue from 3.0.5 Release (Split Brain)
  ampqConnection.queueDelete("sensor").onComplete {
    case Success(s) => log.info(s"Deleted Queue: sensor")
    case Failure(e) => log.info(s"Unable to delete queue: sensor" + e.getLocalizedMessage)
  }

  private[this] lazy val rabbitSourceMeter: Meter = metrics.meter("fromRabbit-meter")

  lazy val sourceDecider: Supervision.Decider = {
    case ex: NullPointerException => log.error("Null element in stream " + ex.getMessage)
      Supervision.Restart
    case ex =>
      log.error("Unhandled exception in stream source: " + ex.getClass.getName + ": " + ex.getMessage)
      Supervision.Restart
  }

  lazy val sinkDecider: Supervision.Decider = {
    case ex: NullPointerException => log.error("Null element in stream " + ex.getMessage); Supervision.Restart
    case ex =>
      log.error("Unhandled exception in stream sink: " + ex.getClass.getName + ": " + ex.getMessage)
      Supervision.Restart
  }

  private def topicName(r: String) = {
    val topicKey = topicMatcher.findFirstIn(r)
    topicKey match {
      case Some(t) => rabbitMQToKafkaTopicMappings.get(t)
      case _ => None
    }
  }

  lazy val rabbitSource = Source
    .fromPublisher(fromQueue)
    .map { msg =>
      rabbitSourceMeter.mark

      //Get the topic name using the message routing key
      val kafkaTopicName = topicName(msg.routingKey)
      if (kafkaTopicName.isEmpty){
        val errorMsg = s"Routing Key : ${msg.routingKey} could not be mapped to any of the topics ${rabbitMQToKafkaTopicMappings}"
        log.error(errorMsg)
        throw new Exception(errorMsg)
      }

      log.info(s"Routing Key : ${msg.routingKey} mapped to Kafka Topic: ${kafkaTopicName.get}")
      (kafkaTopicName.get, msg.routingKey, msg.message.body.toArray)
    }
    .withAttributes(ActorAttributes.supervisionStrategy(sourceDecider))

  lazy val rabbitSink =
    Sink.fromSubscriber(toExchange).withAttributes(ActorAttributes.supervisionStrategy(sinkDecider))


  var sourceRetryCount: Long = 0

  def incSourceRetryCounter(): Unit = sourceRetryCount = sourceRetryCount + 1

  lazy val restartableRabbitMQSource = RestartSource.withBackoff(
    minBackoff = 1.seconds,
    maxBackoff = 5.seconds,
    randomFactor = 0.2
  ) {
    () =>
      if (sourceRetryCount == 0) {
        incSourceRetryCounter()
        log.info("Starting Consumer for fromRabbit from Exchange: " + fromRabbit_exchange +
          ", Queue: " + fromRabbit_queue + ", with Routing Key: " + fromRabbit_routing)
      } else {
        incSourceRetryCounter()
        log.error("Reconnecting fromRabbit Consumer on failure count: " + sourceRetryCount + "from Exchange: "
          + fromRabbit_exchange + ", Queue: " + fromRabbit_queue + ", with Routing Key: " + fromRabbit_routing)
        ConnectionRetry.rabbitMQMaxRetryCheck(sourceRetryCount)(system)
      }
      rabbitSource
  }
    .withAttributes(Common.attributes)

  var sinkRetryCount: Long = 0

  def incSinkRetryCounter(): Unit = sinkRetryCount = sinkRetryCount + 1

  lazy val restartableRabbitMQSink: Sink[Message, NotUsed] = RestartSink.withBackoff(
    minBackoff = 1.seconds,
    maxBackoff = 5.seconds,
    randomFactor = 0.2
  ) {
    () =>
      if (sinkRetryCount == 0) {
        incSinkRetryCounter()
        log.info("Starting Publisher for ToRabbitMQ "  + toExchange + " routing: "
          + toRabbit_routing)
      } else {
        incSinkRetryCounter()
        log.error("Reconnecting RMQ Publisher on failure count: " + sinkRetryCount
          + " exchange: " + toExchange + " routing " + toRabbit_routing)
        ConnectionRetry.rabbitMQMaxRetryCheck(sinkRetryCount)(system)
      }
      rabbitSink
  }
}
