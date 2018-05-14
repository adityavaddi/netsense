package com.vz.nsp.eventsimulator.service.actor

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Sink, Source}
import akka.stream.{ActorMaterializer, ThrottleMode}
import com.verizon.netsense.metrics.Instrumented
import com.vz.nsp.eventsimulator.service.config.ConfigLoader
import com.vz.nsp.eventsimulator.service.util._
import io.scalac.amqp.{Connection, Message, Routed}
import nl.grons.metrics.scala.Timer

import scala.concurrent.Future
import scala.concurrent.duration._

/**
 * Created by Jittara on 4/17/17.
 * NorthboundSimulator publishes messages to RabbitMQ
 */
object NorthboundPublisher extends Instrumented with Logging {
//Creating acor system, ExecutionContextExecutor and materializer
  implicit val system                          = ActorSystem.create("NorthboundSimulator-system")
  implicit val ec                              = system.dispatcher
  implicit val mat                             = ActorMaterializer()
  private[this] val northPublisherTimer: Timer = metrics.timer("northpublisher-timer")
  val random                                   = new scala.util.Random

  lazy private val connection: Connection = AMQPConnection.getConnection

  /**
   * A Publisher Runnable Graph which generates the sample events
   * & convert them to MsgPack Encoded String the Flow can be controlled using throttle and
   * Sink will be pushing the events to the RabbitMQ ${queueName} by a Routing key
   */
  def publishtoRabbitMQ {
    log.debug("Northbound simulator publishing to exchange " + ConfigLoader.exchange)
    Source
      .fromIterator(() => Iterator from 0)
      .mapAsync(ConfigLoader.parallel)(x => getMessage)
      .throttle(ConfigLoader.throttleLevel, 1.second, ConfigLoader.throttleLevel, ThrottleMode.shaping)
      //.take(ConfigLoader.messageCount)
      .map { x =>
        log.debug("Northbound simulator publishing message " + MsgPackUtil.fromBytes(x))
        northPublisherTimer.time()
        Routed(ConfigLoader.routingKey, Message(body = x))
      }
      .runWith(Sink.fromSubscriber(connection.publish(exchange = ConfigLoader.exchange)))
    log.debug("Northbound simulator publishing connected to RabbitMQ AMQP !!!")

  }

  def getMessage: Future[Array[Byte]] = {
    var messageTypes: Array[Future[Array[Byte]]] = Array(MessageFormat.generateDeviceEvent,
                                                         MessageFormat.generateActionEvent,
                                                         MessageFormat.generateLightEvent,
                                                         MessageFormat.generateScheduleEvent)
    messageTypes(random.nextInt(messageTypes.size))

  }
}
