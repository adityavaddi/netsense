package com.vz.ns.ts.service.service

import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.{Done, NotUsed}
import com.verizon.netsense.metrics.Instrumented
import com.vz.ns.ts.service.config.AMQPConnection
import com.vz.ns.ts.service.config.ConfigLoader._
import com.vz.ns.ts.service.util.{AkkaRuntime, MsgPackUtil}
import io.scalac.amqp.{Connection, Delivery}
import nl.grons.metrics.scala.Timer

import scala.concurrent.{ExecutionContext, Future}

/*
   NetSense event consumer.
   Reads the events from NetSense RabbitMQ and publishes to ThingSpace
 */
class NSEventConsumer(tsEventUtil: TsEventService, _akkaRuntime: AkkaRuntime) extends Logging with Instrumented {

  import _akkaRuntime._

  private val parallelism: Int = 5

  private[this] val RabbitMqSinkTimer: Timer = metrics.timer("RabbitMq-sink-timer")

  lazy private val connection: Connection = AMQPConnection.getConnection

  log.debug("QueueName: " + queueName)

  // Source which produces the Stream of [T] elements from the RabbitMq queue subscribed to queueName
  val consumerSourceFromMQ: Source[Delivery, NotUsed] = Source.fromPublisher(connection.consume(queueName, 100000))

  // Flow[Delivery] works non-blocking to transform Delivered elements into String
  val messagePackTransformFlow =
    Flow[Delivery].mapAsync(parallelism)(x => MsgPackUtil.unPacker(x.message.body.toArray))

  // Sink which takes the Stream of [T] elements from UpStream
  val consumerSinkForPushingToTS: Sink[Option[Object], Future[Done]] = Sink.foreachParallel(parallelism)(
    e => {
      RabbitMqSinkTimer.time()
      // post events to TS
      log.trace("events => " + e.get.asInstanceOf[Map[String, Any]])
      tsEventUtil.checkPayloadForNodeIdAndProcess(e.get.asInstanceOf[Map[String, Any]])
    }
  )

  lazy val nodeDataConsumerStream = consumerSourceFromMQ via messagePackTransformFlow to consumerSinkForPushingToTS
}

object NSEventConsumer {
  def apply(tsEventUtil: TsEventService, _akkaRuntime: AkkaRuntime): NSEventConsumer =
    new NSEventConsumer(tsEventUtil, _akkaRuntime)
}
