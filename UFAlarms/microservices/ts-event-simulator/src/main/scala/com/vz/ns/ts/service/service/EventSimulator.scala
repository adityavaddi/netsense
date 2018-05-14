package com.vz.ns.ts.service.service

import akka.NotUsed
import akka.stream.ThrottleMode
import akka.stream.scaladsl.{RunnableGraph, Sink, Source}
import com.vz.ns.ts.service.TsEventSimulator._
import com.vz.ns.ts.service.config.AMQPConnection
import com.vz.ns.ts.service.config.ConfigLoader._
import com.vz.ns.ts.service.model.DeviceEvent
import io.scalac.amqp.{Connection, Message, Routed}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

/**
 * Created by donthgo on 5/2/17.
 */
object EventSimulator {

  lazy private val connection: Connection = AMQPConnection.getConnection

  val listOfNodeIds = List.fill(noOfNodes)("node" + Random.nextInt().toString)

  log.debug("#### List of Node Id's: " + listOfNodeIds.mkString(" , "))

  log.debug("#### Sensor's List: " + sensorsArray.mkString(" , "))

  log.debug("#### exchangeName: " + exchangeName)

  log.debug("####routingKey: " + routingKey)

  /**
   * Helper to generate the Sample device events from DeviceEvent
   *
   * @return - Future Value of MsgPack encoded Array[Byte] of simulated DeviceEvent
   */
  def generateRandomEvent(implicit ec: ExecutionContext): Future[Array[Byte]] =
    MsgPacker.packerAsync(
      DeviceEvent(Random.shuffle(listOfNodeIds).head,
                  Random.shuffle(sensorsArray.toList).head.toString,
                  Random.nextInt((1000 - 0) + 1).toLong,
                  System.currentTimeMillis())
    )

  lazy private val exchange = {
    log.info("Connecting to AMQP for publisher")
    connection.publish(exchange = exchangeName)
  }

  /**
   * A Publisher Runnable Graph which generates the sample device event [DeviceEvent]
   * & convert them to MsgPack Encoded String the Flow can be controlled using throttle and
   * Sink will be pushing the events to the RabbitMQ ${queueName} by a Routing key
   */
  val eventPublisher: RunnableGraph[NotUsed] = Source
    .fromIterator(() => Iterator from 0)
    .mapAsync(2)(x => generateRandomEvent)
    .throttle(10, 1.second, 10, ThrottleMode.shaping)
    .take(noOfEvents)
    .map { x =>
      log.debug("=> message published: " + x)
      Routed(routingKey, Message(body = x))
    }
    .to(Sink.fromSubscriber(exchange))

}
