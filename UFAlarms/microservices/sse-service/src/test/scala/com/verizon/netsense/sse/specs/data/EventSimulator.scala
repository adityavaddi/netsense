package com.verizon.netsense.sse.specs.data

import akka.Done
import akka.stream.ActorMaterializer
import akka.stream.alpakka.mqtt.MqttMessage
import akka.stream.scaladsl.{Sink, Source}
import akka.util.ByteString
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.producer.ProducerRecord

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

/**
 * Created by sundach on 6/1/17.
 */
trait EventSimulator {
  def parallelism: Int
  def sink: Sink[ProducerRecord[Array[Byte], Array[Byte]], Future[Done]]

  def eventWithTopicName(n: Int, topicName: String, event: Int => Array[Byte]) =
    (1 to n).par
      .map(
        i => {

          new ProducerRecord[Array[Byte], Array[Byte]](
            topicName,
            event(i)
          )
        }
      )
      .toList

  def eventWithTopicPartition(n: Int, p: Int, topicName: String, event:  Int => Array[Byte]) =
    (n - 1 to n).par
      .map(
        i => {

          new ProducerRecord[Array[Byte], Array[Byte]](
            topicName,
            p,
            null,
            event(i)
          )
        }
      )
      .toList

  def mqttEventWithTopicName(n: Int, topicName: String, event: Int => Array[Byte]) =
    (1 to n).par
      .map(
        i => MqttMessage(topicName, ByteString(event(i)))
      )
      .toList
  def triggerEvent(e: List[ProducerRecord[Array[Byte], Array[Byte]]],
                   timer: Timer,
                   parallelProcess: Int)(implicit ec: ExecutionContext, mat: ActorMaterializer) =
    (1 to parallelProcess).par.map { x =>
      Source
        .tick(initialDelay = 1 micro, interval = 1 micro, e)
        .map(i => e)
        .mapConcat(identity)
        .mapAsync(parallelism)(
          e =>
            Future {
              timer.time {
                e
              }
          }
        )
        .runWith(sink)
    }

  def triggerMqttEvent(e: List[MqttMessage],
                       timer: Timer,
                       parallelProcess: Int,
                       sink: Sink[MqttMessage, Future[Done]])(implicit ec: ExecutionContext, mat: ActorMaterializer) =
    (1 to parallelProcess).par.map { x =>
      Source
        .tick(initialDelay = 1 micro, interval = 1 micro, e)
        .map(i => e)
        .mapConcat(identity)
        .mapAsync(parallelism)(
          e =>
            Future {
              timer.time {
                e
              }
          }
        )
        .runWith(sink)
    }

}
