package com.verizon.netsense.sse.simulator.Event

import akka.Done
import akka.stream.{ActorMaterializer, ThrottleMode}
import akka.stream.scaladsl.{Sink, Source}
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.producer.ProducerRecord

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import com.verizon.netsense.sse.simulator.config.ConfigLoader._
import com.verizon.netsense.connector.KafkaSettings
import com.verizon.netsense.sse.simulator.data.SSEEventData
import com.verizon.netsense.utils.Logging

/**
 * Created by subrsi9 on 11/20/17.
 */
trait EventTrigger extends KafkaSettings with SSEEventData with Logging {

  override def kafkaBootstrapServers: String = kafkaServersParam

  override def kafkaClientId: String = kafkaClientIdParam

  override def kafkaGroupId: String = kafkaGroupIdParam

  override def kafkaOffsetReset: String = kafkaOffsetResetParam

  //val parallelism: Int=parallelism

  def sink: Sink[ProducerRecord[Array[Byte], Array[Byte]], Future[Done]]

  def triggerEvent(e: List[ProducerRecord[Array[Byte], Array[Byte]]],
                   timer: Timer,
                   parallelProcess: Int,
                   interval: Int)(implicit ec: ExecutionContext, mat: ActorMaterializer) =
    (1 to parallelProcess).par.map { x =>
      Source
      //.tick(initialDelay = msgInitialDelay second, interval = interval second, e)
        .fromIterator(() => Iterator from 0)
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
        .throttle(throttleLevel, 1.second, throttleLevel, ThrottleMode.Shaping)
        .runWith(sink)
    }

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

}
