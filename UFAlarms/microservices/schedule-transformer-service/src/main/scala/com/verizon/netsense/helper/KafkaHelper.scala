package com.verizon.netsense.helper

import akka.kafka.Subscriptions
import akka.kafka.scaladsl.Consumer
import akka.stream.ActorAttributes
import akka.stream.scaladsl.RestartSource
import com.verizon.netsense.connector.KafkaConnection
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.util.Common
import com.verizon.netsense.utils.Logging
import nl.grons.metrics.scala.Timer
import scala.concurrent.duration._


class KafkaHelper(kafkaConnection : KafkaConnection, metricName : String) extends Instrumented
  with Common
  with Logging{

  //for metrics
  private[this] val kafkaSourceTimer: Timer = metrics.timer(metricName)

  //Source Kafka
  private def kafkaConsumerSource(topicName: String, groupId: String) = Consumer
    .plainSource(kafkaConnection.consumerSettings.withGroupId(groupId), Subscriptions.topics(topicName))
    .map { msg =>
      kafkaSourceTimer.time {
        val str: String = new String(msg.value(), "UTF-8")
        log.info("Kafka Source message: " + str)
        msg.value()
      }
    }.withAttributes(ActorAttributes.supervisionStrategy(sourceDecider))

  var sourceRetryCount: Long = 0
  def incSourceRetryCounter(): Unit = sourceRetryCount += 1

  def restartableConsumerSource(topicName: String, groupId: String) =
    RestartSource.withBackoff(
      minBackoff = 1.seconds,
      maxBackoff = 5.seconds,
      randomFactor = 0.2
    ) { () =>
      if (sourceRetryCount == 0) {
        incSourceRetryCounter()
        log.info("Starting Consumer for Kafka " + kafkaConnection.kafkaHost
          +  " groupId: " + kafkaConnection.groupId)
      } else {
        incSourceRetryCounter()
        log.error("Restarting Consumer for Kafka " + kafkaConnection.kafkaHost
          +  " groupId: " + kafkaConnection.groupId)
      }
      kafkaConsumerSource(topicName, groupId)
    }.withAttributes(ActorAttributes.supervisionStrategy(sourceDecider))
}
