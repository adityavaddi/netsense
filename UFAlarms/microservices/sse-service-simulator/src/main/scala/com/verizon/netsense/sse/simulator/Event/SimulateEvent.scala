package com.verizon.netsense.sse.simulator.Event

import akka.Done
import akka.actor.ActorSystem
import akka.kafka.scaladsl.Producer
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Sink
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.sse.simulator.config.ConfigLoader._
import com.verizon.netsense.sse.simulator.data.SSEEventData
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.producer.ProducerRecord

import scala.concurrent.{ExecutionContext, Future}

/**
 * Created by subrsi9 on 11/20/17.
 */
object SimulateEvent extends EventTrigger with SSEEventData with Instrumented {

  implicit def actorSystem = ActorSystem.create("SSE-Simulator-System")

  implicit val materializer = ActorMaterializer()

  implicit val ec = ExecutionContext.Implicits.global

  private[this] val loginReqMetrics: Timer              = metrics.timer("SSE-simulator-kafka-loginReq")
  private[this] val sensorSampleMetrics: Timer          = metrics.timer("SSE-simulator-kafka-sensor")
  private[this] val corenodeSensorSampleMetrics: Timer  = metrics.timer("SSE-simulator-kafka-corenode.sensor")
  private[this] val deviceAlarmMetrics: Timer           = metrics.timer("SSE-simulator-kafka-deviceAlarm")
  private[this] val busAlertMetrics: Timer              = metrics.timer("SSE-simulator-kafka-businessAlert")
  private[this] val connectionStatusMetrics: Timer      = metrics.timer("SSE-simulator-kafka-connStatus")
  private[this] val gpsSampleMetrics: Timer             = metrics.timer("SSE-simulator-kafka-gpsSample")
  private[this] val sseSubscribeMetrics: Timer          = metrics.timer("SSE-simulator-kafka-subscribe")
  private[this] val sseHeartBeatMetrics: Timer          = metrics.timer("SSE-simulator-kafka-heartbeat")
  private[this] val sseDisconnectMetrics: Timer         = metrics.timer("SSE-simulator-kafka-disconnect")
  private[this] val sseBusinessSubscribeMetrics: Timer  = metrics.timer("SSE-simulator-kafka-businesssubscribe")
  private[this] val sseBusinessHeartBeatMetrics: Timer  = metrics.timer("SSE-simulator-kafka-businessheartbeat")
  private[this] val sseBusinessDisconnectMetrics: Timer = metrics.timer("SSE-simulator-kafka-businessdisconnect")

  def sink: Sink[ProducerRecord[Array[Byte], Array[Byte]], Future[Done]] =
    Producer.plainSink(producerSettings)

  def sseLoginEvent =
    (1 to msgCount).par
      .map(i => {
        triggerEvent(eventWithTopicName(numerOfNode, loginKafkaTopic, constructLoginReq(_).getBytes()),
                     loginReqMetrics,
                     parProcess,
                     msgInterval)
      })

  def sseSensorSampleEvent =
    (1 to msgCount).par
      .map(i => {
        triggerEvent(eventWithTopicName(numerOfNode, sensorKafkaTopic, constructSensor(_).getBytes()),
                     sensorSampleMetrics,
                     parProcess,
                     msgInterval)
      })

  def sseCoreNodeSensorSampleEvent =
    (1 to msgCount).par
      .map(i => {
        triggerEvent(eventWithTopicName(numerOfNode, corenodesensorKafkaTopic, constructCorenodeSensor(_).getBytes()),
                     corenodeSensorSampleMetrics,
                     parProcess,
                     msgInterval)
      })

  def sseDeviceAlarmEvent =
    (1 to msgCount).par
      .map(i => {
        triggerEvent(eventWithTopicName(numerOfNode, alertKafkaTopic, constructDeviceAlarm(_).getBytes()),
                     deviceAlarmMetrics,
                     parProcess,
                     msgInterval)
      })

  def sseBusinessAlertEvent =
    (1 to msgCount).par
      .map(i => {
        triggerEvent(eventWithTopicName(numerOfNode, busalertKafkaTopic, constructBusinessAlert(_).getBytes()),
                     busAlertMetrics,
                     parProcess,
                     msgInterval)
      })

  def sseConStatusEvent =
    (1 to msgCount).par
      .map(i => {
        triggerEvent(eventWithTopicName(numerOfNode, conStatusKafkaTopic, constructConStatus(_).getBytes()),
                     connectionStatusMetrics,
                     parProcess,
                     msgInterval)
      })

  def sseGpsSampleEvent =
    (1 to msgCount).par
      .map(i => {
        triggerEvent(eventWithTopicName(numerOfNode, gpsKafkaTopic, constructGpsSample(_).getBytes()),
                     gpsSampleMetrics,
                     parProcess,
                     msgInterval)
      })

  def sseSubscribe(`type`: String) =
    triggerEvent(
      eventWithTopicName(subscribeCount, sseSubscribeTopic, Int => constructSubscribe(`type`).getBytes()),
      sseSubscribeMetrics,
      parProcess,
      subscribeInterval
    )

  def sseBusinessSubscribe(`type`: String) =
    triggerEvent(
      eventWithTopicName(subscribeCount,
                         sseBusinessSubscribeTopic,
                         Int => constructbusinessAlertSubscribe(`type`).getBytes()),
      sseBusinessSubscribeMetrics,
      parProcess,
      subscribeInterval
    )

  def sseHeartBeat(`type`: String) =
    triggerEvent(
      eventWithTopicName(subscribeCount, sseSubscribeTopic, Int => constructSubscribe(`type`).getBytes()),
      sseHeartBeatMetrics,
      parProcess,
      heartBeatInterval
    )

  def sseBusinessHeartBeat(`type`: String) =
    triggerEvent(
      eventWithTopicName(subscribeCount,
                         sseBusinessSubscribeTopic,
                         Int => constructbusinessAlertSubscribe(`type`).getBytes()),
      sseBusinessHeartBeatMetrics,
      parProcess,
      heartBeatInterval
    )

  def sseDisconnect(`type`: String) =
    triggerEvent(
      eventWithTopicName(subscribeCount, sseSubscribeTopic, Int => constructSubscribe(`type`).getBytes()),
      sseDisconnectMetrics,
      parProcess,
      disconnectInterval
    )

  def sseBusinessDisconnect(`type`: String) =
    triggerEvent(
      eventWithTopicName(subscribeCount,
                         sseBusinessSubscribeTopic,
                         Int => constructbusinessAlertSubscribe(`type`).getBytes()),
      sseBusinessDisconnectMetrics,
      parProcess,
      disconnectInterval
    )
}
