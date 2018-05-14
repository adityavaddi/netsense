package com.verizon.netsense.services.bridge.flow

import java.net.InetAddress

import akka.event.{Logging => ALogging}
import akka.stream.scaladsl.Flow
import akka.stream.{ActorAttributes, Supervision}
import com.fasterxml.jackson.databind.{JsonMappingException, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.services.bridge.utils.ConfigLoader.fromRabbitAsyncParallelism
import com.verizon.netsense.utils.Logging
import nl.grons.metrics.scala.{MetricName, Timer}
import org.velvia.MsgPackUtils

import scala.concurrent.Future

/**
 * Created by wswaney on 6/7/17.
 */
object Common extends /*App with*/ Instrumented with Logging {

  val localhost     = InetAddress.getLocalHost
  val metricsPrefix = localhost + ".bridge.common-flow."
  implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  override lazy val metricBaseName = MetricName(metricsPrefix)

  val mapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)

  private[this] val xformTimer: Timer  = metrics.timer("xform-timer")


  log.debug("FromRabbit Async Parallelism:" + fromRabbitAsyncParallelism)

  lazy val flowDecider: Supervision.Decider = {
    case ex: JsonMappingException => log.error("Unable to unpack the element", ex); Supervision.Restart
    case ex: NullPointerException => log.error("Null element in stream " + ex.getMessage); Supervision.Restart
    case ex: java.util.NoSuchElementException =>
      log.error("NoSuchElementException while transforming map: " + ex.getMessage); Supervision.resume
    case ex =>
      log.error("Unhandled exception in stream flow: " + ex.getClass.getName + ": " + ex.getMessage);
      Supervision.Restart
  }

  lazy val sourceDecider: Supervision.Decider = {
    case ex: NullPointerException =>
      ex.getStackTrace.foreach(x => log.error(x.toString))
      log.error("Null element in stream " + ex.getMessage); Supervision.Restart
    case ex: Throwable =>
      ex.getStackTrace.foreach(x => log.error(x.toString))
      log.error("Unhandled exception in stream source: " + ex.getClass.getName + ": " + ex.getMessage);
      Supervision.Restart
  }

  lazy val attributes =
    ActorAttributes
      .logLevels(onElement = ALogging.DebugLevel, onFinish = ALogging.DebugLevel, onFailure = ALogging.DebugLevel)
      .and(
        ActorAttributes
          .supervisionStrategy(sourceDecider)
      )


  val messageDelivery =
    Flow[(String, String, Array[Byte])] //Flow[kafka topic, mqtt msg topic, mqtt msg payload]
      .map { x =>
      xformTimer.time{
        val kafkaTopic = x._1
        val mqttTopic = x._2
        val msg = x._3
        val m = MsgPackUtils.unpackMap(msg)

        val payload = if (m.contains("l")) {
          msgPackToJson(m)
        }
        else {
          log.debug("Non-RESTPack Message Payload:"+ new String(mapper.writeValueAsBytes(m)))
          msg
        }
        log.debug(s"Topic: ${kafkaTopic} Payload: ${payload}")
        (kafkaTopic, mqttTopic, payload)
      }}
      .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  def msgPackToJson(m: Map[Any, Any]): Array[Byte] = {
    log.debug("Scala Map: " + m)
    log.debug(s"Scala payload: ${new String(m("l").asInstanceOf[Array[Byte]])}")
    // FIXME: MsgPackUtils is not unpacking inner maps right now
    val updatedMap = m.updated("l", MsgPackUtils.unpackMap(m("l").asInstanceOf[Array[Byte]]))
    val jsonValue = mapper.writeValueAsBytes(updatedMap)
    log.debug("JSON: " + new String(jsonValue))
    jsonValue
  }

  val messageDeliveryForCoreNode =
    Flow[(String, String, Array[Byte])] //Flow[kafka topic, RMQ message routing key, RMQ message payload]
      .mapAsync(fromRabbitAsyncParallelism) { x =>
      val topic = x._1
      val routingKey = x._2
      log.debug(s"Kafka Topic:${topic}, Routing Key:${routingKey}")
      Future {
        val jsonValue = asJson(x._3)
        (topic, routingKey, jsonValue)
      }
    }.withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val asJson = (msg: Array[Byte]) => {
    val m = MsgPackUtils.unpackMap(msg)
    log.debug("Scala Map: " + m)
    val b = mapper.writeValueAsBytes(m)
    log.debug("JSON: " + new String(b))
    b
  }
}
