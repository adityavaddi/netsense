package com.verizon.netsense.services.gps.simulator

import java.util.Calendar

import akka.Done
import akka.actor.ActorSystem
import akka.kafka.ProducerSettings
import akka.kafka.scaladsl.Producer
import akka.stream.scaladsl.{Sink, Source}
import akka.stream.{ActorMaterializer, ThrottleMode}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.services.gps.model.GpsEvent
import com.verizon.netsense.services.gps.util.ObjectMapperUtil
import com.verizon.netsense.utils.{ConfigLoader, Logging}
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.StringSerializer

import scala.concurrent.Future
import scala.concurrent.duration._

object GpsSampleMessageSimulator extends App with Logging with Instrumented{

  lazy val throttleLevel = 2
  val eventCount         = 10 //GPS message generation count
  val solicitationType   = "UNSOL"
  val path               = "v1/global/UNSOL/gps"

  implicit val system = ActorSystem("Gps-Simulator-Actor-System")
  implicit val mat    = ActorMaterializer()
  implicit val ec     = system.dispatcher

  private[this] val producerTimer: Timer = metrics.timer("kafka-producer-timer")

  lazy val kafkaConfig = ConfigLoader.config.getConfig("kafka")
  val requestTopic     = kafkaConfig.getString("gps-request-topic")

  val producerSettings = ProducerSettings(system, new StringSerializer, new StringSerializer)
    .withBootstrapServers(kafkaConfig.getString("host") + ":" + kafkaConfig.getInt("port"))

  def sink: Sink[ProducerRecord[String, String], Future[Done]] =
    Producer.plainSink(producerSettings)


  def getGps(i: Long): GpsEvent = {
    val gpsEvent = GpsEvent("NSN66666",Some("GpsSample"), 102.345,234.567,Calendar.getInstance().getTimeInMillis)
    log.debug("Gps " + i + " sent with payload: " + ObjectMapperUtil.toJson(gpsEvent))
    gpsEvent
  }



  lazy val eventSimulator = Source(1 to eventCount)
    .map(x => getGps(x))
    .map { e =>
      producerTimer.time(e)
      new ProducerRecord[String, String](requestTopic, ObjectMapperUtil.toJson(e))
    }
    .throttle(throttleLevel, 1000.milli, throttleLevel, ThrottleMode.Shaping)
    .runWith(sink)

  Thread.sleep(2000)
  eventSimulator
  log.info(eventCount + " Gps Sample message is pushed.")

}
