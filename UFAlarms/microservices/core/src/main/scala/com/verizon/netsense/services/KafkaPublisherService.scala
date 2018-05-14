package com.verizon.netsense.services

import akka.actor.ActorSystem
import akka.kafka.scaladsl.Producer
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, OverflowStrategy, Supervision}
import akka.stream.scaladsl.{Keep, Sink, Source}
import com.verizon.netsense.connector.KafkaConnection
import com.verizon.netsense.entity.KafkaMessage
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.producer.ProducerRecord
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods.{compact, parse, render}

import scala.util.control.NonFatal

/**
 * Created by davor on 5/25/17.
 */
abstract class KafkaPublisherService(val connection: KafkaConnection)(implicit val system: ActorSystem)
    extends Instrumented
    with Logging {
  private[this] val isPublisherSourceTimer: Timer = metrics.timer("is-publisher-source-timer")

  lazy val mainSupervisor: Supervision.Decider = {
    case NonFatal(ex) => log.error("Exception found in the Stream " + ex.getMessage); Supervision.Restart
    //case ex: Exception => log.error("Unhandled Exception seen " + ex.getMessage); Supervision.stop;
    case tr: Throwable =>
      tr.getStackTrace.foreach(x => log.error(x.toString))
      log.error("Unhandled Exception seen " + tr.getMessage); Supervision.stop;
  }

  implicit val materializer = ActorMaterializer(
    ActorMaterializerSettings(system)
      .withSupervisionStrategy(mainSupervisor)
  )(system)

  implicit val formats = DefaultFormats

  val (actorRef, publisher) =
    Source
      .actorRef[KafkaMessage](1000, OverflowStrategy.dropHead)
      .toMat(Sink.asPublisher(fanout = true))(Keep.both)
      .run()

  Source
    .fromPublisher(publisher)
    .map { elem =>
      log.debug(s"Send message to: ${elem.sendTo}")

      isPublisherSourceTimer.time()

      new ProducerRecord[Array[Byte], Array[Byte]](elem.sendTo, elem.msg)
    }
    .toMat(Producer.plainSink(connection.producerSettings))(Keep.right)
    .run()

  def sendMessage(msg: KafkaMessage): Unit = actorRef ! msg
}
