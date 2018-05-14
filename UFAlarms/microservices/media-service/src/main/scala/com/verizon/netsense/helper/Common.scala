package com.verizon.netsense.helper

import java.net.ConnectException
import java.util.{Date, UUID}

import akka.Done
import akka.kafka.scaladsl.Consumer.Control
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.kafka.{ConsumerSettings, ProducerSettings, Subscriptions}
import akka.stream.Supervision
import akka.stream.scaladsl.{Sink, Source}
import akka.util.ByteString
import com.datastax.driver.core.exceptions.{NoHostAvailableException, ReadTimeoutException, WriteTimeoutException}
import com.fasterxml.jackson.databind.JsonMappingException
import com.verizon.netsense.model.{SicISQueryEnvelope, SicReqEvent, SicRequestPayload, SicResponseEvent}
import com.verizon.netsense.util.{Logging}
import com.verizon.netsense.util.ObjectMapperUtil
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.ProducerRecord

import scala.concurrent.Future
import scala.util.control.NonFatal

// Todo: Refactor and move to core/common.
/**
  * Copied over from Sensor Serive, as Created by maidapr on 7/10/17.
  *
  */

trait Common extends Logging {

  lazy val parallelism = 1000

  lazy val systemDecider: Supervision.Decider = {
    case ex: ConnectException => log.error("Unable to connect to Graphite " + ex.getMessage); Supervision.Resume
    case ex: NoHostAvailableException => log.error("Unable to connect to DB " + ex.getMessage); Supervision.Resume
    case NonFatal(ex) => log.error("Exception found in the Stream " + ex.getMessage); Supervision.Resume
    case ex: Exception => log.error("Unhandled Exception seen " + ex.getMessage); Supervision.stop;
  }

  lazy val flowDecider: Supervision.Decider = {
    case ex: JsonMappingException => log.error("Unable to unpack the element", ex); Supervision.Resume
    case ex: NullPointerException => log.error("Null element in stream " + ex.getMessage); Supervision.Restart
    case ex: Exception => log.error("Unhandled exception in stream" + ex.getMessage); Supervision.Resume
  }

  lazy val convertToJson: (Any) => String = e => ObjectMapperUtil.toJson(e)

  def configKafkaSource[K, V](_consumerSettings: ConsumerSettings[K, V],
                              _topics: Set[String]): Source[ConsumerRecord[K, V], Control] =
    Consumer.plainSource(_consumerSettings, Subscriptions.topics(_topics))

  def configKafkaProducer[K, V](_producerSettings: ProducerSettings[K, V]): Sink[ProducerRecord[K, V], Future[Done]] =
    Producer.plainSink(_producerSettings)

  lazy val convertCASELtoRestPack: (SicISQueryEnvelope) => (SicISQueryEnvelope, SicReqEvent) =
    query => {
      log.debug(s"CASEL2RP, IS/SIC Query: ${query.toString}")
      val c: Short = (51200).toShort
      val sicReqPayload = SicRequestPayload(query.request.sicProps.imgFormat, c)
      log.debug(s"sicReqPayload: $sicReqPayload")
      val payloadJson= SicRequestPayload(query.request.sicProps.imgFormat, c).toJSON
      log.debug(s"payload as Json: ${payloadJson}")
      val payloadMsgPack = SicRequestPayload(query.request.sicProps.imgFormat, c).toMsgPack
      log.debug(s"payload to MsgPack: ${ByteString(payloadMsgPack).utf8String}")
      (query,
        SicReqEvent(
          uuid = query.messageid,
          sid = query.request.nodeprops.nodeid,
          a = "GET",
          p = s"v1/${query.request.nodeprops.nodeid}/in/GET/mediaserver/imgget/${query.request.sicProps.channel}${query.request.sicProps.res.toUpperCase}",
          l = payloadMsgPack
        ))
    }
}
