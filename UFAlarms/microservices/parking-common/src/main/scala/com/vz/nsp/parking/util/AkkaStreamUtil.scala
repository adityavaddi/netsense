package com.vz.nsp.parking.util

import java.net.ConnectException

import akka.kafka.scaladsl.Consumer
import akka.kafka.scaladsl.Consumer.Control
import akka.kafka.{ConsumerSettings, Subscriptions}
import akka.stream.Supervision
import akka.stream.scaladsl.Source
import com.datastax.driver.core.exceptions.{NoHostAvailableException, ReadTimeoutException, WriteTimeoutException}
import com.fasterxml.jackson.databind.JsonMappingException
import com.verizon.netsense.utils.Logging
import org.apache.kafka.clients.consumer.ConsumerRecord

import scala.util.control.NonFatal

/**
  * Created by donthgo on 11/2/17.
  */
object AkkaStreamUtil extends Logging {

  def configKafkaSource[K, V](_consumerSettings: ConsumerSettings[K, V],
                              _topics: Set[String]): Source[ConsumerRecord[K, V], Control] =
    Consumer.plainSource(_consumerSettings, Subscriptions.topics(_topics))


  lazy val akkaGraphDecider: Supervision.Decider = {
    case ex: ConnectException         => log.error("Unable to connect to Graphite " + ex.getMessage); Supervision.Resume
    case ex: NoHostAvailableException => log.error("Unable to connect to DB " + ex.getMessage); Supervision.Resume
    case NonFatal(ex)                 => log.error("### akkaGraphDecider: Exception found in the Stream " + ex.getMessage); ex.printStackTrace(); Supervision.Resume
    case ex: Exception                => log.error("### akkaGraphDecider: Unhandled Exception seen " + ex.getMessage); Supervision.stop;
  }
  lazy val msgUnmarshallFlowDecider: Supervision.Decider = {
    case ex: JsonMappingException => log.error("msgUnmarshallFlowDecider: Unable to unpack the element", ex); Supervision.Resume
    case ex: NullPointerException =>
      log.error("### flowDecider: Null element in stream " + ex.printStackTrace()); Supervision.Resume
    case ex: Exception => log.error("### msgUnmarshallFlowDecider: Unhandled exception in stream" + ex.printStackTrace()); Supervision.Resume
  }

  lazy val queryFlowDecider: Supervision.Decider = {
    case ex: WriteTimeoutException => log.error("### Unable to write the event to cassandra", ex.printStackTrace()); Supervision.Resume
    case ex: NoHostAvailableException =>
      log.error("### Unable to establish cassandra connection ", ex.printStackTrace()); Supervision.Restart
    case ex: ReadTimeoutException => log.error("### Unable to read the events from cassandra", ex.printStackTrace()); Supervision.Resume
    case ex: NullPointerException =>
      log.error("### queryFlowDecider: Null element in stream " + ex.printStackTrace()); Supervision.Resume
    case ex: Exception => log.error("### queryFlowDecider: Unhandled Exception seen "+ ex +" " + ex.printStackTrace()); Supervision.Resume
  }


}