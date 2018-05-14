package com.verizon.netsense.util

import java.net.ConnectException

import akka.stream.Supervision
import com.datastax.driver.core.exceptions.NoHostAvailableException
import com.fasterxml.jackson.core.JsonParseException
import com.verizon.netsense.utils
import com.verizon.netsense.utils.Logging
import org.apache.kafka.common.KafkaException

import scala.util.control.NonFatal

/**
  * Created by ssaurav on 1/2/18.
  */
trait Common extends utils.TimeConverter with Logging{

  lazy val parallelism = 1000

  lazy val systemDecider: Supervision.Decider = {
    case ex: ConnectException         => log.error("Unable to connect to Graphite " + ex.getMessage)
      Supervision.Resume
    case ex: NoHostAvailableException => log.error("Unable to connect to DB " + ex.getMessage)
      Supervision.Resume
    case NonFatal(ex)                 => log.error("Exception found in the Stream " + ex.getMessage)
      ex.printStackTrace()
      Supervision.Resume
    case ex: Exception                => log.error("Unhandled Exception seen " + ex.getMessage)
      ex.printStackTrace()
      Supervision.Resume
  }

  lazy val flowDecider: Supervision.Decider = {
    case ex: JsonParseException   => log.error("Unablee to Parse message " + ex.getMessage)
      Supervision.Resume
    case ex: NullPointerException => log.error("Null element in stream " + ex.getMessage)
      ex.printStackTrace()
      Supervision.Resume
    case ex: IllegalArgumentException => log.error("The input sent to stream has issues: " + ex.getMessage)
      Supervision.Resume
    case ex: RuntimeException     => log.error("Runtime issue: " + ex.getMessage)
      Supervision.Resume
    case ex: Exception            => log.error("Unhandled exception in stream " + ex)
      ex.printStackTrace()
      Supervision.Resume
  }

  lazy val sourceDecider: Supervision.Decider = {
    case ex: NullPointerException => log.error("Null element in stream " + ex.getMessage)
      ex.printStackTrace()
      Supervision.Resume
    case ex: KafkaException =>
      log.error("KafkaException in Consumer: " + ex.getClass.getName + ": " + ex.getMessage)
      Supervision.Restart
    case ex =>
      log.error("Unhandled exception in stream source: " + ex.getClass.getName + ": " + ex.getMessage)
      ex.printStackTrace()
      Supervision.Resume
  }

  lazy val sinkDecider: Supervision.Decider = {
    case ex: NullPointerException => log.error("Null element in stream " + ex.getMessage)
      ex.printStackTrace()
      Supervision.Resume
    case ex: KafkaException =>
      log.error("KafkaException in Producer: " + ex.getClass.getName + ": " + ex.getMessage)
      Supervision.Restart
    case ex =>
      log.error("Unhandled exception in stream sink: " + ex.getClass.getName + ": " + ex.getMessage)
      ex.printStackTrace()
      Supervision.Resume
  }
}


