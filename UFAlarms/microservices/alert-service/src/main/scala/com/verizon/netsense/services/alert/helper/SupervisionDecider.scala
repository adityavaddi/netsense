package com.verizon.netsense.services.alert.helper

import java.net.ConnectException

import akka.stream.Supervision
import com.datastax.driver.core.exceptions.NoHostAvailableException
import com.fasterxml.jackson.databind.exc.MismatchedInputException
import com.verizon.netsense.services.alert.customexceptions._
import com.verizon.netsense.utils.Logging

import scala.util.control.NonFatal

/**
 * Created by maidapr on 7/10/17.
 */
trait SupervisionDecider extends Logging {

  lazy val PARALLELISM         = 10000
  lazy val INITIAL_BUFFER_SIZE = Math.pow(2, 13).toInt
  lazy val MAX_BUFFER_SIZE     = Math.pow(2, 20).toInt

  lazy val systemDecider: Supervision.Decider = {
    case ex: ConnectException         => log.error("Unable to connect to Graphite " + ex.getMessage); Supervision.Resume
    case ex: NoHostAvailableException => log.error("Unable to connect to DB " + ex.getMessage); Supervision.Resume
    case NonFatal(ex)                 => log.error("Exception found in the Stream " + ex.getMessage); Supervision.Resume
    case ex: Exception                => log.error("Unhandled Exception seen " + ex.getMessage); Supervision.Resume;
  }

  val dbDecider: Supervision.Decider = {
    case ex: CassandraDBException =>
      log.error("Database Operation Failed: " + ex.getMessage); ex.printStackTrace(); Supervision.Resume
    case ex: AlarmNotCreatedException => log.error("Invalid alarm received: " + ex.getMessage); Supervision.Resume
    case ex: AlertEnrichmentException => log.error(ex.getMessage); Supervision.Resume
    case ex: OrgDataNotFoundException =>
      log.warn("No org data found for the node. Enriching alarm to alert failed.\n" + ex.getMessage); Supervision.Resume
    case ex: InvalidRequestPayloadException   => log.error(ex.getMessage); Supervision.Resume
    case ex: DataNotFoundInDbOrCacheException => log.warn(ex.getMessage); Supervision.Resume
    case ex: NullPointerException             => log.error("Null element in stream " + ex); Supervision.Resume
    case ex: Exception                        => log.error("Unhandled exception in stream " + ex); Supervision.Resume
  }

  lazy val flowDecider: Supervision.Decider = {
    case ex: CassandraDBException =>
      log.error("Database Operation Failed: " + ex.getMessage); ex.printStackTrace(); Supervision.Resume
    case ex: JsonParsingException => log.error("Unable to parse the JSON" + ex.getMessage); Supervision.Resume
    case ex: RestPackConversionException =>
      log.error("Unable to convert alarm to RestPack" + ex.getMessage); Supervision.Resume
    case ex: AlarmNotCreatedException => log.error("Invalid alarm received: " + ex.getMessage); Supervision.Resume
    case ex: MismatchedInputException =>
      log.error("Unable to construct Alarm from JSON payload" + ex.getMessage); Supervision.Resume
    case ex: NullPointerException => log.error("Null element in stream " + ex.getMessage); Supervision.Resume
    case ex: Exception            => log.error("Unhandled exception in stream" + ex); Supervision.Resume
  }

}
