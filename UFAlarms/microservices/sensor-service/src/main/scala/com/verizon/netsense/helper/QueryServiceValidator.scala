package com.verizon.netsense.helper

import java.util.Calendar

import akka.kafka.scaladsl.Producer
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Keep, Source}
import com.verizon.netsense.config.KafkaConfig.bootStrapServers
import com.verizon.netsense.config.SensorKafkaConnector
import com.verizon.netsense.exceptions.CustomExceptions.MessageUnMarshallingException
import com.verizon.netsense.model._
import com.verizon.netsense.service.SensorHistoryQueryService._
import com.verizon.netsense.utils.Logging
import com.verizon.netsense.utils.TimeConverter
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.ByteArraySerializer

/**
  * Created by nalamte on 9/12/17.
  */

object QueryServiceValidator extends Logging with TimeConverter {

  object UnappliedEnvelopeM{
    def unapply(x: SensorQueryEnvelope): Some[(String, String, String, String, String, String, String, String, Option[String])]
    = Some(x.request.nodeprops.nodeid,
      x.request.extprops.sensorid,
      x.responsetopic,
      x.request.`type`,
      x.request.siteprops.siteid,
      x.messageid,
      x.request.extprops.date,
      x.request.extprops.date1,
      x.request.extprops.date2)
  }

  lazy val setupProducerSettings = SensorKafkaConnector
    .configKafkaProducerSettings(bootStrapServers,
      _keySerializer = new ByteArraySerializer,
      _valueSerializer = new ByteArraySerializer)

  implicit val kafkaIntermediateSinkActorForFailure =
    Source.actorRef[ProducerRecord[Array[Byte], Array[Byte]]](Int.MaxValue, OverflowStrategy.dropHead)
      .map{ msg => log.debug("msg received at actor ref " + new String(msg.value(), "UTF-8") + " " + msg.topic()); msg }
      .toMat(Producer.plainSink(setupProducerSettings))(Keep.both).run()

  def failureMessageKafkaSink(errorMessage: String = "Invalid input", status: Int = 400)(rQ: SensorQueryEnvelope) = {
    log.error("Sensor request failed in validation check " + rQ.toJSON + " " + errorMessage)
    kafkaIntermediateSinkActorForFailure._1 ! buildProducerRecord(errorMessage, status)(rQ)
  }

  def buildProducerRecord(errorMsg: String, status: Int)(sqe: SensorQueryEnvelope):
  ProducerRecord[Array[Byte], Array[Byte]] = {
    new ProducerRecord[Array[Byte], Array[Byte]](sqe.responsetopic, sqe.messageid.getBytes,
      SensorSampleQueryResponse(sqe.messageid, response = SensorSampleQueryPayload(sqe.request.requestid,
        success = false, sqe.request.timestamp, errorMsg, status, items = null)).toJSON.getBytes)
  }

  lazy val invalidDateErrorMsg = "Invalid date format found in the request"

  def dateValidatorFunction(eventType: String, date: String, date1: String, date2: Option[String])
                           (rQ: SensorQueryEnvelope): Boolean = (eventType, date, date1, date2) match {
    case ("getSensorHistory", date: String, _, toDate) =>
      val tuple = (validateDateFormat(date),
        validateDateFormat(toDate.getOrElse(Calendar.getInstance().toInstant.toString)))
      tuple match {
        case (true, true) => true
        case (false, true) => log.error(s"Date Validations failed for $eventType: ${rQ.toJSON}")
          failureMessageKafkaSink(invalidDateErrorMsg)(rQ)
          false
        case (_, _) => false
      }
    case ("getSensorHistoryFromTo", _, fromDate: String, toDate) =>
      val tuple = (validateDateFormat(fromDate), validateDateFormat(toDate.getOrElse(Calendar.getInstance().toInstant.toString)))
      tuple match {
        case (true, true) => true
        case (_, _) => log.error(s"Date Validations failed for $eventType ${rQ.toJSON}")
          failureMessageKafkaSink(invalidDateErrorMsg)(rQ)
          false
      }
    case (_, _, fromDate: String, toDate) =>
      val tuple = (validateDateFormat(fromDate), validateDateFormat(toDate.getOrElse(Calendar.getInstance().toInstant.toString)))
      tuple match {
        case (true, true) => true
        case (_, _) => log.error(s"Date Validations failed for $eventType ${rQ.toJSON}")
          failureMessageKafkaSink(invalidDateErrorMsg)(rQ)
          false
      }
    case (_, _, _, _) => failureMessageKafkaSink(invalidDateErrorMsg)(rQ); false
  }

  val validationPredicate: (SensorQueryEnvelope) => Boolean = {
    case e@UnappliedEnvelopeM(nodeid, sensorid, responsetopic, eventType, siteid, messageid, date, date1, date2)
      if nodeid != null &&
        sensorid != null &&
        responsetopic != null &&
        siteid != null &&
        messageid != null
    =>
      dateValidatorFunction(eventType, date, date1, date2)(e)
    case e@UnappliedEnvelopeM(nodeid, sensorid, responsetopic, eventType, siteid, messageid, date, date1, date2)
      if (messageid != null && responsetopic != null && eventType != null) &&
        (sensorid == null || siteid == null || nodeid == null)
    =>  val invalidDateErrorMsg = "Invalid date format found in the request"
      dateValidatorFunction(eventType, date, date1, date2)(e)
      failureMessageKafkaSink("One of the required fields in nodeid, siteid, sensorid or date fields are missing")(e)
      false

    case e@UnappliedEnvelopeM(nodeid, sensorid, responsetopic, eventType, siteid, messageid, date, date1, date2)
      if messageid == null ||
        responsetopic == null ||  eventType == null =>
      throw new MessageUnMarshallingException("Required fields messageid or sensorQueryPayload are null " + e.toJSON); false
    case a: SensorQueryEnvelope => log.error("Corrupted event found in query validation: " + a.toJSON); false
  }
}

