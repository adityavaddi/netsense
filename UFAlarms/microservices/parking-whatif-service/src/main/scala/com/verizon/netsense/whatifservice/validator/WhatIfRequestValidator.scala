package com.verizon.netsense.whatifservice.validator

/**
 * Created by maleva on 3/30/18.
 */
import akka.actor.ActorSystem
import akka.kafka.scaladsl.Producer
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, OverflowStrategy}
import akka.stream.scaladsl.{Keep, Source}
import com.verizon.netsense.utils.Logging
import com.verizon.netsense.whatifservice.flow.KafkaConnector
import com.verizon.netsense.whatifservice.model.ResponseMessage.{apply => _, _}
import com.verizon.netsense.whatifservice.model.WhatIfJobRequest
import com.verizon.netsense.whatifservice.model.casel._
import com.verizon.netsense.whatifservice.util.AkkaStreamUtil.akkaGraphDecider
import com.verizon.netsense.whatifservice.model.WhatIfConstants.{apply => _, _}
import org.apache.kafka.clients.producer.ProducerRecord
import com.verizon.netsense.whatifservice.util.ObjectMapperUtil._
import com.verizon.netsense.whatifservice.model.StatusCodes._
import org.joda.time.format.DateTimeFormat

/**
 * Created by maleva on 3/30/18.
 */
class WhatIfRequestValidator(kafkaConnector: KafkaConnector) extends Logging {
  implicit val queryServiceActorSystemd = ActorSystem("WhatIfSystemValidator")
  implicit val mat = ActorMaterializer(
    ActorMaterializerSettings(queryServiceActorSystemd)
      .withInputBuffer(Math.pow(2, 10).toInt, Math.pow(2, 20).toInt)
      .withSupervisionStrategy(akkaGraphDecider)
  )

  def validateWhatIf(whatIf: WhatIfJobRequest): String =
    ""

  object UnApplyAppRequestValidator {
    def unapply(x: AppRequest): Some[(String, String, OrgProps, SiteProps, String)] = {
      val c = Some(x.responsetopic, x.request.`type`, x.request.orgprops, x.request.siteprops, x.messageid)
      c
    }
  }

  implicit val kafkaIntermediateSinkActorForFailure =
    Source
      .actorRef[ProducerRecord[Array[Byte], Array[Byte]]](Int.MaxValue, OverflowStrategy.dropHead)
      .map { msg =>
        log.debug("msg received at actor ref " + new String(msg.value(), "UTF-8") + " " + msg.topic()); msg
      }
      .toMat(Producer.plainSink(kafkaConnector.setupProducerSettings))(Keep.both)
      .run()

  def buildProducerRecord(errorMsg: String, status: Int)(sqe: AppRequest): ProducerRecord[Array[Byte], Array[Byte]] =
    new ProducerRecord[Array[Byte], Array[Byte]](
      sqe.responsetopic,
      sqe.messageid.getBytes,
      toJson(
        AppFailureResponse(
          sqe.messageid,
          response =
            FailureResponseBody(sqe.request.requestid, sqe.request.timestamp, success = false, errorMsg, BADREQUEST.id)
        )
      ).getBytes
    )

  def failureMessageKafkaSink(errorMessage: String = "Invalid input", status: Int = 400)(rQ: AppRequest) = {
    log.error("What If request failed in validation check " + toJson(rQ) + " " + errorMessage)
    kafkaIntermediateSinkActorForFailure._1 ! buildProducerRecord(errorMessage, status)(rQ)
  }
  lazy val dateFormat = "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"
  def validateDateFormat(dateAndTime: String): Boolean =
    try {
      val fmt = DateTimeFormat forPattern dateFormat
      Some(fmt parseDateTime dateAndTime)
      true
    } catch {
      case e: IllegalArgumentException => log.error("Error while validating dateformat " + e.getMessage); false
    }

  def fromTimeltToTime(fromTime: String, toTime: String): Boolean = {
    val fmt    = DateTimeFormat forPattern dateFormat
    val result = (fmt parseDateTime fromTime) isBefore (fmt parseDateTime toTime)
    result
  }

  def parkingGroupValidation(parkingGroups:List[String]): Boolean =
  {
    parkingGroups.map(parkingGroup=> if(parkingGroup.isEmpty)false else true).reduce(_&&_)
  }

  def whatIfPropsValidation(apiType: String, whatIfProps: Option[WhatIfProps])(rQ: AppRequest): Boolean =
    (apiType, whatIfProps) match {
      case ((createWhatIfJob.value | updateWhatIfJob.value ), whatIfProps: Option[WhatIfProps]) =>
        whatIfProps match {
          case Some(whatIf) => {
            whatIf.whatIfJobRequest match {
              case Some(payLoad) => {
                if (payLoad.name.isEmpty || payLoad.fromTime.isEmpty | payLoad.toTime.isEmpty | payLoad.parkingGroups.isEmpty |  payLoad.parkingPolicies.isEmpty | payLoad.mode.isEmpty) {
                  failureMessageKafkaSink(RequiredFieldsMissing.value)(rQ)
                  false
                }
                else if(!(parkingGroupValidation(payLoad.parkingGroups))){
                  failureMessageKafkaSink(ParkingGroup.value)(rQ)
                  false
                }
                  else
                 if (!validateDateFormat(payLoad.fromTime) | !validateDateFormat(payLoad.toTime) | !fromTimeltToTime(
                             payLoad.fromTime,
                             payLoad.toTime
                           )) {
                  failureMessageKafkaSink(DateFormatError.value)(rQ)
                  false

                }
                else if(!(payLoad.mode.equals(AsIs.value)|(payLoad.mode.equals(NoViolations.value)))) {
                  failureMessageKafkaSink(WhatIf_ValidationFailed.value)(rQ)
                  false
                }

                else {
                  true
                }
              }
              case None =>
                failureMessageKafkaSink(SearchMissingInRequest.value)(rQ)
                false
            }
          }
          case None =>
            failureMessageKafkaSink(SearchMissingInRequest.value)(rQ)
            false
        }
      case ((getWhatIfJob.value | updateWhatIfJob.value | abortWhatIfJob.value| deleteWhatIfJob.value), whatIfProps: Option[WhatIfProps]) =>
        whatIfProps match {
          case Some(whatIf) => {
            whatIf.jobid match {
              case Some(jobid) => true
              case None =>
                failureMessageKafkaSink(WhatIfJobIdMissingInRequest.value)(rQ)
                false
            }
          }
          case None =>
            failureMessageKafkaSink(WhatIfJobIdMissingInRequest.value)(rQ)
            false
        }
      case (searchWhatIfJob.value, whatIfProps: Option[WhatIfProps]) =>
        whatIfProps match {
          case Some(whatIf) => {
            whatIf.search match {
              case Some(searchVal) => true
              case None =>
                failureMessageKafkaSink(SearchMissingInRequest.value)(rQ)
                false
            }
          }
          case None =>
            failureMessageKafkaSink(SearchMissingInRequest.value)(rQ)
            false
        }
      case (getAllWhatIfJobs.value, None) => true

      case _ => failureMessageKafkaSink(WhatIfJobOrSearchMissing.value)(rQ); false
    }

  val validationPredicate: (AppRequest) => Boolean = {
    case e @ UnApplyAppRequestValidator(responsetopic, eventType, orgProps, siteProps, messageid)
        if e.responsetopic != null && e.request.`type` != null && e.request.orgprops != null && e.request.siteprops != null &&
        e.messageid != null =>
      whatIfPropsValidation(eventType, e.request.whatifprops)(e)
    case e @ UnApplyAppRequestValidator(responsetopic, eventType, orgProps, siteProps, messageid)
        if (messageid != null && responsetopic != null && eventType != null) &&
        (orgProps == null || siteProps == null || orgProps.orgid == null || siteProps.siteid == null) =>
      failureMessageKafkaSink("Required fields  orgid and/or siteid fields are missing")(e)
      false

    case e @ UnApplyAppRequestValidator(responsetopic, eventType, orgProps, siteProps, messageid)
        if messageid == null ||
        responsetopic == null || eventType == null =>
      throw new Exception("Required fields messageid and/or responsetopic and/or type are missing ");
      false
    case a: AppRequest => {
      log.error("Corrupted request found in What if request: ")
      failureMessageKafkaSink(WhatIfJobOrSearchMissing.value)(a)
      false
    }
  }
}

object WhatIfRequestValidator {
  def apply(kafkaConnector: KafkaConnector): WhatIfRequestValidator =
    new WhatIfRequestValidator(kafkaConnector)
}
