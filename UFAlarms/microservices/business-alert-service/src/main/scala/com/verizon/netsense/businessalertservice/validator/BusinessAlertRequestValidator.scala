package com.verizon.netsense.businessalertservice.validator

import akka.actor.ActorSystem
import akka.kafka.scaladsl.Producer
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, OverflowStrategy}
import akka.stream.scaladsl.{Keep, Source}
import com.verizon.netsense.utils.Logging
import com.verizon.netsense.businessalertservice.flow.KafkaConnector
import com.verizon.netsense.businessalertservice.model._
import com.verizon.netsense.businessalertservice.util.AkkaStreamUtil.akkaGraphDecider
import com.verizon.netsense.businessalertservice.util.ObjectMapperUtil._
import org.apache.kafka.clients.producer.ProducerRecord
import com.verizon.netsense.businessalertservice.model.StatusCodes.BADREQUEST
import com.verizon.netsense.businessalertservice.model.BusinessAlertConstants._
import com.verizon.netsense.businessalertservice.model.ResponseMessage._

class BusinessAlertRequestValidator(kafkaConnector: KafkaConnector) extends Logging {
  implicit val queryServiceActorSystemd = ActorSystem("BusinessAlertSystemValidator")
  implicit val mat = ActorMaterializer(
    ActorMaterializerSettings(queryServiceActorSystemd)
      .withInputBuffer(Math.pow(2, 10).toInt, Math.pow(2, 20).toInt)
      .withSupervisionStrategy(akkaGraphDecider)
  )

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

  def failureMessageKafkaSink(errorMessage: String = "Invalid input", status: Int = 400)(rQ: AppRequest) = {
    log.error("Business Alert request failed in validation check " + toJson(rQ) + " " + errorMessage)
    kafkaIntermediateSinkActorForFailure._1 ! buildProducerRecord(errorMessage, status)(rQ)
  }

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

  def businessAlertPropsValidation(apiType: String,
                                   businessAlertProps: Option[BusinessAlertProps])(rQ: AppRequest): Boolean =
    (apiType, businessAlertProps) match {
      case ((getBusinessAlert.value | dismissBusinessAlert.value), businessAlertProps: Option[BusinessAlertProps]) =>
        businessAlertProps match {
          case Some(businessAlert) => {
            businessAlert.businessAlertId match {
              case Some(businessAlertId) => true
              case None =>
                failureMessageKafkaSink(BusinessAlertIdMissingInRequest.value)(rQ)
                false
            }
          }
          case None =>
            failureMessageKafkaSink(BusinessAlertIdMissingInRequest.value)(rQ)
            false
        }
      case (filterBusinessAlert.value, businessAlertProps: Option[BusinessAlertProps]) =>
        businessAlertProps match {
          case Some(businessAlert) => {
            businessAlert.search match {
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
      case (getAllBusinessAlerts.value, None) => true

      case (getBusinessAlertSys.value, businessAlertProps) =>
        businessAlertProps match {
          case Some(businessAlert) => {
            businessAlert.businessAlertId match {
              case Some(businessAlertId) => {
                //Validate userid
                if (rQ.request.user.getOrElse("") == rootUser.value) true
                else {
                  failureMessageKafkaSink(UnAuthorizedUser.value, 403)(rQ)
                  false
                }
              }
              case None =>
                failureMessageKafkaSink(BusinessAlertIdMissingInRequest.value)(rQ)
                false
            }
          }
          case None =>
            failureMessageKafkaSink(BusinessAlertIdMissingInRequest.value)(rQ)
            false
        }

      case _ => failureMessageKafkaSink(BusinessAlertOrSearchMissing.value)(rQ); false
    }

  val validationPredicate: (AppRequest) => Boolean = {
    case e @ UnApplyAppRequestValidator(responsetopic, eventType, orgProps, siteProps, messageid)
        if messageid == null ||
        responsetopic == null || eventType == null =>
      throw new Exception("Required fields messageid and/or responsetopic and/or type are missing ");
      false

    case e @ UnApplyAppRequestValidator(responsetopic, eventType, orgProps, siteProps, messageid)
        if orgProps == null || siteProps == null || orgProps.orgid == null || siteProps.siteid == null =>
      failureMessageKafkaSink("Required fields  orgid and/or siteid fields are missing")(e)
      false

    case e @ UnApplyAppRequestValidator(responsetopic, eventType, orgProps, siteProps, messageid)
        if responsetopic != null && eventType != null && orgProps != null && siteProps != null &&
        messageid != null =>
      businessAlertPropsValidation(eventType, e.request.businessalertprops)(e)

    case a: AppRequest => {
      log.error("Corrupted request found in Business Alert request: ")
      failureMessageKafkaSink(BusinessAlertOrSearchMissing.value)(a)
      false
    }
  }
}

object BusinessAlertRequestValidator {
  def apply(kafkaConnector: KafkaConnector): BusinessAlertRequestValidator =
    new BusinessAlertRequestValidator(kafkaConnector)
}
