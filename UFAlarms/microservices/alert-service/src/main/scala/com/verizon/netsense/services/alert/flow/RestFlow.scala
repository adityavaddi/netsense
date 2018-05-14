package com.verizon.netsense.services.alert.flow

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.Flow
import akka.stream.{ActorAttributes, ActorMaterializer, ActorMaterializerSettings}
import com.fasterxml.jackson.core.JsonParseException
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.services.alert.customexceptions.InvalidRequestPayloadException
import com.verizon.netsense.services.alert.helper.KafkaConfig.isRequestTopic
import com.verizon.netsense.services.alert.helper.{KafkaSettings, RequestValidator, SupervisionDecider}
import com.verizon.netsense.services.alert.model.AppRequest
import com.verizon.netsense.services.alert.services.{
  AlertRequestHandler,
  JsonUtils,
  NotificationRequestHandler,
  UFAlarmRequestHandler
}
import com.verizon.netsense.services.alert.util.ObjectMapperUtil._
import com.verizon.netsense.utils.Logging
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.producer.ProducerRecord

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.Try

trait RestFlow extends Logging with JsonUtils with KafkaSettings with SupervisionDecider with Instrumented {
  private[this] val unPackingFlowTimer: Timer  = metrics.timer("JsonUnpacking-timer")
  private[this] val kafkaSinkTimer: Timer      = metrics.timer("kafka-rest-sink-producer")
  private[this] val processRequestTimer: Timer = metrics.timer("processRequest-timer")

  implicit val restServiceActorSystem       = ActorSystem("Rest-Service-system")
  implicit val ec: ExecutionContextExecutor = restServiceActorSystem.dispatcher
  implicit val mat = ActorMaterializer(
    ActorMaterializerSettings(restServiceActorSystem)
      .withInputBuffer(INITIAL_BUFFER_SIZE, MAX_BUFFER_SIZE)
      .withSupervisionStrategy(systemDecider)
  )
  lazy val alertRequestHandler        = AlertRequestHandler(ec)
  lazy val notificationRequestHandler = NotificationRequestHandler(ec)
  lazy val ufAlarmRequestHandler      = UFAlarmRequestHandler(ec)

  lazy val apiKafkaSource = kafkaRestartableConsumerSource(Set(isRequestTopic))

  val jsonStringToAppRequestFlow: Flow[Array[Byte], AppRequest, NotUsed] = Flow[Array[Byte]]
    .mapAsync(PARALLELISM) { jsonString =>
      unPackingFlowTimer.time {
        fromJsonAsync[AppRequest](jsonString).recover {
          case ex: JsonParseException =>
            log.error("Unable to parse the request: " + jsonString + " " + ex.getMessage); throw ex
        }
      }
    }
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val validateRequestFlow: Flow[AppRequest, AppRequest, NotUsed] = Flow[AppRequest]
    .filter(RequestValidator.validationPredicate)
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  def processRequest(appRequest: AppRequest): Future[CrudResponse] =
    processRequestTimer
      .time {
        Try {
          appRequest.request.model match {
            case "AlertModel"        => alertRequestHandler.getAppResponse(appRequest)
            case "NotificationModel" => notificationRequestHandler.getAppResponse(appRequest)
            case "UFAlarmModel"      => ufAlarmRequestHandler.getAppResponse(appRequest)
            case _ =>
              throw new InvalidRequestPayloadException("Invalid AppRequest model" + appRequest.request.model)
          }
        }.toEither match {
          case Left(l) =>
            log.error("Error: " + l.getMessage, l)
            errorResponse("Error: " + l.getMessage, appRequest).future()
          case Right(r) =>
            r.recover {
              case ex: Exception =>
                log.error("Error: " + ex.getMessage, ex)
                errorResponse("Error: " + ex.getMessage, appRequest)
            }
        }
      }
      .map { (appRequest.responsetopic, _) }

  val processAppRequestFlow: Flow[AppRequest, CrudResponse, NotUsed] =
    Flow[AppRequest]
      .mapAsync(PARALLELISM) {
        processRequest
      }
      .withAttributes(ActorAttributes.supervisionStrategy(dbDecider))

  val jsonToRecord: Flow[CrudResponse, ProducerRecord[Array[Byte], Array[Byte]], NotUsed] = {
    Flow[CrudResponse]
      .mapAsync(PARALLELISM) { cResp =>
        log.debug("API Response: " + cResp._2)
        kafkaSinkTimer.time {
          Future(new ProducerRecord[Array[Byte], Array[Byte]](cResp._1, toJsonBytes(cResp._2)))
        }
      }
  }.withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val kafkaSink = configKafkaProducer(_producerSettings = setupProducerSettings)

}
