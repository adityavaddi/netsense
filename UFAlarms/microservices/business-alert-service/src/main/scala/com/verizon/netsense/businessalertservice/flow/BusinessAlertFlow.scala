package com.verizon.netsense.businessalertservice.flow

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.ActorAttributes
import akka.stream.scaladsl.Flow
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import com.verizon.netsense.businessalertservice.flow.RestApiFlow
import com.verizon.netsense.businessalertservice.model.AppRequest
import com.verizon.netsense.businessalertservice.util.AkkaStreamUtil.{msgUnmarshallFlowDecider, queryFlowDecider}
import com.verizon.netsense.businessalertservice.util.ObjectMapperUtil
import com.verizon.netsense.businessalertservice.validator.BusinessAlertRequestValidator
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.consumer.ConsumerRecord

import scala.concurrent.ExecutionContext

class BusinessAlertFlow(restApiFlow: RestApiFlow, businessAlertRequestValidator: BusinessAlertRequestValidator) extends Instrumented with Logging {
  implicit val queryServiceActorSystem = ActorSystem()
  lazy val parallelism = 1000
  implicit val ec = ExecutionContext.Implicits.global
  private[this] val businessAlertUnMarshallingFlowTimer: Timer =
    metrics.timer("business-alert-json-unmarshall-flow-timer")

  lazy val businessAlertProcessRequestFlow: Flow[AppRequest, (AppRequest, String), NotUsed] = Flow[AppRequest]
    .map(appRequest => (appRequest, restApiFlow.processRequest(appRequest)))
    .mapAsync(parallelism) {
      case (appRequest, futureOfAppResponse) =>
        futureOfAppResponse.map(appRes => (appRequest, ObjectMapperUtil.toJson(appRes)))
    }
    .withAttributes(ActorAttributes.supervisionStrategy(queryFlowDecider))

  lazy val messageValidationFlow: Flow[AppRequest, AppRequest, NotUsed] = Flow[AppRequest]
    .filter(businessAlertRequestValidator.validationPredicate)
    .withAttributes(ActorAttributes.supervisionStrategy(queryFlowDecider))

  lazy val msgUnmarshallingFlow: Flow[ConsumerRecord[String, String], AppRequest, NotUsed] =
    Flow[ConsumerRecord[String, String]]
      .mapAsync(parallelism) { consumerRecord =>
        businessAlertUnMarshallingFlowTimer.time {
          ObjectMapperUtil.jsonToAppRequest(consumerRecord.value)
        }
      }
      .withAttributes(ActorAttributes.supervisionStrategy(msgUnmarshallFlowDecider))

}
object BusinessAlertFlow {
  def apply(streamHelper: RestApiFlow, businessAlertRequestValidator: BusinessAlertRequestValidator): BusinessAlertFlow = new BusinessAlertFlow(streamHelper,businessAlertRequestValidator)
}
