package com.verizon.netsense.whatifservice.flow

import akka.{Done, NotUsed}
import akka.actor.ActorSystem
import akka.stream.{ActorAttributes, Attributes}
import akka.stream.scaladsl.{Flow, Keep, Sink}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import com.verizon.netsense.whatifservice.model.{SparkResultToCassandra, WhatIfSparkRequest}
import com.verizon.netsense.whatifservice.model.casel.AppRequest
import com.verizon.netsense.whatifservice.service.SparkJobRESTService
import org.apache.kafka.clients.consumer.ConsumerRecord
import nl.grons.metrics.scala.Timer
import com.verizon.netsense.whatifservice.util.AkkaStreamUtil._
import com.verizon.netsense.whatifservice.util.ObjectMapperUtil
import com.verizon.netsense.whatifservice.validator.WhatIfRequestValidator
import com.verizon.netsense.whatifservice.config.ServiceConfig.parallelism

import scala.concurrent.{ExecutionContext, Future}

/**
 * Created by maleva on 3/18/18.
 */
class WhatIfFlow(restApiFlow: RestApiFlow,
                 whatIfRequestValidator: WhatIfRequestValidator,
                 sparkJobRESTService: SparkJobRESTService)
    extends Instrumented
    with Logging {

  private[this] val whatifUnMarshallingFlowTimer: Timer = metrics.timer("whatif-json-unmarshall-flow-timer")

  implicit val queryServiceActorSystem = ActorSystem()
  implicit val ec                      = ExecutionContext.Implicits.global

  lazy val whatIfProcessRequestFlow: Flow[AppRequest, (AppRequest, String, WhatIfSparkRequest), NotUsed] =
    Flow[AppRequest]
      .map(appRequest => (appRequest, restApiFlow.processRequest(appRequest)))
      .mapAsync(parallelism) {
        case (appRequest, futureOfAppResponse) =>
          futureOfAppResponse.map(appRes => (appRequest, ObjectMapperUtil.toJson(appRes._1), appRes._2))
      }
      .withAttributes(ActorAttributes.supervisionStrategy(queryFlowDecider))

  lazy val messageValidationFlow: Flow[AppRequest, AppRequest, NotUsed] = Flow[AppRequest]
    .filter(whatIfRequestValidator.validationPredicate)
    .withAttributes(ActorAttributes.supervisionStrategy(queryFlowDecider))

  lazy val msgUnmarshallingFlow: Flow[ConsumerRecord[String, String], AppRequest, NotUsed] =
    Flow[ConsumerRecord[String, String]]
      .mapAsync(parallelism) { consumerRecord =>
        whatifUnMarshallingFlowTimer.time {
          ObjectMapperUtil.jsonToAppRequest(consumerRecord.value)
        }
      }
      .withAttributes(ActorAttributes.supervisionStrategy(msgUnmarshallFlowDecider))

  lazy val sparkRequestFlow: Flow[(AppRequest, String, WhatIfSparkRequest), SparkResultToCassandra, NotUsed] =
    Flow[(AppRequest, String, WhatIfSparkRequest)]
      .filter(request => request._3.requestSpark)
      .mapAsync(200) { request =>
        sparkJobRESTService.callSparkJob(request)
      }
      .addAttributes(Attributes.inputBuffer(Math.pow(2, 15).toInt, Math.pow(2, 20).toInt))
      .withAttributes(ActorAttributes.supervisionStrategy(queryFlowDecider))

  lazy val sparkResultToCassandraSink: Sink[SparkResultToCassandra, Future[Done]] =
    Flow[SparkResultToCassandra]
      .map(result => sparkJobRESTService.writeSparkStatusToCassandra(result))
      .toMat(Sink.ignore)(Keep.right)
      .withAttributes(ActorAttributes.supervisionStrategy(cassandraSinkDecider))

}

object WhatIfFlow {
  def apply(streamHelper: RestApiFlow,
            whatIfRequestValidator: WhatIfRequestValidator,
            sparkJobRESTService: SparkJobRESTService): WhatIfFlow =
    new WhatIfFlow(streamHelper, whatIfRequestValidator, sparkJobRESTService)
}
