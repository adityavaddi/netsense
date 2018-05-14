package com.verizon.netsense.whatifservice.service

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.pattern.after
import akka.stream.ActorMaterializer
import com.verizon.netsense.utils.Logging
import com.verizon.netsense.whatifservice.config.ServiceConfig.{sparkCallRetrials, sparkImplicitTimeout}
import com.verizon.netsense.whatifservice.config.WhatIfConfigLoader.{
  confKey,
  confValue,
  livyPort,
  resourcePort,
  sparkClassName,
  sparkDriverMemory,
  sparkFileName,
  sparkLivyServer,
  sparkName,
  sparkProxyUser,
  sparkResourceManager,
  whatIfEnvSuffix
}
import com.verizon.netsense.whatifservice.db.WhatIfDbLayer
import com.verizon.netsense.whatifservice.exceptions.CustomExceptions.{
  InvalidSparkStatus,
  WhatifSparkCallException,
  WhatifSparkTimeoutException
}
import com.verizon.netsense.whatifservice.model.ResponseMessage._
import com.verizon.netsense.whatifservice.model.SparkJsonProtocol._
import com.verizon.netsense.whatifservice.model.WhatIfConstants.{AbnormallyEnded, _}
import com.verizon.netsense.whatifservice.model.casel.AppRequest
import com.verizon.netsense.whatifservice.model.{SparkResultToCassandra, _}
import com.verizon.netsense.whatifservice.util.{ObjectMapperUtil, RequestResponseGenerator}

import scala.concurrent.duration.{FiniteDuration, _}
import scala.concurrent.{ExecutionContextExecutor, Future, _}

class SparkJobRESTService(reqResGenerator: RequestResponseGenerator, dbLayer: WhatIfDbLayer) extends Logging {

  implicit val system: ActorSystem                        = ActorSystem()
  implicit val materializer: ActorMaterializer            = ActorMaterializer()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher

  implicit class FutureExtensions[T](f: Future[T]) {
    def withTimeout(timeout: => Throwable)(implicit duration: FiniteDuration, system: ActorSystem): Future[T] =
      Future firstCompletedOf Seq(f, after(duration, system.scheduler)(Future.failed(timeout)))
  }

  def postWhatIfToSparkJob(sparkJobRequest: SparkJobRequest): Future[HttpResponse] = {
    implicit val timeout = sparkImplicitTimeout.second
    val responseFuture: Future[HttpResponse] = Http().singleRequest(
      HttpRequest(method = HttpMethods.POST, uri = s"$sparkLivyServer:$livyPort/batches")
        .withEntity(
          ContentTypes.`application/json`,
          ObjectMapperUtil.toJson(
            payLoadForPostJob(
              sparkJobRequest.jobId,
              sparkJobRequest.orgId,
              sparkJobRequest.siteId,
              sparkJobRequest.parkingGroupList,
              sparkJobRequest.parkingPolicyList,
              sparkJobRequest.fromTime,
              sparkJobRequest.toTime,
              sparkJobRequest.mode,
              sparkJobRequest.aggregationType,
              sparkJobRequest.userEmail
            )
          )
        )
    )

    try {
      responseFuture withTimeout new WhatifSparkTimeoutException(SparkTimeOut.value) {
        override val underlyingMessage = ""
      }
    }
  }

  def unMarshallingResult(sparkJobRequest: SparkJobRequest): Future[SparkJobResult] =
    postWhatIfToSparkJob(sparkJobRequest) flatMap (
        e => Unmarshal(e.entity).to[SparkJobResult]
    )

  def retryFuture[T](retries: Int)(function: => Future[T]): Future[T] =
    function.recoverWith {
      case _ if retries > 1 =>
        log.warn("Retrying to call spark again")
        retryFuture(retries - 1)(function)
    }

  def getCurrentTime: Long = System.currentTimeMillis()

  def callSparkJob(sparkJobRequestTuple: (AppRequest, String, WhatIfSparkRequest)): Future[SparkResultToCassandra] =
    sparkJobRequestTuple._3.sparkJobRequest match {
      case Some(jobRequest) =>
        retryFuture(sparkCallRetrials)(unMarshallingResult(jobRequest))
          .map(sparkResponse => {
            log
              .info(
                s"Successfully received batchId: ${sparkResponse.id} for jobId: ${jobRequest.jobId} and job details: ${jobRequest} from sparkjob"
              )
            SparkResultToCassandra(jobRequest, jobId = Some(sparkResponse.id))
          })
          .recover {
            case ex: Exception =>
              log.error(
                s"Failed calling sparkjob $sparkCallRetrials times for jobId: ${jobRequest.jobId} ;" +
                s" Setting job status to ${AbnormallyEnded.value}"
              )
              SparkResultToCassandra(jobRequest, jobStatus = Some(AbnormallyEnded.value))
          }
      case None =>
        throw new WhatifSparkCallException(
          Spark_Payload.value + sparkJobRequestTuple._1
        ) {
          override val underlyingMessage = ""
        }
    }

  def abortWhatIfJobById(appId: String): Future[HttpResponse] = {
    log.debug(s"Entered abortWhatIfJobById with appId $appId")
    implicit val timeout = 1.second

    val responseFuture: Future[HttpResponse] = Http().singleRequest(
      HttpRequest(method = HttpMethods.PUT,
                  uri = s"$sparkResourceManager:$resourcePort/ws/v1/cluster/apps/$appId/state")
        .withEntity(ContentTypes.`application/json`,
                    ObjectMapperUtil.toJson(
                      payLoadForAbortJob
                    ))
    )

    try {
      responseFuture withTimeout new TimeoutException(Spark_API.value)
    }

  }

  def getStatusCode(appId: String): Future[Option[Int]] =
    abortWhatIfJobById(appId).map(e => Some(e._1.intValue()))

  def payLoadForPostJob(jobId: String,
                        orgId: String,
                        siteId: String,
                        parkingGroups: List[String],
                        parkingPolicies: List[String],
                        fromTime: Long,
                        toTime: Long,
                        mode: String,
                        aggregationType: String,
                        userEmail: String) = {
    val payLoadForSpark = Map(
      classNameKey.value    -> sparkClassName,
      fileKey.value         -> sparkFileName,
      driverMemoryKey.value -> sparkDriverMemory,
      nameKey.value         -> (sparkName + whatIfEnvSuffix),
      proxyUserKey.value    -> sparkProxyUser,
      confMapKey.value      -> Map(confKey -> confValue),
      argsKey.value -> Array(jobId,
                             orgId,
                             siteId,
                             parkingGroups.mkString(","),
                             parkingPolicies.mkString(","),
                             fromTime,
                             toTime,
                             mode,
                             aggregationType,
                             userEmail,
                             whatIfEnvSuffix)
    )
    log.info(s"spark inputs userEmail: ${userEmail},envSuffix:${whatIfEnvSuffix},mode: ${mode}")
    log.debug(s"payLoadForSpark ${payLoadForSpark}, Json format:", ObjectMapperUtil.toJson(payLoadForSpark))
    payLoadForSpark
  }

  def payLoadForAbortJob =
    Map("state" -> "KILLED")

  def writeSparkStatusToCassandra(sparkResultToCassandra: SparkResultToCassandra): Future[SuccessMessage] =
    (sparkResultToCassandra.jobId, sparkResultToCassandra.jobStatus) match {
      case (Some(bid), None)    => updateWhatIfBatchIdInDB(sparkResultToCassandra.sparkJobRequest, bid)
      case (None, Some(status)) => updateWhatIfJobStatusInDB(sparkResultToCassandra.sparkJobRequest, status)
      case er @ _ =>
        throw new InvalidSparkStatus(
          Invalid_Arguments.value + sparkResultToCassandra.sparkJobRequest + "," +
          BatchId.value + sparkResultToCassandra.jobId + Job.value + sparkResultToCassandra.jobStatus
        ) {
          override val underlyingMessage: String = ""
        }
    }

  def updateWhatIfBatchIdInDB(sparkJobRequest: SparkJobRequest, batchId: Int): Future[SuccessMessage] =
    dbLayer.updateWhatIfBatchId(sparkJobRequest.orgId,
                                sparkJobRequest.siteId,
                                sparkJobRequest.jobId,
                                Option(batchId),
                                Some(getCurrentTime))

  def updateWhatIfJobStatusInDB(sparkJobRequest: SparkJobRequest, jobStatus: String): Future[SuccessMessage] =
    dbLayer.updateWhatIfJobStatus(sparkJobRequest.orgId,
                                  sparkJobRequest.siteId,
                                  sparkJobRequest.jobId,
                                  AbnormallyEnded.value,
                                  Some(getCurrentTime),
                                  SparkFailureAddMessage.value)

}

object SparkJobRESTService {
  def apply(reqResGenerator: RequestResponseGenerator, dbLayer: WhatIfDbLayer): SparkJobRESTService =
    new SparkJobRESTService(reqResGenerator, dbLayer)
}
