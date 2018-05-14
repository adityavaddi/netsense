package com.verizon.netsense.sse.service

import com.outworkers.phantom.dsl.ResultSet
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.sse.model._
import com.verizon.netsense.sse.db.service.PhantomService
import com.verizon.netsense.utils.Logging
import nl.grons.metrics.scala.Timer

import scala.concurrent.{ExecutionContext, Future}

/**
 * Created by muppasw on 8/3/17.
 */
case class ResponseHelper(status: Boolean, sSESubscribeRequest: SSESubscribeRequest)

object SubscribeProcess extends Instrumented with Logging with PhantomService {

  private[this] val subscribeMainMetrics: Timer = metrics.timer("sse-stream-subscribe-main")

  private[this] val subscribeMetrics: Timer = metrics.timer("sse-stream-subscribe")

  private[this] val heartBeatMetrics: Timer = metrics.timer("sse-stream-heartbeat")

  private[this] val disconnectMetrics: Timer = metrics.timer("sse-stream-disconnect")

  implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  /*
   *  Validate the `type` from interface service. If valid sends success else bad request
   */
  def validateAndProcess(
      sseSubRequest: SSESubscribeRequest
  )(implicit ec: ExecutionContext): Future[SSESubscribeResponse] =
    subscribeMainMetrics.time {
      buildProcess(sseSubRequest)
    }

  def validateAndProcessBusinessAlert(
      sseSubBusinessAlertRequest: SSESubscribeBusinessAlertRequest
  )(implicit ec: ExecutionContext): Future[SSESubscribeBusinessAlertResponse] =
    subscribeMainMetrics.time { buildBusinessAlertProcess(sseSubBusinessAlertRequest) }

  /*
   *  Check the request.`type` from interface service
   */
  def buildProcess(
      sSESubscribeRequest: SSESubscribeRequest
  )(implicit ec: ExecutionContext): Future[SSESubscribeResponse] = {
    val fMsg                  = sSESubscribeRequest.request.`type` + ": Bad Request"
    val sMsg                  = sSESubscribeRequest.request.`type` + ": Success"
    val sStatusCode           = 200
    val fStatusCode           = 400
    val subscribeType: String = sSESubscribeRequest.request.`type`
    subscribeType match {
      case _ if subscribeType != "subscribe" && subscribeType != "heartbeat" && subscribeType != "disconnect" =>
        Future {
          buildResponseMsg(fStatusCode.toString, fMsg, sSESubscribeRequest)
        }
      case _
          if sSESubscribeRequest.request.orgprops.orgid != null && sSESubscribeRequest.request.siteprops.siteid != null =>
        val dbCallResult = processbuildFlow(sSESubscribeRequest)
        val sr = dbCallResult map { rs =>
          rs.wasApplied() match {
            case true  => buildResponseMsg(sStatusCode.toString, sMsg, sSESubscribeRequest)
            case false => buildResponseMsg(fStatusCode.toString, fMsg, sSESubscribeRequest)
          }
        }
        sr
      case _ =>
        Future {
          buildResponseMsg(fStatusCode.toString, fMsg, sSESubscribeRequest)
        }
    }
  }

  def buildBusinessAlertProcess(
      sseSubBusinessAlertRequest: SSESubscribeBusinessAlertRequest
  )(implicit ec: ExecutionContext): Future[SSESubscribeBusinessAlertResponse] = {
    val fMsg                  = sseSubBusinessAlertRequest.request.`type` + ": Bad Request"
    val sMsg                  = sseSubBusinessAlertRequest.request.`type` + ": Success"
    val sStatusCode           = 200
    val fStatusCode           = 400
    val subscribeType: String = sseSubBusinessAlertRequest.request.`type`
    subscribeType match {
      case _ if subscribeType != "subscribe" && subscribeType != "heartbeat" && subscribeType != "disconnect" =>
        Future {
          buildBusinessAlertResponseMsg(fStatusCode.toString, fMsg, sseSubBusinessAlertRequest)
        }
      case _
          if sseSubBusinessAlertRequest.request.orgprops.orgid != null && sseSubBusinessAlertRequest.request.siteprops.siteid != null =>
        val dbCallResult = processBusinessAlertbuildFlow(sseSubBusinessAlertRequest)
        val sr = dbCallResult map { rs =>
          rs.wasApplied() match {
            case true  => buildBusinessAlertResponseMsg(sStatusCode.toString, sMsg, sseSubBusinessAlertRequest)
            case false => buildBusinessAlertResponseMsg(fStatusCode.toString, fMsg, sseSubBusinessAlertRequest)
          }
        }
        sr
      case _ =>
        Future { buildBusinessAlertResponseMsg(fStatusCode.toString, fMsg, sseSubBusinessAlertRequest) }
    }
  }

  def processbuildFlow(sSESubscribeRequest: SSESubscribeRequest)(implicit ec: ExecutionContext): Future[ResultSet] =
    sSESubscribeRequest.request.`type` match {

      case "subscribe" =>
        subscribeMetrics.time {
          log.info("SSE-Subscribe-Request -->" + sSESubscribeRequest)
          SSEStreamHelper.sseSubscribe(sSESubscribeRequest)
        }
      case "heartbeat" =>
        heartBeatMetrics.time {

          log.info("SSE-HeartBeat-Request -->" + sSESubscribeRequest)
          SSEStreamHelper.sseHeartBeat(sSESubscribeRequest)

        }
      case "disconnect" =>
        disconnectMetrics.time {
          log.info("SSE-Disconnect-Request -->" + sSESubscribeRequest)
          SSEStreamHelper.sseDisconnect(sSESubscribeRequest)
        }
    }

  def processBusinessAlertbuildFlow(
      sseSubBusinessAlertRequest: SSESubscribeBusinessAlertRequest
  )(implicit ec: ExecutionContext): Future[ResultSet] =
    sseSubBusinessAlertRequest.request.`type` match {

      case "subscribe" =>
        subscribeMetrics.time {
          log.info("SSE-BusinessAlertSubscribe-Request -->" + sseSubBusinessAlertRequest)
          SSEStreamHelper.sseBusinessAlertSubscribe(sseSubBusinessAlertRequest)
        }
      case "heartbeat" =>
        heartBeatMetrics.time {

          log.info("SSE-BusinessAlertHeartBeat-Request -->" + sseSubBusinessAlertRequest)
          SSEStreamHelper.sseBusinessAlertHeartBeat(sseSubBusinessAlertRequest)

        }
      case "disconnect" =>
        disconnectMetrics.time {
          log.info("SSE-BusinessAlertDisconnect-Request -->" + sseSubBusinessAlertRequest)
          SSEStreamHelper.sseBusinessAlertDisconnect(sseSubBusinessAlertRequest)
        }
    }
  def buildResponseMsg(statusCode: String,
                       statusMsg: String,
                       sseSubRequest: SSESubscribeRequest): SSESubscribeResponse =
    SSESubscribeResponse(
      sseSubRequest.messageid,
      sseSubRequest.responsetopic,
      SSEResponse(
        sseSubRequest.request.instanceid,
        sseSubRequest.request.requestid,
        sseSubRequest.request.timestamp,
        sseSubRequest.request.`type`,
        sseSubRequest.request.model,
        sseSubRequest.request.user,
        sseSubRequest.request.orgprops,
        sseSubRequest.request.siteprops,
        sseSubRequest.request.nodeprops,
        sseSubRequest.request.extprops,
        StatusProps(statusCode.toString, statusMsg)
      )
    )

  def buildBusinessAlertResponseMsg(
      statusCode: String,
      statusMsg: String,
      sseSubBusinessAlertRequest: SSESubscribeBusinessAlertRequest
  ): SSESubscribeBusinessAlertResponse =
    SSESubscribeBusinessAlertResponse(
      sseSubBusinessAlertRequest.messageid,
      sseSubBusinessAlertRequest.responsetopic,
      SSEBusinessResponse(
        sseSubBusinessAlertRequest.request.instanceid,
        sseSubBusinessAlertRequest.request.requestid,
        sseSubBusinessAlertRequest.request.timestamp,
        sseSubBusinessAlertRequest.request.`type`,
        sseSubBusinessAlertRequest.request.model,
        sseSubBusinessAlertRequest.request.user,
        sseSubBusinessAlertRequest.request.orgprops,
        sseSubBusinessAlertRequest.request.siteprops,
        sseSubBusinessAlertRequest.request.businessalertprops,
        StatusProps(statusCode.toString, statusMsg)
      )
    )

}
