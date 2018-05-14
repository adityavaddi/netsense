package com.verizon.netsense.sse.service

/**
 * Created by muppasw on 8/8/17.
 */
import com.outworkers.phantom.dsl.ResultSet
import com.verizon.netsense.sse.config.SSEConfigLoader._
import com.verizon.netsense.sse.db.service.PhantomService
import com.verizon.netsense.sse.model.{SSESubscribeBusinessAlertRequest, SSESubscribeRequest}
import com.verizon.netsense.utils.Logging

import scala.concurrent.{ExecutionContext, Future}

object SSEStreamHelper extends PhantomService with Logging {

  /*
   * when type is `subscribe` data is inserted into sse_filter table
   */
  def sseSubscribe(sseSubRequest: SSESubscribeRequest)(implicit ec: ExecutionContext): Future[ResultSet] =
    getCounter(sseSubRequest) flatMap {
      case Some(count) =>
        if (count > 0) {
          updateSSEFilter(sseSubRequest, sseStreamTtl, buildTopicName(sseSubRequest), count + 1)
        } else {
          insertSSEFilter(sseSubRequest, buildTopicName(sseSubRequest), sseStreamTtl, 1)
        }
      case None =>
        insertSSEFilter(sseSubRequest, buildTopicName(sseSubRequest), sseStreamTtl, 1) //throw new RuntimeException("Exception while insert/update the SSE Filter table !!!")
    }

  def sseBusinessAlertSubscribe(
      sseSubBusinessAlertRequest: SSESubscribeBusinessAlertRequest
  )(implicit ec: ExecutionContext): Future[ResultSet] =
    getbussinessAlertCounter(sseSubBusinessAlertRequest) flatMap {
      case Some(businesscount) =>
        if (businesscount > 0) {
          updateBusinessAlertSSEFilter(sseSubBusinessAlertRequest,
                                       sseStreamTtl,
                                       businessAlertbuildTopicName(sseSubBusinessAlertRequest),
                                       businesscount + 1)
        } else {
          insertBusinessAlertSSEFilter(sseSubBusinessAlertRequest,
                                       businessAlertbuildTopicName(sseSubBusinessAlertRequest),
                                       sseStreamTtl,
                                       1)
        }
      case None =>
        insertBusinessAlertSSEFilter(
          sseSubBusinessAlertRequest,
          businessAlertbuildTopicName(sseSubBusinessAlertRequest),
          sseStreamTtl,
          1
        ) //throw new RuntimeException("Exception while insert/update the SSE Filter table !!!")
    }

  /*
   * when type is `heartbeat` data is updated into sse_filter table and resets the ttl
   */

  def sseHeartBeat(sseSubRequest: SSESubscribeRequest)(implicit ec: ExecutionContext): Future[ResultSet] =
    getCounter(sseSubRequest) flatMap {
      case Some(count) => {
        updateSSEFilterHeartBeat(sseSubRequest, buildTopicName(sseSubRequest), count, sseStreamTtl)
      }
      case None => updateSSEFilterHeartBeat(sseSubRequest, buildTopicName(sseSubRequest), 1, sseStreamTtl)
    }

  def sseBusinessAlertHeartBeat(
      sseSubBusinessAlertRequest: SSESubscribeBusinessAlertRequest
  )(implicit ec: ExecutionContext): Future[ResultSet] =
    getbussinessAlertCounter(sseSubBusinessAlertRequest) flatMap {
      case Some(businesscount) => {
        updateBusinessAlertSSEFilterHeartBeat(sseSubBusinessAlertRequest,
                                              businessAlertbuildTopicName(sseSubBusinessAlertRequest),
                                              businesscount,
                                              sseStreamTtl)
      }
      case None =>
        updateBusinessAlertSSEFilterHeartBeat(sseSubBusinessAlertRequest,
                                              businessAlertbuildTopicName(sseSubBusinessAlertRequest),
                                              1,
                                              sseStreamTtl)
    }

  /*
   * when type is `disconnect` data is deleted from sse_filter table
   */
  def sseDisconnect(sseSubRequest: SSESubscribeRequest)(implicit ec: ExecutionContext): Future[ResultSet] =
    getCounter(sseSubRequest) flatMap {
      case Some(count) =>
        if (count > 1) {
          updateSSEFilter(sseSubRequest, sseStreamTtl, buildTopicName(sseSubRequest), count - 1)
        } else {
          deleteSSEFilter(sseSubRequest)
        }
      case None =>
        Future {
          throw new RuntimeException("No data found in sse-filter table when heartbeat/disconnect request type is sent!!! ")

        }
    }

  def sseBusinessAlertDisconnect(
      sseSubBusinessAlertRequest: SSESubscribeBusinessAlertRequest
  )(implicit ec: ExecutionContext): Future[ResultSet] =
    getbussinessAlertCounter(sseSubBusinessAlertRequest) flatMap {
      case Some(businesscount) =>
        if (businesscount > 1) {
          updateBusinessAlertSSEFilter(sseSubBusinessAlertRequest,
                                       sseStreamTtl,
                                       businessAlertbuildTopicName(sseSubBusinessAlertRequest),
                                       businesscount - 1)
        } else {
          deleteBusinessAlertSSEFilter(sseSubBusinessAlertRequest)
        }
      case None =>
        Future {
          throw new RuntimeException(
            "No data found while update/delete in the SSE BusinessAlert Filter table for disconnect !!! check for ttl"
          )
        }
    }

  /*
   * build the topicname as "/streamv1/orgid/siteid/nodeid/eventType" in sse_filter table
   */
  def buildTopicName(sseSubRequest: SSESubscribeRequest): String = {

    var topicName = kafkaStreamingTopicPrefix + sseSubRequest.request.orgprops.orgid + "/" + sseSubRequest.request.siteprops.siteid + "/" + sseSubRequest.request.nodeprops.nodeid + "/" + sseSubRequest.request.extprops.eventtype
    topicName
  }

  /*
   * /streamv1/orgid/siteid/businessalert/{application}/{triggerid}
   */

  def businessAlertbuildTopicName(sseSubBusinessAlertRequest: SSESubscribeBusinessAlertRequest): String = {
    var businessAlerttopicName = kafkaStreamingTopicPrefix + sseSubBusinessAlertRequest.request.orgprops.orgid + "/" + sseSubBusinessAlertRequest.request.siteprops.siteid + "/" + businessAlertTopicSuffix + "/" + sseSubBusinessAlertRequest.request.businessalertprops.application + "/" + sseSubBusinessAlertRequest.request.businessalertprops.triggerid
    businessAlerttopicName
  }
}
