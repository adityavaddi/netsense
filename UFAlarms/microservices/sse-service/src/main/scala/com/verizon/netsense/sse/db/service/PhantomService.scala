package com.verizon.netsense.sse.db.service

import com.outworkers.phantom.dsl.ResultSet
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.sse.config.SSEConfigLoader._
import com.verizon.netsense.sse.db.database.{MicroservicesDatabaseProvider, SSEDatabase}
import com.verizon.netsense.sse.exceptions.CustomExceptions.CassandraDBException
import com.verizon.netsense.sse.model.{SSEEvent, SSESubscribeBusinessAlertRequest, SSESubscribeRequest}
import nl.grons.metrics.scala.Timer

import scala.concurrent.{ExecutionContext, Future}

trait PhantomService extends Instrumented with MicroservicesDatabaseProvider {

  private[this] val insertSubscribe: Timer = metrics.timer("sse-subscribe-db-insert")

  private[this] val updateSubscribe: Timer = metrics.timer("sse-subscribe-db-update")

  private[this] val updateHeartBeat: Timer = metrics.timer("sse-subscribe-db-heartbeat-update")

  private[this] val deleteSubscribe: Timer = metrics.timer("sse-subscribe-db-delete")

  private[this] val getTopic: Timer = metrics.timer("sse-subscribe-db-get-topic")

  private[this] val getCount: Timer = metrics.timer("sse-subscribe-db-get-count")

  private[this] val enrichGet: Timer = metrics.timer("sse-event-enrich-db-get-orgsite")

  val connection = sseCassandraConnector
  def database   = new SSEDatabase(connection)

  def insertSSEFilter(eachinsertSSEFilter: SSESubscribeRequest, topicName: String, sseStreamTtl: Long, counter: Int)(
      implicit ec: ExecutionContext
  ): Future[ResultSet] = insertSubscribe.time {
    database.sseFilterEvent
      .insertSSEFilter(eachinsertSSEFilter, topicName, sseStreamTtl, counter)
      .recover {
        case ex: Exception =>
          throw new CassandraDBException("Failed to insert data in sse_filter table: "
                                         + eachinsertSSEFilter + ex.getMessage,
                                         ex.getCause)
      }
  }

  def updateSSEFilter(eachupdateSSEFilter: SSESubscribeRequest, sseStreamTtl: Long, topicName: String, counter: Int)(
      implicit ec: ExecutionContext
  ): Future[ResultSet] =
    updateSubscribe.time {
      database.sseFilterEvent.updateSSEFilter(eachupdateSSEFilter, sseStreamTtl, topicName, counter).recover {
        case ex: Exception =>
          throw new CassandraDBException("Failed to update data in sse_filter table: "
                                         + eachupdateSSEFilter + ex.getMessage,
                                         ex.getCause)
      }
    }

  def updateSSEFilterHeartBeat(sSESubscribeRequest: SSESubscribeRequest,
                               topicName: String,
                               count: Int,
                               sseStreamTtl: Long)(implicit ec: ExecutionContext): Future[ResultSet] =
    updateHeartBeat.time {
      database.sseFilterEvent.updateSSEFilterHeartBeat(sSESubscribeRequest, topicName, count, sseStreamTtl).recover {
        case ex: Exception =>
          throw new CassandraDBException("Failed to update heartbeat data in sse_filter table: "
                                         + sSESubscribeRequest + ex.getMessage,
                                         ex.getCause)
      }
    }

  def deleteSSEFilter(eachDeleteSSEFilter: SSESubscribeRequest)(implicit ec: ExecutionContext): Future[ResultSet] =
    deleteSubscribe.time {
      database.sseFilterEvent.deleteSSEFilter(eachDeleteSSEFilter).recover {
        case ex: Exception =>
          throw new CassandraDBException("Failed to delete data in sse_filter table: "
                                         + eachDeleteSSEFilter + ex.getMessage,
                                         ex.getCause)
      }
    }

  /*
   * sse-filter-business-alerts
   */
  def insertBusinessAlertSSEFilter(eachBusinessAlertinsertSSEFilter: SSESubscribeBusinessAlertRequest,
                                   topicName: String,
                                   sseStreamTtl: Long,
                                   counter: Int)(implicit ec: ExecutionContext): Future[ResultSet] =
    insertSubscribe.time {
      database.sseBusinessAlertFilterEvent
        .insertBussinessAlertSSEFilter(eachBusinessAlertinsertSSEFilter, topicName, sseStreamTtl, counter)
        .recover {
          case ex: Exception =>
            throw new CassandraDBException("Failed to insert data in sse_filter_businessalerts table: "
                                           + eachBusinessAlertinsertSSEFilter + ex.getMessage,
                                           ex.getCause)
        }
    }

  def updateBusinessAlertSSEFilter(eachBusinessAlertupdateSSEFilter: SSESubscribeBusinessAlertRequest,
                                   sseStreamTtl: Long,
                                   topicName: String,
                                   counter: Int)(implicit ec: ExecutionContext): Future[ResultSet] =
    updateSubscribe.time {
      database.sseBusinessAlertFilterEvent
        .updateBussinessAlertSSEFilter(eachBusinessAlertupdateSSEFilter, sseStreamTtl, topicName, counter)
        .recover {
          case ex: Exception =>
            throw new CassandraDBException("Failed to update data in sse_filter_businessalerts table: "
                                           + eachBusinessAlertupdateSSEFilter + ex.getMessage,
                                           ex.getCause)
        }
    }

  def updateBusinessAlertSSEFilterHeartBeat(sSESubscribeBusinessAlertRequest: SSESubscribeBusinessAlertRequest,
                                            topicName: String,
                                            count: Int,
                                            sseStreamTtl: Long)(implicit ec: ExecutionContext): Future[ResultSet] =
    updateHeartBeat.time {
      database.sseBusinessAlertFilterEvent
        .updateBussinessAlertSSEFilterHeartBeat(sSESubscribeBusinessAlertRequest, topicName, count, sseStreamTtl)
        .recover {
          case ex: Exception =>
            throw new CassandraDBException("Failed to update/reset heartbeat data in sse_filter_businessalerts table: "
                                           + sSESubscribeBusinessAlertRequest + ex.getMessage,
                                           ex.getCause)
        }
    }

  def deleteBusinessAlertSSEFilter(
      eachBusinessAlertUpdateSSEFilter: SSESubscribeBusinessAlertRequest
  )(implicit ec: ExecutionContext): Future[ResultSet] = deleteSubscribe.time {
    database.sseBusinessAlertFilterEvent.deleteBussinessAlertSSEFilter(eachBusinessAlertUpdateSSEFilter).recover {
      case ex: Exception =>
        throw new CassandraDBException("Failed to delete data in sse_filter_businessalerts table: "
                                       + eachBusinessAlertUpdateSSEFilter + ex.getMessage,
                                       ex.getCause)
    }
  }

  def enrichDevice(nodeid: String)(implicit ec: ExecutionContext): Future[Option[(String, String, String)]] =
    enrichGet.time {
      database.orgByNode$.getbyNodeID(nodeid).recover {
        case ex: Exception =>
          throw new CassandraDBException("Failed to enrich orgid and siteid based on nodeid: "
                                         + nodeid + ex.getMessage,
                                         ex.getCause)
      }
    }

  def filterDevice(eachSSEEvent: SSEEvent)(implicit ec: ExecutionContext): Future[List[(String, String, String)]] =
    getTopic.time {
      database.sseFilterEvent.getbyOrgandSiteID(eachSSEEvent).recover {
        case ex: Exception =>
          throw new CassandraDBException("Failed to filter data nodeid and eventtype from sse_filter_table: "
                                         + eachSSEEvent + ex.getMessage,
                                         ex.getCause)
      }
    }

  def getCounter(sSESubscribeRequest: SSESubscribeRequest)(implicit ec: ExecutionContext): Future[Option[Int]] =
    getCount.time {
      database.sseFilterEvent.getCounter(sSESubscribeRequest).recover {
        case ex: Exception =>
          throw new CassandraDBException("Failed to get counter in sse_filter_table: "
                                         + sSESubscribeRequest + ex.getMessage,
                                         ex.getCause)
      }
    }

  def getbussinessAlertCounter(
      sSESubscribeBusinessAlertRequest: SSESubscribeBusinessAlertRequest
  )(implicit ec: ExecutionContext): Future[Option[Int]] = getCount.time {
    database.sseBusinessAlertFilterEvent.bussinessAlertgetCounter(sSESubscribeBusinessAlertRequest).recover {
      case ex: Exception =>
        throw new CassandraDBException("Failed to get counter in sse_filter_businessalerts table: "
                                       + sSESubscribeBusinessAlertRequest + ex.getMessage,
                                       ex.getCause)
    }
  }
}
