package com.verizon.netsense.sse.specs.phantom

import com.outworkers.phantom.dsl.ResultSet
import com.verizon.netsense.sse.config.SSEConfigLoader._
import com.verizon.netsense.sse.model.{SSEEvent, SSESubscribeRequest}

import scala.concurrent.Future

trait PhantomServiceSpec extends MyDbProvider {

  val connectionTest = sseCassandraConnector
  def databaseTest   = new PhantomConnector(connectionTest)

  def insertSSEFilterTable(eachinsertSSEFilter: SSESubscribeRequest,
                           topicName: String,
                           sseStreamTtl: Long,
                           counter: Int): Future[ResultSet] =
    databaseTest.ssefiltertest.insertSSEFilter(eachinsertSSEFilter, topicName, sseStreamTtl, counter)

  def updateSSEFilterTable(eachupdateSSEFilter: SSESubscribeRequest,
                           sseStreamTtl: Long,
                           topicName: String,
                           counter: Int): Future[ResultSet] =
    databaseTest.ssefiltertest.updateSSEFilter(eachupdateSSEFilter, sseStreamTtl, topicName, counter)

  def updateSSEFilterHeartBeatTable(sSESubscribeRequest: SSESubscribeRequest,
                                    topicName: String,
                                    count: Int,
                                    sseStreamTtl: Long): Future[ResultSet] =
    databaseTest.ssefiltertest.updateSSEFilterHeartBeat(sSESubscribeRequest, topicName, count, sseStreamTtl)

  def deleteSSEFilterTable(eachUpdateSSEFilter: SSESubscribeRequest): Future[ResultSet] =
    databaseTest.ssefiltertest.deleteSSEFilter(eachUpdateSSEFilter)

  def enrichDeviceTable(nodeid: String): Future[Option[(String, String, String)]] =
    databaseTest.orgtabletest.getbyNodeID(nodeid)

  def filterDeviceTable(eachSSEEvent: SSEEvent): Future[List[(String, String, String)]] =
    databaseTest.ssefiltertest.getbyOrgandSiteID(eachSSEEvent)

  def getCounterTable(sSESubscribeRequest: SSESubscribeRequest): Future[Option[Int]] =
    databaseTest.ssefiltertest.getCounter(sSESubscribeRequest)
}
