package com.verizon.netsense.sse.db.model

import com.outworkers.phantom.dsl._
import com.verizon.netsense.sse.model.{OrgEvent, SSEEvent, SSESubscribeBusinessAlertRequest, SSESubscribeRequest}
import com.verizon.netsense.sse.config.SSEConfigLoader

import scala.concurrent.{ExecutionContextExecutor, Future}

class OrgByNodeTable extends CassandraTable[ConcreteOrgByNode, OrgEvent] {

  override def tableName: String = SSEConfigLoader.sseLookupTable
  object orgid  extends StringColumn(this) with PartitionKey
  object siteid extends StringColumn(this) with PartitionKey
  object nodeid extends StringColumn(this) with PrimaryKey
}

class SSEFilterTable extends CassandraTable[ConcreteFilterEvent, SSESubscribeRequest] {
  override def tableName: String = SSEConfigLoader.sseStreamSubscribeTtlTable
  object orgid     extends StringColumn(this) with PrimaryKey
  object siteid    extends StringColumn(this) with PrimaryKey
  object nodeid    extends StringColumn(this) with PrimaryKey
  object eventType extends StringColumn(this) with PartitionKey
  object topicName extends StringColumn(this)
  object counter   extends IntColumn(this)
}

class SSEFilterBusinessAlertTable
    extends CassandraTable[ConcreteBusinessAlertFilterEvent, SSESubscribeBusinessAlertRequest] {
  override def tableName: String = SSEConfigLoader.sseStreamBusinnesalertsTable
  object orgid       extends StringColumn(this) with PrimaryKey
  object siteid      extends StringColumn(this) with PrimaryKey
  object application extends StringColumn(this) with PrimaryKey
  object triggerid   extends StringColumn(this) with PartitionKey
  object topicName   extends StringColumn(this)
  object counter     extends IntColumn(this)
}

abstract class ConcreteOrgByNode(implicit context: ExecutionContextExecutor)
    extends OrgByNodeTable
    with RootConnector {

  def getbyNodeID(nodeid: String): Future[Option[(String, String, String)]] =
    select(_.nodeid, _.orgid, _.siteid)
      .where(_.nodeid eqs nodeid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .one()

  def insertOrgEvent(eachOrgEvent: OrgEvent): Future[ResultSet] =
    insert
      .value(_.orgid, eachOrgEvent.orgid)
      .value(_.siteid, eachOrgEvent.siteid)
      .value(_.nodeid, eachOrgEvent.nodeid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()
}

abstract class ConcreteFilterEvent(implicit context: ExecutionContextExecutor)
    extends SSEFilterTable
    with RootConnector {

  def insertSSEFilter(sSESubscribeRequest: SSESubscribeRequest,
                      topicName: String,
                      sseStreamTtl: Long,
                      counter: Int): Future[ResultSet] = {
    val query = insert
      .value(_.orgid, sSESubscribeRequest.request.orgprops.orgid)
      .value(_.siteid, sSESubscribeRequest.request.siteprops.siteid)
      .value(_.nodeid, sSESubscribeRequest.request.nodeprops.nodeid)
      .value(_.eventType, sSESubscribeRequest.request.extprops.eventtype)
      .value(_.topicName, topicName)
      .value(_.counter, counter)
      .ttl(sseStreamTtl)
      .consistencyLevel_=(ConsistencyLevel.ONE)
    query.future()
  }

  def updateSSEFilter(sSESubscribeRequest: SSESubscribeRequest,
                      sseStreamTtl: Long,
                      topicName: String,
                      counter: Int): Future[ResultSet] = {
    val baseQuery = update
      .where(_.orgid eqs sSESubscribeRequest.request.orgprops.orgid)
      .and(_.siteid eqs sSESubscribeRequest.request.siteprops.siteid)
      .and(_.nodeid eqs sSESubscribeRequest.request.nodeprops.nodeid)
      .and(_.eventType eqs sSESubscribeRequest.request.extprops.eventtype)
      .modify(_.counter setTo counter)
      .and(_.topicName setTo topicName)
      .ttl(sseStreamTtl)

    baseQuery.consistencyLevel_=(ConsistencyLevel.ONE).future()
  }

  def updateSSEFilterHeartBeat(sSESubscribeRequest: SSESubscribeRequest,
                               topicName: String,
                               count: Int,
                               sseStreamTtl: Long): Future[ResultSet] = {
    val baseQuery = update
      .where(_.orgid eqs sSESubscribeRequest.request.orgprops.orgid)
      .and(_.siteid eqs sSESubscribeRequest.request.siteprops.siteid)
      .and(_.nodeid eqs sSESubscribeRequest.request.nodeprops.nodeid)
      .and(_.eventType eqs sSESubscribeRequest.request.extprops.eventtype)
      .modify(_.counter setTo count)
      .and(_.topicName setTo topicName)
      .ttl(sseStreamTtl)
    baseQuery.consistencyLevel_=(ConsistencyLevel.ONE).future()

  }

  def deleteSSEFilter(sSESubscribeRequest: SSESubscribeRequest): Future[ResultSet] = {

    val baseQuery = delete
      .where(_.orgid eqs sSESubscribeRequest.request.orgprops.orgid)
      .and(_.siteid eqs sSESubscribeRequest.request.siteprops.siteid)
      .and(_.nodeid eqs sSESubscribeRequest.request.nodeprops.nodeid)
      .and(_.eventType eqs sSESubscribeRequest.request.extprops.eventtype)
    baseQuery.consistencyLevel_=(ConsistencyLevel.ONE).future()
  }

  def getbyOrgandSiteID(eachSSEEvent: SSEEvent): Future[List[(String, String, String)]] =
    select(_.nodeid, _.eventType, _.topicName)
      .where(_.orgid eqs eachSSEEvent.orgid)
      .and(_.siteid eqs eachSSEEvent.siteid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .allowFiltering()
      .fetch()

  def getCounter(sSESubscribeRequest: SSESubscribeRequest): Future[Option[(Int)]] = {
    val baseQuery = select(_.counter)
      .where(_.orgid eqs sSESubscribeRequest.request.orgprops.orgid)
      .and(_.siteid eqs sSESubscribeRequest.request.siteprops.siteid)
      .and(_.nodeid eqs sSESubscribeRequest.request.nodeprops.nodeid)
      .and(_.eventType eqs sSESubscribeRequest.request.extprops.eventtype)
      .consistencyLevel_=(ConsistencyLevel.ONE)
    baseQuery.one()

  }

}

abstract class ConcreteBusinessAlertFilterEvent(implicit context: ExecutionContextExecutor)
    extends SSEFilterBusinessAlertTable
    with RootConnector {

  def insertBussinessAlertSSEFilter(sSESubscribeBusinessAlertRequest: SSESubscribeBusinessAlertRequest,
                                    topicName: String,
                                    sseStreamTtl: Long,
                                    counter: Int): Future[ResultSet] = {
    val query = insert
      .value(_.orgid, sSESubscribeBusinessAlertRequest.request.orgprops.orgid)
      .value(_.siteid, sSESubscribeBusinessAlertRequest.request.siteprops.siteid)
      .value(_.application, sSESubscribeBusinessAlertRequest.request.businessalertprops.application)
      .value(_.triggerid, sSESubscribeBusinessAlertRequest.request.businessalertprops.triggerid)
      .value(_.topicName, topicName)
      .value(_.counter, counter)
      .ttl(sseStreamTtl)
      .consistencyLevel_=(ConsistencyLevel.ONE)
    query.future()
  }

  def updateBussinessAlertSSEFilter(sSESubscribeBusinessAlertRequest: SSESubscribeBusinessAlertRequest,
                                    sseStreamTtl: Long,
                                    topicName: String,
                                    counter: Int): Future[ResultSet] = {
    val baseQuery = update
      .where(_.orgid eqs sSESubscribeBusinessAlertRequest.request.orgprops.orgid)
      .and(_.siteid eqs sSESubscribeBusinessAlertRequest.request.siteprops.siteid)
      .and(_.application eqs sSESubscribeBusinessAlertRequest.request.businessalertprops.application)
      .and(_.triggerid eqs sSESubscribeBusinessAlertRequest.request.businessalertprops.triggerid)
      .modify(_.counter setTo counter)
      .and(_.topicName setTo topicName)
      .ttl(sseStreamTtl)

    baseQuery.consistencyLevel_=(ConsistencyLevel.ONE).future()
  }

  def updateBussinessAlertSSEFilterHeartBeat(sSESubscribeBusinessAlertRequest: SSESubscribeBusinessAlertRequest,
                                             topicName: String,
                                             count: Int,
                                             sseStreamTtl: Long): Future[ResultSet] = {
    val baseQuery = update
      .where(_.orgid eqs sSESubscribeBusinessAlertRequest.request.orgprops.orgid)
      .and(_.siteid eqs sSESubscribeBusinessAlertRequest.request.siteprops.siteid)
      .and(_.application eqs sSESubscribeBusinessAlertRequest.request.businessalertprops.application)
      .and(_.triggerid eqs sSESubscribeBusinessAlertRequest.request.businessalertprops.triggerid)
      .modify(_.counter setTo count)
      .and(_.topicName setTo topicName)
      .ttl(sseStreamTtl)
    baseQuery.consistencyLevel_=(ConsistencyLevel.ONE).future()

  }

  def deleteBussinessAlertSSEFilter(
      sSESubscribeBusinessAlertRequest: SSESubscribeBusinessAlertRequest
  ): Future[ResultSet] = {

    val baseQuery = delete
      .where(_.orgid eqs sSESubscribeBusinessAlertRequest.request.orgprops.orgid)
      .and(_.siteid eqs sSESubscribeBusinessAlertRequest.request.siteprops.siteid)
      .and(_.application eqs sSESubscribeBusinessAlertRequest.request.businessalertprops.application)
      .and(_.triggerid eqs sSESubscribeBusinessAlertRequest.request.businessalertprops.triggerid)
    baseQuery.consistencyLevel_=(ConsistencyLevel.ONE).future()
  }

  def bussinessAlertgetCounter(
      sSESubscribeBusinessAlertRequest: SSESubscribeBusinessAlertRequest
  ): Future[Option[(Int)]] = {
    val baseQuery = select(_.counter)
      .where(_.orgid eqs sSESubscribeBusinessAlertRequest.request.orgprops.orgid)
      .and(_.siteid eqs sSESubscribeBusinessAlertRequest.request.siteprops.siteid)
      .and(_.application eqs sSESubscribeBusinessAlertRequest.request.businessalertprops.application)
      .and(_.triggerid eqs sSESubscribeBusinessAlertRequest.request.businessalertprops.triggerid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
    baseQuery.one()

  }

}
