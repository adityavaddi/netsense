package com.verizon.netsense.sse.simulator.database

import com.outworkers.phantom.dsl._
import com.verizon.netsense.sse.simulator.config.ConfigLoader
import com.verizon.netsense.sse.simulator.model.{OrgEvent, SSESubscribeBusinessAlertRequest, SSESubscribeRequest}

import scala.concurrent.{ExecutionContextExecutor, Future}

/**
 *
 * Created by subrsi9 on 11/24/17.
 */
class OrgByNodeTable extends CassandraTable[ConcreteOrgByNode, OrgEvent] {

  override def tableName: String = ConfigLoader.sseLookupTable
  object orgid  extends StringColumn(this) with PartitionKey
  object siteid extends StringColumn(this) with PartitionKey
  object nodeid extends StringColumn(this) with PrimaryKey
}

class SSEFilterTable extends CassandraTable[ConcreteFilterEvent, SSESubscribeRequest] {
  override def tableName: String = ConfigLoader.sseStreamSubscribeTtlTable
  object orgid     extends StringColumn(this) with PrimaryKey
  object siteid    extends StringColumn(this) with PrimaryKey
  object nodeid    extends StringColumn(this) with PrimaryKey
  object eventType extends StringColumn(this) with PartitionKey
  object topicName extends StringColumn(this)
  object counter   extends IntColumn(this)
}

class SSEFilterBusinessAlertTable
    extends CassandraTable[ConcreteBusinessAlertFilterEvent, SSESubscribeBusinessAlertRequest] {
  override def tableName: String = ConfigLoader.sseStreambusinessAlertSubscribeTtlTable
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

  def insertOrgEvent(eachOrgEvent: OrgEvent) =
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

  def insertSSEFilter(sSESubscribeRequest: SSESubscribeRequest, topicName: String, sseStreamTtl: Long, counter: Int) = {
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
}
