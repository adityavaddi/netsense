package com.vz.ns.ts.service.db

import com.outworkers.phantom.dsl._
import com.outworkers.phantom.keys.PartitionKey
import com.vz.ns.ts.service.config.ConfigLoader.cassandraTable
import com.vz.ns.ts.service.model.Mapping

import scala.concurrent.{ExecutionContextExecutor, Future}

class DeviceMappingTable extends CassandraTable[ConcreteDeviceMapping, Mapping] {

  override def tableName: String = cassandraTable

  object nodeId extends StringColumn(this) with PartitionKey

  object tsDeviceId extends StringColumn(this)

  object createdOn extends DateTimeColumn(this) with ClusteringOrder

}

abstract class ConcreteDeviceMapping(implicit context: ExecutionContextExecutor)
    extends DeviceMappingTable
    with RootConnector {

  def store(deviceMapping: Mapping): Future[ResultSet] =
    insert
      .value(_.nodeId, deviceMapping.nodeId)
      .value(_.tsDeviceId, deviceMapping.tsDeviceId)
      .value(_.createdOn, deviceMapping.createdOn)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()

  def getNodeIdByTsDeviceId(nodeId: String): Future[Option[Mapping]] =
    select.where(_.nodeId eqs nodeId).one()

  def getAllMapping: Future[List[Mapping]] =
    select.fetch()
}
