package com.vz.nsp.parking.policyservice.db

import com.outworkers.phantom.builder.query.execution.{ExecutableStatements, QueryCollection}
import com.outworkers.phantom.connectors.ContactPoints
import com.outworkers.phantom.dsl.{Database, KeySpaceDef, ResultSet, context => _, _}
import com.outworkers.phantom.udt.UDTPrimitive
import com.verizon.netsense.utils.ConfigLoader
import com.vz.nsp.parking.model._

import scala.concurrent.{ExecutionContextExecutor, Future}


object Defaults {
  val cassandraHost = ConfigLoader.config.getString("cassandra.host")
  val cassandraKeyspace = ConfigLoader.config.getString("cassandra.keyspace")
  val keySpaceBuilder = ContactPoints(Seq(cassandraHost))
  val connection = keySpaceBuilder.keySpace(cassandraKeyspace)
}

class PolicyConnector(override val connector: KeySpaceDef) extends Database[PolicyConnector](connector) {
  outer =>
  def initPolicy: QueryCollection[Seq] = {
    new QueryCollection[Seq](

      Seq(
        UDTPrimitive[Period].schemaQuery(),
        UDTPrimitive[DatePeriod].schemaQuery(),
        UDTPrimitive[TimeRange].schemaQuery(),
        UDTPrimitive[ChargeDuration].schemaQuery(),
        UDTPrimitive[MaxDuration].schemaQuery(),
        UDTPrimitive[ViolationFine].schemaQuery(),
        UDTPrimitive[ParkingPenalty].schemaQuery(),
        UDTPrimitive[ParkingCharge].schemaQuery(),
        UDTPrimitive[ParkingRule].schemaQuery(),
        UDTPrimitive[ParkingSchedule].schemaQuery(),
        UDTPrimitive[PolicyLevelViolation].schemaQuery(),
        UDTPrimitive[PolicyRule].schemaQuery()
      )
    )
  }

  def customTypesPolicy()(implicit ex: ExecutionContextExecutor): Future[Seq[ResultSet]] = {
    new ExecutableStatements(initPolicy).sequence flatMap (_ => outer.createAsync())
  }

  object tagTable extends ConcreteTag with connector.Connector

  object policyMappingTable extends ConcretePolicy with connector.Connector

  object parkingGroupPolicyLinkTable extends ConcreteGroupPolicy with connector.Connector

  object whatIfPolicyTable extends ConcreteWhatIfPolicy with connector.Connector

}


object PolicyConnector extends PolicyConnector(Defaults.connection)
