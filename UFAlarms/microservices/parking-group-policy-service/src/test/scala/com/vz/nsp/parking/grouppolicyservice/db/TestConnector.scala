package com.vz.nsp.parking.grouppolicyservice.db

import com.outworkers.phantom.builder.QueryBuilder
import com.outworkers.phantom.builder.query.execution.{ExecutableCqlQuery, ExecutableStatements, QueryCollection}
import com.outworkers.phantom.connectors.ContactPoints
import com.outworkers.phantom.dsl.{context => _, _}
import com.outworkers.phantom.udt.UDTPrimitive
import com.verizon.netsense.utils.ConfigLoader
import com.vz.nsp.parking.model._

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}

object Defaults {
  val cassandraHost = ConfigLoader.config.getString("cassandra.host")
  val cassandraKeyspace = ConfigLoader.config.getString("cassandra.keyspace")
  val keySpaceBuilder = ContactPoints(Seq(cassandraHost))
  val connection = keySpaceBuilder.keySpace(cassandraKeyspace)
}

class parkingServicesDbTest(override val connector: KeySpaceDef) extends Database[parkingServicesDbTest](connector) {
  outer =>
  def indexesForPolicy: QueryCollection[Seq] = {
    val name = "farallones"
    new QueryCollection[Seq](

      Seq(
        ExecutableCqlQuery(QueryBuilder.Create.index(ParkingSpaceTable.tableName, name, "orgid")),
        ExecutableCqlQuery(QueryBuilder.Create.index(ParkingSpaceTable.tableName, name, "siteid")),
        ExecutableCqlQuery(QueryBuilder.Create.index(ParkingSpaceTable.tableName, name, "isdeleted")),
        ExecutableCqlQuery(QueryBuilder.Create.index(ParkingSpaceTable.tableName, name, "tags")),
        ExecutableCqlQuery(QueryBuilder.Create.index(ParkingSpaceTable.tableName, name, "name"))
      )
    )
  }

  def create()(implicit ex: ExecutionContextExecutor): Future[Seq[ResultSet]] = {
    new ExecutableStatements(initUds).sequence flatMap (_ => outer.createAsync())
  }

  def initUds: QueryCollection[Seq] = {
    new QueryCollection[Seq](

      Seq(
        UDTPrimitive[GeoCoordinate].schemaQuery(),
        UDTPrimitive[SpaceGeometry].schemaQuery()
      )
    )
  }

  def customTypesPolicy()(implicit ex: ExecutionContextExecutor): Future[Seq[ResultSet]] = {
    new ExecutableStatements(initPolicy).sequence flatMap (_ => outer.createAsync())
  }

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

  def indexGroupCreation()(implicit ex: ExecutionContextExecutor) = {
    new ExecutableStatements(indexesForGroup).future()
  }

  def indexesForGroup: QueryCollection[Seq] = {
    val name = "farallones"
    new QueryCollection[Seq](

      Seq(
        ExecutableCqlQuery(QueryBuilder.Create.index(ParkingSpaceTable.tableName, name, "parkinggroupid")),
        ExecutableCqlQuery(QueryBuilder.Create.index(ParkingSpaceTable.tableName, name, "policyid")),
        ExecutableCqlQuery(QueryBuilder.Create.index(ParkingSpaceTable.tableName, name, "endtime"))
      )
    )
  }

  object ParkingSpaceTable extends ConcreteParkingSpace with connector.Connector

  object parkingGroupPolicyLinkTable extends ConcreteGroupPolicy with connector.Connector

  object policyMappingTable extends ConcretePolicy with connector.Connector

}


object parkingServicesDbTest extends parkingServicesDbTest(Defaults.connection) {
  implicit val ec = ExecutionContext.Implicits.global
}
