package com.vz.nsp.parking.grouppolicyservice.db

import com.outworkers.phantom.dsl._
import com.outworkers.phantom.keys.PartitionKey
import com.vz.nsp.parking.model._

import scala.concurrent.Future

abstract class ConcreteGroupPolicy extends Table[ConcreteGroupPolicy, ParkingGroupPolicyLink] {

  def createGroupTable() = create.ifNotExists().future()

  def truncateTable(): Future[ResultSet] = truncate().future()

  override def tableName: String = "policygrouptimeline"

  object uid extends StringColumn with PartitionKey

  object parkinggroupid extends StringColumn with Index

  object policyid extends StringColumn with Index

  object parkinggroupname extends StringColumn

  object version extends IntColumn

  object starttime extends LongColumn

  object endtime extends LongColumn with Index

}
