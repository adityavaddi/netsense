package com.vz.nsp.parking.tagservice.db

import com.outworkers.phantom.dsl._
import com.outworkers.phantom.keys.PartitionKey
import com.vz.nsp.parking.model._

import scala.concurrent.Future

abstract class ConcreteTag extends Table[ConcreteTag, Tag] {

  override def tableName: String = "parkingtag"

  object uid extends StringColumn with PartitionKey

  object name extends StringColumn

  object description extends StringColumn

  object orgid extends StringColumn with Index

  object siteid extends StringColumn with Index

  object createdon extends LongColumn

  object lastupdated extends LongColumn

  object isdeleted extends BooleanColumn with Index

  def truncateTable(): Future[ResultSet] =
    truncate().future()

  def createTable() = create.ifNotExists().future()

}
