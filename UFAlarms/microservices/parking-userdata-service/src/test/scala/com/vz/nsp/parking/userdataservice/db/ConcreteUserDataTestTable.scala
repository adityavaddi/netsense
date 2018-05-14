package com.vz.nsp.parking.userdataservice.db

import com.outworkers.phantom.dsl._
import com.outworkers.phantom.keys.PartitionKey

import com.vz.nsp.parking.model.UserData

import scala.concurrent.Future

abstract class ConcreteUserDataTestTable extends Table[ConcreteUserDataTestTable, UserData] {

  override def tableName: String = "appuserdata"

  object userdataid extends StringColumn with PartitionKey

  object userid extends StringColumn with Index

  object appid extends StringColumn with PrimaryKey

  object dataValue extends StringColumn

  object createdon extends LongColumn

  object lastupdated extends LongColumn

  object isdeleted extends BooleanColumn with Index

  def truncateTable(): Future[ResultSet] =
    truncate().future()

  def createTable() = create.ifNotExists().future()

}


