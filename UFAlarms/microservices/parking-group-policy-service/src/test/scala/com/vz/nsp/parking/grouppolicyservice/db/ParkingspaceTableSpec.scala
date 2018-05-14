package com.vz.nsp.parking.grouppolicyservice.db

import com.outworkers.phantom.dsl._
import com.outworkers.phantom.keys.PartitionKey
import com.vz.nsp.parking.model._

abstract class ConcreteParkingSpace extends Table[ConcreteParkingSpace, ParkingSpace] with RootConnector {

  def customTypesSpace() = parkingServicesDbTest.create

  def createSpaceTable() = create.ifNotExists().future()

  def truncateSpaceTable() = truncate().future()

  override def tableName: String = "space_attributes"

  object parkingspaceid extends StringColumn with PartitionKey

  object name extends StringColumn

  object typeofvehicle extends ListColumn[String]

  object reservation extends BooleanColumn

  object handicap extends BooleanColumn

  object monitoringsensorid extends StringColumn

  object businessuse extends StringColumn

  object howmetered extends StringColumn

  object active extends BooleanColumn

  object ppv extends BooleanColumn

  object areaoftype extends ListColumn[String]

  object meterid extends StringColumn

  object paystationid extends StringColumn

  object level extends StringColumn

  object parkingspacetype extends StringColumn

  object geocoordinates extends CustomColumn[SpaceGeometry]

  object createdon extends LongColumn

  object lastupdated extends LongColumn

  object isdeleted extends BooleanColumn

}
