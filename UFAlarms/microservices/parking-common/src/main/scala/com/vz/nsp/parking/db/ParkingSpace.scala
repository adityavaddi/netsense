package com.vz.nsp.parking.db

import com.outworkers.phantom.dsl._
import com.outworkers.phantom.keys.PartitionKey
import com.vz.nsp.parking.config.CassandraConfig
import com.vz.nsp.parking.model._

import scala.concurrent.Future

abstract class ParkingSpaceTable extends Table[ParkingSpaceTable, ParkingSpace] {
  override def tableName: String = CassandraConfig.parkingSpaceTableName

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

  object geocoordinates extends OptionalCol[SpaceGeometry]

  object createdon extends LongColumn

  object lastupdated extends LongColumn

  object isdeleted extends BooleanColumn

}

abstract class ConcreteParkingSpace extends ParkingSpaceTable with RootConnector {

  lazy val insertParkingSpaceQuery=insert
    .p_value(_.parkingspaceid, ?)
    .p_value(_.name, ?)
    .p_value(_.typeofvehicle, ?)
    .p_value(_.reservation, ?)
    .p_value(_.handicap, ?)
    .p_value(_.monitoringsensorid, ?)
    .p_value(_.businessuse, ?)
    .p_value(_.howmetered, ?)
    .p_value(_.active, ?)
    .p_value(_.ppv, ?)
    .p_value(_.areaoftype, ?)
    .p_value(_.meterid, ?)
    .p_value(_.paystationid, ?)
    .p_value(_.level, ?)
    .p_value(_.parkingspacetype, ?)
    .p_value(_.geocoordinates, ?)
    .p_value(_.createdon, ?)
    .p_value(_.lastupdated, ?)
    .p_value(_.isdeleted, ?)
    .consistencyLevel_=(ConsistencyLevel.ONE)
    .prepareAsync()

  def storeParkingSpace(parkingSpace: ParkingSpace, createdOnFromDb:Long): Future[ResultSet] =

    insertParkingSpaceQuery.flatMap(_.bind(parkingSpace.parkingspaceid,parkingSpace.name,parkingSpace.typeOfVehicle,parkingSpace.reservation,parkingSpace.handicap,parkingSpace.monitoringSensorid,parkingSpace.businessUse,
      parkingSpace.howMetered,parkingSpace.active,parkingSpace.PPV,parkingSpace.areaType,parkingSpace.meterid,parkingSpace.paystationid,parkingSpace.level,parkingSpace.parkingSpaceType,parkingSpace.geoCoordinates,createdOnFromDb,parkingSpace.lastUpdated,parkingSpace.isDeleted
      ).future())


  def deleteByParkingSpaceId(parkingspaceid: List[String]): Future[ResultSet] = {
    update
      .where(_.parkingspaceid in parkingspaceid)
      .modify(_.isdeleted setTo true)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()
  }

  def getParkingSpacesByIds(parkingspaceids: List[String]): Future[List[ParkingSpace]] =
    select
      .where(_.parkingspaceid in parkingspaceids)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .fetch()

  def getParkingSpaceById(parkingspaceid: String): Future[Option[Long]] =
    select(_.createdon)
      .where(_.parkingspaceid eqs  parkingspaceid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .one()
}
