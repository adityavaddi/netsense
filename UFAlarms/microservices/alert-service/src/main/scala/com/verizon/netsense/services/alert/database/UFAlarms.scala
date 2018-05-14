package com.verizon.netsense.services.alert.database

import com.outworkers.phantom.dsl._
import com.outworkers.phantom.connectors.RootConnector
import com.verizon.netsense.services.alert.database.CassandraDB.{
  ufAlarms,
  READ_CONSISTENCY_LEVEL,
  WRITE_CONSISTENCY_LEVEL
}
import com.verizon.netsense.services.alert.model.UfAlarm

import scala.concurrent.Future

abstract class UFAlarms extends Table[UFAlarms, UfAlarm] with RootConnector {
  def getAllUfAlarms: Future[List[UfAlarm]] =
    select
      .consistencyLevel_=(READ_CONSISTENCY_LEVEL)
      .fetch()

  def getUfAlarmByType(alarmType: String): Future[Option[UfAlarm]] =
    select
      .where(_.alarmtype eqs alarmType)
      .consistencyLevel_=(READ_CONSISTENCY_LEVEL)
      .one()

  def deleteUfAlarmByType(alarmType: String): Future[ResultSet] =
    delete
      .where(_.alarmtype eqs alarmType)
      .consistencyLevel_=(WRITE_CONSISTENCY_LEVEL)
      .future()

  def updateUfAlarmByType(ufAlarm: UfAlarm): Future[ResultSet] =
    update
      .where(_.alarmtype eqs ufAlarm.alarmType)
      .modify(_.ufname setTo ufAlarm.ufName)
      .and(_.nodemodels setTo ufAlarm.nodeModels)
      .and(_.description setTo ufAlarm.description)
      .and(_.displaytocustomer setTo ufAlarm.displayToCustomer)
      .and(_.displaytopartner setTo ufAlarm.displayToPartner)
      .and(_.updated setTo ufAlarm.updated)
      .ifExists
      .consistencyLevel_=(WRITE_CONSISTENCY_LEVEL)
      .future()

  def createUfAlarmByType(ufAlarm: UfAlarm): Future[ResultSet] =
    insert
      .value(_.mappingid, ufAlarm.mappingId)
      .value(_.alarmtype, ufAlarm.alarmType)
      .value(_.ufname, ufAlarm.ufName)
      .value(_.nodemodels, ufAlarm.nodeModels)
      .value(_.description, ufAlarm.description)
      .value(_.displaytocustomer, ufAlarm.displayToCustomer)
      .value(_.displaytopartner, ufAlarm.displayToPartner)
      .value(_.created, ufAlarm.created)
      .value(_.updated, ufAlarm.updated)
      .consistencyLevel_=(WRITE_CONSISTENCY_LEVEL)
      .future()

  override def tableName: String = ufAlarms

  object mappingid extends StringColumn

  object alarmtype extends StringColumn with PartitionKey

  object nodemodels extends SetColumn[String]

  object ufname extends OptionalStringColumn

  object description extends OptionalStringColumn

  object displaytocustomer extends BooleanColumn

  object displaytopartner extends BooleanColumn

  object created extends OptionalLongColumn

  object updated extends OptionalLongColumn

}
