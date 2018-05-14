package com.vz.nsp.parking.db

import com.outworkers.phantom.dsl.{ConsistencyLevel, _}
import com.outworkers.phantom.keys.PartitionKey
import com.vz.nsp.parking.config.CassandraConfig
import com.vz.nsp.parking.model.UserData

import scala.concurrent.Future

abstract class UserDataTable extends Table[ConcreteUserData, UserData] {

  override def tableName: String = CassandraConfig.parkingUserDataTableName

  object userdataid extends StringColumn with PartitionKey

  object userid extends StringColumn with Index

  object appid extends StringColumn with PrimaryKey

  object dataValue extends StringColumn

  object createdon extends LongColumn

  object lastupdated extends LongColumn

  object isdeleted extends BooleanColumn with Index

}

abstract class ConcreteUserData extends UserDataTable with RootConnector {

  def store(userData: UserData): Future[ResultSet] =
    insert
      .value(_.appid, userData.appid)
      .value(_.userid, userData.userid)
      .value(_.userdataid, userData.userdataid)
      .value(_.dataValue, userData.datavalue)
      .value(_.createdon, userData.createdon)
      .value(_.lastupdated, userData.lastupdated)
      .value(_.isdeleted, false)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()

  def getUserDataById(appid: String, userid: String, userdataid: String): Future[Option[UserData]] =
    select
      .where(_.userdataid eqs userdataid)
      .and(_.isdeleted eqs false)
      .and(_.appid eqs appid)
      .and(_.userid eqs userid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .allowFiltering().one()

  def getAllUserData(appid: String, userid: String): Future[List[UserData]] =
    select
      .where(_.appid eqs appid)
      .and(_.isdeleted eqs false)
      .and(_.userid eqs userid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .allowFiltering()
      .fetch()

  def deleteByUserDataId(appid: String, userid: String, userdataid: String): Future[ResultSet] =
    update
      .where(_.userdataid eqs userdataid)
      .and(_.appid eqs appid)
      .modify(_.isdeleted setTo true)
      .ifExists
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()

  def updateByUserDataId(appid: String, userid: String, userdataid: String, userData: UserData): Future[ResultSet] =
    update
      .where(_.userdataid eqs userData.userdataid)
      .and(_.appid eqs userData.appid)
      .modify(_.userid setTo userData.userid)
      .and(_.createdon setTo userData.createdon)
      .and(_.isdeleted setTo false)
      .and(_.dataValue setTo userData.datavalue)
      .and(_.lastupdated setTo userData.lastupdated)
      .ifExists
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()

}
