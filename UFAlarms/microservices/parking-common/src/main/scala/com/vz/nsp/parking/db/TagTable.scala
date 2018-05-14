package com.vz.nsp.parking.db

import com.outworkers.phantom.dsl.{ConsistencyLevel, _}
import com.outworkers.phantom.keys.PartitionKey
import com.vz.nsp.parking.config.CassandraConfig
import com.vz.nsp.parking.model.Tag

import scala.concurrent.Future

abstract class TagTable extends Table[TagTable, Tag] {

  override def tableName: String = CassandraConfig.parkingTagTableName

  object uid extends StringColumn with PartitionKey

  object name extends StringColumn

  object description extends StringColumn

  object orgid extends StringColumn

  object siteid extends StringColumn

  object createdon extends LongColumn

  object lastupdated extends LongColumn

  object isdeleted extends BooleanColumn

}

abstract class ConcreteTag extends TagTable with RootConnector {

  def store(tag: Tag): Future[ResultSet] =
    insert
      .value(_.uid, tag.uid)
      .value(_.name, tag.name)
      .value(_.description, tag.description)
      .value(_.orgid, tag.orgid)
      .value(_.siteid, tag.siteid)
      .value(_.createdon, tag.createdon)
      .value(_.lastupdated, tag.lastupdated)
      .value(_.isdeleted, false)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()

  def getTagById(orgid: String, siteid: String, uid: String): Future[Option[Tag]] =
    select
      .where(_.uid eqs uid)
      .and(_.orgid is orgid)
      .and(_.siteid is siteid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .allowFiltering()
      .one()

  def getAllTag(orgid: String, siteid: String): Future[List[Tag]] =
    select
      .where(_.isdeleted is false)
      .and(_.orgid is orgid)
      .and(_.siteid is siteid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .allowFiltering()
      .fetch()

  def deleteByTagId(uid: String): Future[ResultSet] =
    update
      .where(_.uid eqs uid)
      .modify(_.isdeleted setTo true)
      .ifExists
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()

  def updateByTagId(uid: String, tag: Tag): Future[ResultSet] =
    update
      .where(_.uid eqs uid)
      .modify(_.name setTo tag.name)
      .and(_.description setTo tag.description)
      .and(_.createdon setTo tag.createdon)
      .and(_.isdeleted setTo false)
      .and(_.lastupdated setTo tag.lastupdated)
      .ifExists
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()

  def getLiveTagIds(requestedTags: List[String]): Future[List[Tag]] =
    select
      .where(_.uid in requestedTags)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .allowFiltering()
      .fetch()
}
