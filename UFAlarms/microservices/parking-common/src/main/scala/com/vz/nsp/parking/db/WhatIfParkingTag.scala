package com.vz.nsp.parking.db

import com.outworkers.phantom.dsl.{ConsistencyLevel, _}
import com.outworkers.phantom.keys.PartitionKey
import com.vz.nsp.parking.config.CassandraConfig
import com.vz.nsp.parking.model.Tag

import scala.concurrent.Future

abstract class WhatIfParkingTag extends Table[WhatIfParkingTag, Tag] {

  override def tableName: String = CassandraConfig.whatIfTagTableName

  object tagid extends StringColumn with ClusteringOrder

  object name extends StringColumn

  object description extends StringColumn

  object orgid extends StringColumn with PartitionKey

  object siteid extends StringColumn with PartitionKey

  object createdon extends LongColumn

  object lastupdated extends LongColumn

  object isdeleted extends BooleanColumn

}

abstract class ConcreteWhatIfParkingTag extends WhatIfParkingTag with RootConnector {

  def store(tag: Tag): Future[ResultSet] =
    insert
      .value(_.tagid, tag.uid)
      .value(_.name, tag.name)
      .value(_.description, tag.description)
      .value(_.orgid, tag.orgid)
      .value(_.siteid, tag.siteid)
      .value(_.createdon, tag.createdon)
      .value(_.lastupdated, tag.lastupdated)
      .value(_.isdeleted, false)
      .consistencyLevel_=(ConsistencyLevel.LOCAL_QUORUM)
      .future()

  def getTagById(orgid: String, siteid: String, uid: String): Future[Option[Tag]] =
    select
      .where(_.tagid eqs uid)
      .and(_.orgid eqs orgid)
      .and(_.siteid eqs siteid)
      .consistencyLevel_=(ConsistencyLevel.LOCAL_QUORUM)
      .one()

  def getAllTags(orgid: String, siteid: String): Future[List[Tag]] =
    select
      .where(_.orgid eqs orgid)
      .and(_.siteid eqs siteid)
      .consistencyLevel_=(ConsistencyLevel.LOCAL_QUORUM)
      .fetch()

  def deleteByTagId(uid: String, orgid: String, siteId: String): Future[ResultSet] =
    update
      .where(_.tagid eqs uid)
      .and(_.orgid eqs orgid)
      .and(_.siteid eqs siteId)
      .modify(_.isdeleted setTo true)
      .ifExists
      .consistencyLevel_=(ConsistencyLevel.LOCAL_QUORUM)
      .future()

  def updateByTagId(uid: String, tag: Tag): Future[ResultSet] =
    update
      .where(_.tagid eqs uid)
      .and(_.orgid eqs tag.orgid)
      .and(_.siteid eqs tag.siteid)
      .modify(_.name setTo tag.name)
      .and(_.description setTo tag.description)
      .and(_.createdon setTo tag.createdon)
      .and(_.isdeleted setTo false)
      .and(_.lastupdated setTo tag.lastupdated)
      .ifExists
      .consistencyLevel_=(ConsistencyLevel.LOCAL_QUORUM)
      .future()

  def getLiveTagIds(requestedTags: List[String]): Future[List[Tag]] =
    select
      .where(_.tagid in requestedTags)
      .consistencyLevel_=(ConsistencyLevel.LOCAL_QUORUM)
      .allowFiltering()
      .fetch()
}
