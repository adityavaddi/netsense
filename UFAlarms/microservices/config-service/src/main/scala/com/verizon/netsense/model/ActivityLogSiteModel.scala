package com.verizon.netsense.model

import com.outworkers.phantom.CassandraTable
import com.outworkers.phantom.connectors.RootConnector
import com.outworkers.phantom.dsl._
import com.verizon.netsense.entity.{ActivityLogSite}

import scala.concurrent.Future

class ActivityLogSiteModel extends CassandraTable[ConcreteActivityLogSiteModel, ActivityLogSite] {

  override def tableName: String = "activity_logs_site"

  object siteid extends StringColumn(this) with PartitionKey

  object when extends TimeUUIDColumn(this) with PartitionKey

  object activity extends StringColumn(this)

  object message extends StringColumn(this)

  object targetid extends StringColumn(this)

  object targettype extends StringColumn(this)

  object userid extends StringColumn(this)

  override def fromRow(r: Row): ActivityLogSite =
    ActivityLogSite(siteid(r), when(r), activity(r), message(r), targetid(r), targettype(r), userid(r))
}

abstract class ConcreteActivityLogSiteModel extends ActivityLogSiteModel with RootConnector {


  def storeActivityLogSite(activityLog: ActivityLogSite): Future[ResultSet] =
    insert
      .value(_.siteid, activityLog.siteid)
      .value(_.when, activityLog.when)
      .value(_.activity, activityLog.activity)
      .value(_.message, activityLog.message)
      .value(_.targetid, activityLog.targetid)
      .value(_.targettype, activityLog.targettype)
      .value(_.userid, activityLog.userid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()
}
