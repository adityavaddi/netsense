package com.verizon.netsense.model

import com.outworkers.phantom.CassandraTable
import com.outworkers.phantom.connectors.RootConnector
import com.outworkers.phantom.dsl._
import com.verizon.netsense.entity.{ActivityLogUser}

import scala.concurrent.Future

class ActivityLogUserModel extends CassandraTable[ConcreteActivityLogUserModel, ActivityLogUser] {

  override def tableName: String = "activity_logs_user"

  object userid extends StringColumn(this) with PartitionKey

  object when extends TimeUUIDColumn(this) with PartitionKey

  object activity extends StringColumn(this)

  object message extends StringColumn(this)

  object targetid extends StringColumn(this)

  object targettype extends StringColumn(this)

  override def fromRow(r: Row): ActivityLogUser =
    ActivityLogUser(userid(r), when(r), activity(r), message(r), targetid(r), targettype(r))
}

abstract class ConcreteActivityLogUserModel extends ActivityLogUserModel with RootConnector {


  def storeActivityLogUser(activityLog: ActivityLogUser): Future[ResultSet] =
    insert
      .value(_.userid, activityLog.userid)
      .value(_.when, activityLog.when)
      .value(_.activity, activityLog.activity)
      .value(_.message, activityLog.message)
      .value(_.targetid, activityLog.targetid)
      .value(_.targettype, activityLog.targettype)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()
}
