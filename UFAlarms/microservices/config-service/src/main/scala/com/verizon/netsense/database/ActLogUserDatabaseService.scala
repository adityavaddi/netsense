package com.verizon.netsense.database

import com.outworkers.phantom.dsl.ResultSet
import com.verizon.netsense.entity.{ActivityLogUser, Config}
import com.verizon.netsense.metrics.Instrumented
import nl.grons.metrics.scala.Timer

import scala.concurrent.{ExecutionContextExecutor, Future}

  trait ActLogUserDatabaseBase {
    def storeActivity(activity: ActivityLogUser): Future[ResultSet]
  }

  trait ActLogUserDatabaseService extends ActLogUserDatabaseBase with ActivityLogUserProductionDatabase with Instrumented {

    private[this] val cassandraTimer: Timer = metrics.timer("cassandra-activity-log-user-timer")

    override def storeActivity(activity: ActivityLogUser): Future[ResultSet] =
      cassandraTimer.time {
        database.activityLogUserModel.storeActivityLogUser(activity)
      }
  }

  object ActLogUserDatabaseService extends ActLogUserDatabaseService with ActivityLogUserProductionDatabase
  class ActLogUserDatabaseServiceImpl extends ActLogUserDatabaseService with ActivityLogUserProductionDatabase
