package com.verizon.netsense.database

import com.outworkers.phantom.dsl.ResultSet
import com.verizon.netsense.entity.{ActivityLogSite}
import com.verizon.netsense.metrics.Instrumented
import nl.grons.metrics.scala.Timer

import scala.concurrent.{Future}

trait ActLogSiteDatabaseBase {
  def storeActivity(activity: ActivityLogSite): Future[ResultSet]
}

trait ActLogSiteDatabaseService extends ActLogSiteDatabaseBase with ActivityLogSiteProductionDatabase with Instrumented {

  private[this] val cassandraTimer: Timer = metrics.timer("cassandra-activity-log-site-timer")

  override def storeActivity(activity: ActivityLogSite): Future[ResultSet] =
    cassandraTimer.time {
      database.activityLogSiteModel.storeActivityLogSite(activity)
    }
}

object ActLogSiteDatabaseService extends ActLogSiteDatabaseService with ActivityLogSiteProductionDatabase
class ActLogSiteDatabaseServiceImpl extends ActLogSiteDatabaseService with ActivityLogSiteProductionDatabase
