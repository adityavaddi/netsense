package com.vz.nsp.trigger.db


import com.outworkers.phantom.dsl.ResultSet
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import com.vz.nsp.trigger.model.db.DBTrigger
import nl.grons.metrics.scala.Timer

import scala.concurrent.Future

trait PhantomService extends ProductionDatabase with Instrumented with Logging{

  implicit private val phantomConnector = ProductionDb.connector
  private[this] val cassandraPersist: Timer      = metrics.timer("cassandra-async-persist-timer")

  def storeTrigger(trigger: DBTrigger): Future[ResultSet] = cassandraPersist.time {
    database.TriggerTable.store(trigger)
  }

  def getTrigger(orgid: String, siteid: String,triggerid: String): Future[Option[DBTrigger]] =
    database.TriggerTable.getTriggerById(triggerid,siteid,orgid)

  def getAllTrigger(triggerid: String, siteid: String): Future[List[DBTrigger]] = database.TriggerTable.getAllTriggerId(triggerid, siteid)

  def deleteByTriggerId(triggerid: String, siteid:String,orgid:String): Future[ResultSet] = cassandraPersist.time {
    database.TriggerTable.deleteByTriggerId(triggerid,siteid,orgid)
  }

  def updateByTriggerId(triggerid: String, siteid:String,orgid:String, trigger: DBTrigger): Future[ResultSet] = cassandraPersist.time {
    database.TriggerTable.updateByTriggerId(triggerid,siteid,orgid,trigger)
  }
}
