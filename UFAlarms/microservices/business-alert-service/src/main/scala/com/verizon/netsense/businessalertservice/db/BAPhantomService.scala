package com.verizon.netsense.businessalertservice.db

import com.outworkers.phantom.dsl.ResultSet
import com.verizon.netsense.metrics.Instrumented
import nl.grons.metrics.scala.Timer
import com.verizon.netsense.businessalertservice.model.{BusinessAlert, BusinessAlertHistory, DBTrigger}

import scala.concurrent.Future

trait BAPhantomService extends ProductionDatabase with Instrumented {

  implicit private val phantomConnector              = ProductionDb.connector
  private[this] val cassandraPersist: Timer          = metrics.timer("cassandra-async-persist-timer")
  private[this] val cassandraDBTriggerGet: Timer     = metrics.timer("cassandra-trigger-read-timer")
  private[this] val cassandraDBTriggerGetById: Timer = metrics.timer("cassandra-trigger-by-id-read-timer")

  def getBusinessAlertById(orgid: String, siteid: String, businessAlertId: String): Future[Option[BusinessAlert]] =
    database.BusinessAlertTable.getBusinessAlertById(orgid, siteid, businessAlertId)

  def getAllBusinessAlerts(orgid: String, siteid: String): Future[List[BusinessAlert]] =
    database.BusinessAlertTable.getAllBusinessAlerts(orgid, siteid)

  def dismissByBusinessAlertId(orgid: String,
                               siteid: String,
                               businessAlertId: String,
                               severity: String,
                               userId: String,
                               lastClearedAt: Long): Future[ResultSet] = cassandraPersist.time {
    database.BusinessAlertTable.dismissByBusinessAlertId(orgid,
                                                         siteid,
                                                         businessAlertId,
                                                         severity,
                                                         userId,
                                                         lastClearedAt)
  }
  def storeBusinessAlertHistory(businessAlert: BusinessAlertHistory): Future[ResultSet] = cassandraPersist.time {
    database.BusinessAlertHistoryTable.storeBusinessAlertHistory(businessAlert)
  }

  def storeBusinessAlert(businessAlert: BusinessAlert): Future[ResultSet] = cassandraPersist.time {
    database.BusinessAlertTable.storeBusinessAlert(businessAlert)
  }

  def getAllBusinessTriggersInOrgSite(orgid: String, siteid: String): Future[List[DBTrigger]] =
    cassandraDBTriggerGet.time {
      database.BusinessTriggerTable.getAllTriggersInOrgSite(orgid, siteid)
    }

  def getBusinessTriggerByIdInOrgSite(orgid: String, siteid: String, triggerid: String): Future[Option[DBTrigger]] =
    cassandraDBTriggerGetById.time {
      database.BusinessTriggerTable.getTriggerById(orgid, siteid, triggerid)
    }

  def storeBusinessTrigger(dBTrigger: DBTrigger): Future[ResultSet] =
    database.BusinessTriggerTable.storeTrigger(dBTrigger)

}
