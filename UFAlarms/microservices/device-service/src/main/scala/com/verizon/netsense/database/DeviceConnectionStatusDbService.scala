package com.verizon.netsense.database

import com.outworkers.phantom.dsl.{ResultSet, UUID}
import com.verizon.netsense.entity.DeviceConnectionStatus
import com.verizon.netsense.metrics.Instrumented
import nl.grons.metrics.scala.Timer

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.concurrent.duration._

/**
 * Created by davor on 7/6/17.
 */
trait DeviceConnectionStatusDbService extends DeviceConnectionStatusProductionDatabase with Instrumented {

  private[this] val cassandraGetConnectionStatusTimer: Timer = metrics.timer("cassandra-get-connection-status-timer")
  private[this] val cassandraStoreConnectionStatusTimer: Timer = metrics.timer("cassandra-store-connection-status-timer")

  def start(): Unit = {
    val ec = scala.concurrent.ExecutionContext.Implicits.global

    database.create(3.seconds)(ec.asInstanceOf[ExecutionContextExecutor])
  }

  def getConnectionStatusByNodeId(nodeid: String): Future[Option[DeviceConnectionStatus]] =
    cassandraGetConnectionStatusTimer.time {
      database.deviceConnectionStatusModel.getConnectionStatusByNodeid(nodeid)
    }

  def storeConnectionStatus(nodeid: String, isConnected: Boolean, since: UUID): Future[ResultSet] =
    cassandraStoreConnectionStatusTimer.time {
      database.deviceConnectionStatusModel.storeConnectionStatus(nodeid, isConnected, since)
    }

}

object DeviceConnectionStatusDbService
    extends DeviceConnectionStatusDbService
    with DeviceConnectionStatusProductionDatabase
