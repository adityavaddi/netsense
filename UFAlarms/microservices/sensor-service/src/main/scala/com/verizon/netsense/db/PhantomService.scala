package com.verizon.netsense.db

import com.outworkers.phantom.dsl.ResultSet
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.model._
import nl.grons.metrics.scala.Timer

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by maidapr on 4/7/17.
  */
trait PhantomService extends ProductionDatabase with Instrumented {

  lazy private val phantomConnector = ProductionDb.connector

  def storeEvent(eachEvent: SensorSampleEvent): Future[ResultSet] =
    database.deviceSensorSampleTable.store(eachEvent)

  def getLightStatus(nodeId: String): Future[Option[Light]] = {
    database.lightTable.getLatestLightModeByNodeId(nodeId)
  }

  def storeLightStatus(light: Light): Future[ResultSet] = {
    database.lightTable.storeLatestLightModeByNodeId(light)
  }

  def getNodeStatus(nodeId: String): Future[Option[NodeStatus]] = {
    database.nodeStatusTable.getNodeStatusbyNodeId(nodeId)
  }

  def storeNodeStatus(nodeStatus: NodeStatus): Future[ResultSet] = {
    database.nodeStatusTable.storeNodeStatus(nodeStatus)
  }

  def getSensorEvents(envelope: SensorQueryEnvelope)(implicit ec: ExecutionContext): Future[List[(Long, Double)]] =
    database.deviceSensorSampleTable.getHistoricalDataForSensor(envelope)

  def getSensorHistoryEvents(nodeId: String, sensorId: String, fromTime: Long, toTime: Long, limit: Int)(implicit ec: ExecutionContext): Future[List[(Long, Double)]] =
    database.deviceSensorSampleTable.getSensorHistoryData(nodeId,sensorId,fromTime,toTime,limit)

  def getAggregatedEnergySavingNode(
                                     envelope: SensorQueryEnvelope
                                   )(implicit ec: ExecutionContext): Future[List[(String, String, String, Double, Double, Double, Double, Double)]] =
    database.energySavingsNodeTable.getEnergySavingsNode(envelope)

  def getAggregatedEnergySavingSite(
                                     envelope: SensorQueryEnvelope
                                   )(implicit ec: ExecutionContext): Future[List[(String, String, Double, Double, Double, Double, Double)]] =
    database.energySavingSiteTable.getEnergySavingsSite(envelope)

}

object PhantomService extends PhantomService with ProductionDatabase
