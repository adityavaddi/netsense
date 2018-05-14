package com.vz.ns.ts.service.db

import com.outworkers.phantom.dsl.ResultSet
import com.vz.ns.ts.service.model.Mapping

import scala.concurrent.Future

trait PhantomService extends ProductionDatabase {

  implicit private val phantomConnector = ProductionDb.connector

  def storeEvent(deviceMapping: Mapping): Future[ResultSet] =
    database.deviceMappingTable.store(deviceMapping)

  def getTSDeviceIdForNodeId(nodeid: String): Future[Option[Mapping]] =
    database.deviceMappingTable.getNodeIdByTsDeviceId(nodeid)

  def getAllMapping: Future[List[Mapping]] =
    database.deviceMappingTable.getAllMapping
}

object PhantomService extends PhantomService with ProductionDatabase
