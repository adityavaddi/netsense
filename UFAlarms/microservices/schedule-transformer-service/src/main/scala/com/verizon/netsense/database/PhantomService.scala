package com.verizon.netsense.database

import com.outworkers.phantom.dsl.ResultSet
import com.verizon.netsense.model.Light

import scala.concurrent.Future

/**
  * Created by maidapr on 1/10/18.
  */
trait PhantomService extends MyDbProvider {

  implicit val phantomConnector = ProductionDb.connector

  def getLatestLightModeByNodeId(nodeId: String): Future[Option[Light]] = {
    database.lightTable.getLatestLightModeByNodeId(nodeId)
  }

  def storeLatestLightModeByNodeId(light: Light): Future[ResultSet] = {
    database.lightTable.storeLatestLightModeByNodeId(light)
  }

}


