package com.verizon.netsense.database

import com.outworkers.phantom.dsl.ResultSet
import com.verizon.netsense.model.Light

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by maidapr on 3/12/18.
  */
class DbLayer(phantomService: PhantomService) {

  def getLatestLightMode(nodeId: String)(implicit ec: ExecutionContext): Future[Option[Light]] = {
    phantomService.getLatestLightModeByNodeId(nodeId)
  }
  def storeLatestLightModeByNodeId(light: Light)(implicit ec: ExecutionContext): Future[ResultSet] = {
    phantomService.storeLatestLightModeByNodeId(light)
  }

}
object DbLayer {
  def apply(): DbLayer = new DbLayer(new PhantomService with ProductionDatabase{})
}
