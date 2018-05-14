package com.vz.ns.ts.service.util

import com.vz.ns.ts.service.db.PhantomService
import com.vz.ns.ts.service.model.Mapping
import com.vz.ns.ts.service.service.Logging

import scala.concurrent.Future

object CassandraUtil extends Logging {

  def searchCassandraForKeyNodeId(nodeId: String): Future[Option[Mapping]] = {
    log.info("Entering CassandraUtil::searchCassandraForKeyNodeId")
    log.debug(s"with NodeID: $nodeId")
    PhantomService.getTSDeviceIdForNodeId(nodeId)
  }

  def getAllMapping: Future[List[Mapping]] = {
    log.info("Entering CassandraUtil::getAllMapping")
    PhantomService.getAllMapping
  }
}
