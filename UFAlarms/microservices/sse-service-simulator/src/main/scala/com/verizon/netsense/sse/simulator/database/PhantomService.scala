package com.verizon.netsense.sse.simulator.database

import com.verizon.netsense.sse.simulator.config.ConfigLoader._
import com.verizon.netsense.sse.simulator.model.{OrgEvent, SSESubscribeRequest}

/**
 *
 * Created by subrsi9 on 11/24/17.
 */
trait PhantomService extends SSESimulatorDBProvider {

  val connection = sseCassandraConnector
  def database   = new SSESimulatorDatabase(connection)

  def insertSSEFilter(sSESubscribeRequest: SSESubscribeRequest, topicName: String, sseStreamTtl: Long, counter: Int) =
    database.sseFilterEvent.insertSSEFilter(sSESubscribeRequest, topicName, sseStreamTtl, counter)

  def insertOrgByNodeId(orgEvent: OrgEvent) =
    database.orgByNode.insertOrgEvent(orgEvent)

}
