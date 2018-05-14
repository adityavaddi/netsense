package com.verizon.netsense.services.data

import java.util.{Calendar, UUID}

import com.verizon.netsense.entity._

trait TestData {
  lazy val messageId: String = UUID.randomUUID().toString
  lazy val requestId: String = UUID.randomUUID().toString
  lazy val instanceId: String = UUID.randomUUID().toString
  lazy val orgId: String = UUID.randomUUID().toString
  lazy val siteId: String = "4aacfab0-4cc8-11e7-a01d-1bee4134e9b6"
  lazy val userId: String = UUID.randomUUID().toString
  lazy val nodeId: String = UUID.randomUUID().toString
  lazy val timestamp: String = Calendar.getInstance().toInstant.toString

  lazy val model = "NodeModel"
  lazy val action = "CAN_CREATE"
  lazy val `type` = "getDefaultConfigs"

  def generateIsRequestWithHeaders(_nodeId: String = nodeId, command: String = `type`, devicemodel: String = "falcon-q" ): IsRequestQueryEnvelope = {
    def responsetopic: String= UUID.randomUUID().toString

    val orgProps = OrgProps(orgId)
    val nodeProps = NodeProps(_nodeId, devicemodel)
    val siteProps = SiteProps(siteId)
    val requestQuery: IsQueryEnvelope = IsQueryEnvelope(requestId, command, model, action, instanceId,
      responsetopic, timestamp, userId, nodeProps, siteProps, orgProps)

    IsRequestQueryEnvelope(messageId, requestQuery)
  }
}
