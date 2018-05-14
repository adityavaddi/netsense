package com.verizon.netsense.services.data

import java.util.{Calendar, UUID}

import akka.http.scaladsl.model.DateTime
import akka.io.Dns.Command
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

  def generateIsConfigRequestWithHeaders(_nodeId: String = nodeId,
                                         command: String = `type`,
                                         devicemodel: String = "falcon-q"
                                        ): IsRequestQueryConfigEnvelope = {
    def responsetopic: String= UUID.randomUUID().toString

    val orgProps = OrgProps(orgId)
    val nodeProps = NodeProps(_nodeId, devicemodel)
    val siteProps = SiteProps(siteId)
    val configProps = ConfigProps(Map("debugmode" -> true))
    val requestQuery: IsQueryConfigEnvelope = IsQueryConfigEnvelope(requestId, command, model, action, instanceId,
      responsetopic, timestamp, userId, nodeProps, siteProps, orgProps, configProps)

    IsRequestQueryConfigEnvelope(messageId, requestQuery)
  }

  def generateIsResponseConfigWithHeaders(_nodeId: String = nodeId): ISMessageConfig = {
    def responsetopic: String= UUID.randomUUID().toString

    val responsePayload = ISResponseConfig(requestId, true, "", Map(), List())

    ISMessageConfig(messageId, responsePayload)
  }

  def generateIsResponseErrorWithHeaders(_nodeId: String = nodeId): ISMessageError = {
    def responsetopic: String= UUID.randomUUID().toString

    val responsePayload = ISResponseError(requestId, true, "", "Error Message", 200)

    ISMessageError(messageId, responsePayload)
  }

  def generateIsResponseSuccessWithHeaders(_nodeId: String = nodeId): ISMessageSuccess = {
    def responsetopic: String= UUID.randomUUID().toString

    val responsePayload = ISResponseSuccess(requestId, true, "", "Error Message")

    ISMessageSuccess(messageId, responsePayload)
  }

  def generateIsResponseTokenWithHeaders(_nodeId: String = nodeId): ISMessageToken = {
    def responsetopic: String= UUID.randomUUID().toString

    val responsePayload = ISResponseToken(requestId, true, "", "Token Message", "123456")

    ISMessageToken(messageId, responsePayload)
  }
}
