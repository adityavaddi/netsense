package com.verizon.netsense.sse.simulator.service

import com.verizon.netsense.sse.simulator.database.PhantomService
import com.verizon.netsense.sse.simulator.model._
import com.verizon.netsense.sse.simulator.config.ConfigLoader._
import com.verizon.netsense.utils.Logging

/**
 * Created by subrsi9 on 11/24/17.
 */
trait SSESimulateDB extends PhantomService with Logging {

  def initDB() = Unit
  if (isInitDB) {
    log.info(s"DB init:$isInitDB ")
    log.info(s"Populating $sseLookupTable")
    populateOrgByNode
    log.info(s"Inserting to $sseLookupTable completed !!!")

    log.info(s"Populating $sseStreamSubscribeTtlTable")
    populateSSEFilter
    log.info(s"Inserting to $sseStreamSubscribeTtlTable completed !!!")
    log.info(s"Inserting to $sseStreamSubscribeTtlTable completed !!!")
    log.info(s"Populating $sseStreambusinessAlertSubscribeTtlTable")
    populateBusinessAlertSSEFilter
    log.info(s"Inserting to $sseStreambusinessAlertSubscribeTtlTable completed !!!")
  } else {

    log.warn("DB init config set to false, make sure SSE tables are pre-populated !!!")
  }

  private def populateOrgByNode() =
    for (i <- 1 to numerOfNode) {
      database.orgByNode.insertOrgEvent(
        OrgEvent(orgId, siteId, nodeIdPrefix + i)
      )

    }

  private def populateSSEFilter() = {

    val topicName = s"$streamingMQTTTopicPrefix$orgId/$siteId/+/+"

    val subscribeReq = SSESubscribeRequest(
      "",
      "",
      SSERequest("", "", "", "", "", "", OrgProps(orgId), SiteProps(siteId), NodeProps("+"), ExtProps("+"))
    )

    database.sseFilterEvent.insertSSEFilter(subscribeReq, topicName, sseStreamTtl, 1)
  }

  private def populateBusinessAlertSSEFilter() = {

    //val businessAlertTopicName = s"$streamingMQTTTopicPrefix$orgId/$siteId/businessalert/$application/$triggerId"
    val businessAlertTopicName = s"$streamingMQTTTopicPrefix$orgId/$siteId/businessalert/+/+"
    val subscribeBusinessAlertReq = SSESubscribeBusinessAlertRequest(
      "",
      "",
      SSEBusinessRequest("", "", "", "", "", "", OrgProps(orgId), SiteProps(siteId), BusinessAlertProps("+", "+"))
    )
    database.sseBusinessFilterEvent.insertBussinessAlertSSEFilter(subscribeBusinessAlertReq,
                                                                  businessAlertTopicName,
                                                                  sseStreamTtl,
                                                                  1)
  }
}
