package com.verizon.netsense.sse.db

import com.verizon.netsense.sse.model.{DeviceEvent, Result, SSEEvent}
import com.verizon.netsense.sse.db.service.PhantomService
import com.verizon.netsense.sse.exceptions.CustomExceptions.{CassandraDBException, OrgDataNotFoundException}
import com.verizon.netsense.utils.Logging

import scala.concurrent.{ExecutionContext, Future}

/**
 * Created by muppasw on 7/17/17.
 */
trait CassandraQuery extends PhantomService with Logging {

  /*
   * Enrich the message with orgid and siteid based on nodeid
   */

  def enrichEvent(sseEvent: SSEEvent)(implicit ec: ExecutionContext): Future[SSEEvent] =
    enrichDevice(sseEvent.asInstanceOf[DeviceEvent].nodeid)
      .recover {
        case ex: OrgDataNotFoundException =>
          log.error(
            "Unable to get Org and Site details from orghierarchy_by_nodeid table for node while enrich: " + sseEvent
              .asInstanceOf[DeviceEvent]
              .nodeid,
            ex
          )
          throw new CassandraDBException(ex.getMessage);
      }
      .map {
        case Some(org) =>
          if (org._2 == "Unknown" || org._3 == "Unknown" || org._2.isEmpty || org._3.isEmpty) {
            val msg =
              s"Invalid Org Id or Site Id for the node:${sseEvent.asInstanceOf[DeviceEvent].nodeid}. OrgId: ${org._2}, SiteId: ${org._3}"
            throw new OrgDataNotFoundException(msg)
          }
          log.debug(s"Data from look up table : $org")
          val sse = sseEvent.asInstanceOf[DeviceEvent]
          sse.nodeid = org._1
          sse.orgid = org._2
          sse.siteid = org._3
          log.debug("EnrichedEvent: " + sse)

          sse
        case None =>
          log.warn("NodeID not found in Orghierarchy table :" + sseEvent.asInstanceOf[DeviceEvent].nodeid)
          sseEvent
      }

  /*
   * Filter the message based on wildcard(+) to nodeid and eventType
   */

  def filterEvent(event: SSEEvent)(implicit ec: ExecutionContext): Future[List[Option[Result]]] =
    filterDevice(event)
      .recover {
        case ex: OrgDataNotFoundException =>
          log.error(
            "Unable to get Org and Site details from orghierarchy_by_nodeid table for node while filter: " + event
              .asInstanceOf[DeviceEvent]
              .nodeid,
            ex
          )
          throw new CassandraDBException(ex.getMessage);
      }
      .map(x => {
        x.map(r => {
          val nodeid    = r._1
          val eventType = r._2
          val topicName = r._3
          log.debug("Event-Node-Type-Topic-Name:" + event + ":" + nodeid + ":" + eventType + ":" + topicName)
          processRecord(event, nodeid, eventType, topicName)
        })
      })

  def processRecord(event: SSEEvent, nodeid: String, eventType: String, topicName: String): Option[Result] =
    (nodeid, eventType) match {
      case _ if nodeid == "+" && eventType == "+" => Some(Result(topicName, event))
      case _
          if event.asInstanceOf[DeviceEvent].nodeid == nodeid && event.asInstanceOf[DeviceEvent].name == eventType =>
        Some(Result(topicName, event))
      case _ if nodeid == "+" && event.asInstanceOf[DeviceEvent].name == eventType   => Some(Result(topicName, event))
      case _ if event.asInstanceOf[DeviceEvent].nodeid == nodeid && eventType == "+" => Some(Result(topicName, event))
      case _                                                                         => None
    }
}
