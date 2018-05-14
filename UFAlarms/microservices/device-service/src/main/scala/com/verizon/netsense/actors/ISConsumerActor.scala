package com.verizon.netsense.actors

import akka.actor.ActorSystem
import akka.event.Logging
import com.verizon.netsense.connector.KafkaConnection
import com.verizon.netsense.database.{DeviceConnectionStatusDbService, NodeStatusDbService, NodeStatusDbServiceBase}
import com.verizon.netsense.entity.ISResponse
import com.verizon.netsense.services.{KafkaConsumerMessageHandler, KafkaConsumerService}
import com.verizon.netsense.utils.{ConfigLoader, Logging}
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods.{compact, parse, render}

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * Created by davor on 7/6/17.
 */
class ISConsumerHandler(val dbService: NodeStatusDbServiceBase)(implicit val system: ActorSystem) extends KafkaConsumerMessageHandler with Logging {

  //val log = Logging(system.eventStream, "config.service.is.consumer")
  implicit val formats = DefaultFormats
  implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  override def messageReceive(message: String): Future[Unit] = {
    log.debug(s"Precess data start")
    val parsedMessage = parse(message)

    val messageId   = (parsedMessage \ "messageid").extractOrElse("")
    val replyTo     = (parsedMessage \ "responsetopic").extractOrElse("")
    val messageType = (parsedMessage \ "request" \ "type").extract[String]
    val model       = (parsedMessage \ "request" \ "model").extractOrElse("")

    val requestId = (parsedMessage \ "request" \ "requestid").extractOrElse("")

    messageType match {
      case "getNodeStatus" =>
        val model  = (parsedMessage \ "request" \ "nodeprops" \ "model").extractOrElse("")
        val nodeId = (parsedMessage \ "request" \ "nodeprops" \ "nodeid") extractOrElse ("")

        val result = dbService.getNodeStatusByNodeId(nodeId)
        result.onComplete {
          case Success(res) =>
            res match {
              case Some(status) =>
                system.eventStream.publish(ISResponse(messageId, requestId, replyTo, model, status.toJSON))
              case None => log.debug(s"Result not found")
            }
          case Failure(_) => log.debug(s"Failed to retrieve result")
        }
    }
    Future.successful()
  }
}
