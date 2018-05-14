package com.verizon.netsense.actors

import akka.actor.{ActorSystem, Props}
import com.verizon.netsense.database.DeviceConfigDatabaseService
import com.verizon.netsense.entity._
import com.verizon.netsense.services.{KafkaConsumerMessageHandler, KafkaConsumerService}
import com.verizon.netsense.utils.{ConfigLoader, Logging}
import org.json4s.jackson.JsonMethods.{compact, parse}
import org.json4s._
import org.json4s.JsonDSL._

import scala.concurrent.Future
import scala.util.matching.Regex
import java.util.Base64

/**
 * Created by davor on 5/15/17.
 */

class ConfigConsumerKafkaHandler(implicit val system: ActorSystem) extends KafkaConsumerMessageHandler with Logging {

  implicit val formats = DefaultFormats
  implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  private val replyConfigPattern: Regex = """.*(GET|PUT)\/REPLY\/([0-9a-zA-Z-]+)\/(config|vpn)\/(list|single|on-demand)""".r

  private val replyTo = ConfigLoader.config.getString("kafka.is-response-topic")

  override def messageReceive(message: String): Future[Unit] = {

    val parsedMessage = parse(message)
    val path      = (parsedMessage \ "p").extractOrElse("")
    val messageId = (parsedMessage \ "uuid").extractOrElse("")
    val error     = (parsedMessage \ "e").extractOrElse("")
    val status    = (parsedMessage \ "s").extractOrElse(-1)
    val nodeId    = (parsedMessage \ "sid").extract[String]
    log.debug(s"Message Path: $path")

    path match {
      case replyConfigPattern("GET", uuid, "config", "list") =>
        val token            = (parsedMessage \ "l" \ "t").extractOrElse("")
        val database         = (parsedMessage \ "l" \ "db").extractOrElse("")
        val b64EncodedConfig = (parsedMessage \ "l" \ "l").extract[String]
        val configBytes      = Base64.getDecoder.decode(b64EncodedConfig)
        val configStr        = new String(configBytes)

        system.eventStream.publish(
          ISDeviceConfigResponse(messageId, uuid, replyTo, DeviceConfig(database, token, configBytes))
        )
        Future.successful()

      case replyConfigPattern("GET", uuid, "config", "single") =>
        val token     = (parsedMessage \ "l" \ "t").extract[String]
        val database  = (parsedMessage \ "l" \ "db").extract[String]
        val key       = (parsedMessage \ "l" \ "k").extract[String]
        val byteValue = (parsedMessage \ "l" \ "v").asInstanceOf[Array[Byte]]
        val value     = new String(byteValue)
        val pair      = (key -> value) ~ Nil
        val pairJson  = compact(pair)
        system.eventStream.publish(
          ISDeviceConfigResponse(messageId, uuid, replyTo, DeviceConfig(database, token, pairJson.getBytes))
        )
        Future.successful()

      case replyConfigPattern("PUT", uuid, "config", _) =>
        val token     = (parsedMessage \ "l" \ "t").extract[String]
        val response = status match {
          case 200 =>
            ("message" -> "success") ~ ("status" -> status)
          case _ =>
            log.error(s"Node RESTPack error: $error")
            ("error" -> error) ~ ("status" -> status)
        }

        DeviceConfigDatabaseService.getConfigByNodeId(nodeId).flatMap {
          case Some(cfg) =>
            if (token != cfg.token) {
              DeviceConfigDatabaseService.updateConfigToken(cfg.nodeid, token).map {
                result =>
                  if (result.wasApplied()) {
                    log.debug(s"Successfully updated token for node $nodeId")
                  } else {
                    log.error(s"Failed to  update token for node $nodeId")
                  }
              }
            } else {
              log.debug(s"Config tokens match for node $nodeId")
              Future.successful()
            }
          case None =>
            log.error(s"Config not found for node: $nodeId")
            Future.failed(new Exception(s"Config not found for node: $nodeId"))
        }

        system.eventStream.publish(ISRespondWithMessage(messageId, uuid, replyTo, status, compact(response)))
        Future.successful()

      case replyConfigPattern("PUT", uuid, "vpn", "on-demand") =>
        val vpnIP = (parsedMessage \ "l" \ "IP").extract[String]
        val response = status match {
          case 200 =>
            ("message" -> "success") ~ ("status" -> status)
          case _ =>
            log.error(s"Node RESTPack error for vpn-on-demand: $error")
            ("error" -> error) ~ ("status" -> status)
        }

        system.eventStream.publish(VPNOnDemandResponse(messageId, uuid, replyTo, status, vpnIP))
        Future.successful()

      case _ =>
        log.info(s"Ignoring received message $message")
        Future.successful()
    }
  }
}
