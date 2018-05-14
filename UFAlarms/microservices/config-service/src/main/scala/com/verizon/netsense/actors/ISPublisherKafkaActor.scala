package com.verizon.netsense.actors

import akka.actor.{Actor, ActorSystem, Props}
import com.verizon.netsense.connector.KafkaConnection
import com.verizon.netsense.entity._
import com.verizon.netsense.metrics.{Instrumented, MetricsActor}
import com.verizon.netsense.services.KafkaPublisherService
import com.verizon.netsense.utils.Logging
import org.joda.time.DateTime
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.JsonDSL._

/**
 * Created by davor on 5/16/2017.
 */
object ISPublisherKafkaActor {
  def props(connection: KafkaConnection)(implicit system: ActorSystem): Props =
    Props(Predef.classOf[ISPublisherKafkaActor], connection, system)
}

class ISPublisherKafkaActor(connection: KafkaConnection)(implicit override val system: ActorSystem)
    extends KafkaPublisherService(connection)
    with MetricsActor
    with Logging {

  override def receive: Receive = ReceiveMetrics {
    case ISDeviceConfigResponse(messageId, requestId, replyTo, config) =>
      log.debug(s"Received reponse to req: $messageId")
      val configListWrite = new String(config.l)
      val msgBody = ("requestid" -> requestId) ~ ("success" -> true) ~ ("timestamp" -> (new DateTime())
        .toString()) /*~("model" -> model)*/ ~ ("config" -> parse(configListWrite))

      val response = ("messageid" -> messageId) ~ ("response" -> msgBody)

      val msgString = compact(render(response))

      sendMessage(KafkaMessage(replyTo, msgString.getBytes))
    case ISGetAllConfigsResponse(messageId, requestId, replyTo, model, configList) =>
      log.debug(s"Received reponse to req: ${requestId}")

      val items = parse(configList).extract[List[Map[String, Any]]]

      val responsePayload = ISResponseAllConfig(requestId, true, (new DateTime()).toString(), items)


      val response = ISMessageAllConfig(messageId, responsePayload)

      sendMessage(KafkaMessage(replyTo, response.toJSON.getBytes))
    case ISConfigResponse(messageId, requestId, replyTo, model, config, nodes) =>
      log.debug(s"Received reponse to req: ${requestId}")

      val responsePayload = ISResponseConfig(requestId, true, (new DateTime()).toString(), config, nodes)

      val response = ISMessageConfig(messageId, responsePayload)

      sendMessage(KafkaMessage(replyTo, response.toJSON.getBytes))
    case ISRespondWithMessage(messageId, requestId, replyTo, status, message) =>
      log.debug(s"respond with message: $message")
      if (status == 404) {
        val responsePayload = ISResponseError(requestId,
          false,
          (new DateTime()).toString(),
          message,
          status)

        val response = ISMessageError(messageId, responsePayload)

        sendMessage(KafkaMessage(replyTo, response.toJSON.getBytes))
      }
      else {
        val responsePayload = ISResponseSuccess(requestId,
          true,
          (new DateTime()).toString(),
          message)

        val response = ISMessageSuccess(messageId, responsePayload)

        sendMessage(KafkaMessage(replyTo, response.toJSON.getBytes))
      }
    case ISApplyConfigToDeviceResponse(messageId, requestId, replyTo, model, status, token) =>
      log.debug(s"Received reponse to req: ${requestId}")
      val str     = "{\"status\":200,\"message\": \"Config Applied\",\"token\":" + token + "}"
      val message = compact(parse(str))

      val tok = "{\"token\":" + token + "}"

      val responsePayload = ISResponseToken(requestId, true, (new DateTime()).toString, "SUCCESS", token)

      val response = ISMessageToken(messageId, responsePayload)
      sendMessage(KafkaMessage(replyTo, response.toJSON.getBytes))
    case ISApplySingleParamToDeviceResponse(messageId, requestId, replyTo, model, status, token) =>
      val tok = "{\"token\":" + token + "}"

      val msgBody = parse(ISResponseSuccess(requestId, true, (new DateTime()).toString, "SUCCESS").toJSON) merge parse(
        tok
      )

      val response = ("messageid" -> messageId) ~ ("response" -> msgBody)

      val msgString = compact(render(response))

      sendMessage(KafkaMessage(replyTo, msgString.getBytes))
    case VPNOnDemandResponse(messageId, requestId, replyTo, status, vpnIP) =>
      log.debug(s"Vpn on-demand Response to req: ${messageId}")
      val msgBody = ("requestid" -> requestId) ~ ("success" -> true) ~ ("timestamp" -> (new DateTime()).toString()) ~ ("vpnip" -> vpnIP)

      val response = ("messageid" -> messageId) ~ ("response" -> msgBody)

      val msgString = compact(render(response))

      sendMessage(KafkaMessage(replyTo, msgString.getBytes))
    case _ =>
      log.error("Message not handled!!")
  }

  override def preStart(): Unit =
    system.eventStream.subscribe(this.self, classOf[ISResponseEvent])

  override def postStop(): Unit =
    system.eventStream.unsubscribe(self)
}
