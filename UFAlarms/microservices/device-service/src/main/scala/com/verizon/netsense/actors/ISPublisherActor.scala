package com.verizon.netsense.actors

import akka.actor.{ActorSystem, Props}
import com.verizon.netsense.connector.KafkaConnection
import com.verizon.netsense.entity.{ISResponse, KafkaMessage}
import com.verizon.netsense.metrics.MetricsActor
import com.verizon.netsense.services.KafkaPublisherService
import com.verizon.netsense.utils.Logging
import org.joda.time.DateTime
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.JsonDSL._

/**
 * Created by davor on 7/6/17.
 */
object ISPublisherActor {
  def props(connection: KafkaConnection)(implicit system: ActorSystem): Props =
    Props(Predef.classOf[ISPublisherActor], connection, system)
}

class ISPublisherActor(connection: KafkaConnection)(implicit override val system: ActorSystem)
    extends KafkaPublisherService(connection)
    with MetricsActor
    with Logging {

  override def receive: Receive = ReceiveMetrics {
    case ISResponse(messageId, requestId, replyTo, model, message) =>
      log.debug(s"Received reponse to req: ${requestId}")

      val msgBody = ("requestid" -> requestId) ~ ("success" -> true) ~ ("timestamp" -> (new DateTime())
        .toString()) ~ ("model" -> model) ~ ("status" -> parse(message))

      val response = ("messageid" -> messageId) ~ ("response" -> msgBody)

      val msgString = compact(render(response))

      sendMessage(KafkaMessage(replyTo, msgString.getBytes))
  }
}
