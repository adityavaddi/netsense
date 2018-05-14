package com.verizon.netsense.actors

import java.util.zip.CRC32

import akka.actor.{Actor, ActorSystem, Props}
import akka.event.Logging
import com.verizon.netsense.connector.KafkaConnection
import com.verizon.netsense.metrics.MetricsActor
import com.verizon.netsense.entity._
import com.verizon.netsense.services.KafkaPublisherService
import com.verizon.netsense.utils.{ConfigLoader, ConfigUtils, Deserializer}
import org.joda.time.DateTime
import org.json4s._
import org.json4s.jackson.JsonMethods.{parse, _}
import org.json4s.JsonDSL._

import scala.concurrent.{ExecutionContext, Future}

/**
 * Created by davor on 5/15/2017.
 */
object ConfigPublisherKafkaActor {
  def props(connection: KafkaConnection)(implicit system: ActorSystem): Props =
    Props(Predef.classOf[ConfigPublisherKafkaActor], connection, system)
}

class ConfigPublisherKafkaActor(connection: KafkaConnection)(implicit override val system: ActorSystem)
    extends KafkaPublisherService(connection)
    with MetricsActor {

  implicit val ec = ExecutionContext.Implicits.global

  val kafkaTopic = ConfigLoader.config.getString("kafka.to-device-topic")

  override def receive: Receive = ReceiveMetrics {

    case GetSingleParam(n, p, db) =>
      log.debug(s"recevied message: n: ${n}, p: ${p}, db: ${db}")
      val nodeId     = n
      val paramName  = p
      val uuid       = java.util.UUID.randomUUID().toString
      val date       = DateTime.now().toDateTimeISO.toString()
      val routingKey = s"v1.${nodeId}.in.GET.config.single.${paramName}"
      val path       = routingKey.replace(".", "/")
      val cp         = SingleParamReq(paramName, db)
      val reqPayload = ReqPayloadMQTT("GET", path, n, date, uuid, "", cp.toMsgPack)
      sendMessage(KafkaMessage(kafkaTopic, reqPayload.toMsgPack))

    case GetConfigFromNode(n, uuid, db) =>
      log.debug(s"Recived request: GetConfigFromNode: ${uuid}")
      val nodeId     = n
      val date       = DateTime.now().toDateTimeISO.toString()
      val routingKey = s"v1.${nodeId}.in.GET.config.list"
      val path       = routingKey.replace(".", "/")
      val cp         = ConfigListReq(db)
      val reqPayload = ReqPayloadMQTT("GET", path, nodeId, date, uuid, "", cp.toMsgPack)
      sendMessage(KafkaMessage(kafkaTopic, reqPayload.toMsgPack))

    case ApplyConfigToNode(n, uuid, database, t, config) =>
      val deviceConfig = DeviceConfig(database, t, compact(parse(config)).getBytes)
      val date         = DateTime.now().toDateTimeISO.toString()
      val routingKey   = s"v1.${n}.in.PUT.config.list"
      val path         = routingKey.replace(".", "/")
      log.debug(s"Apply config to node: ${n}")
      log.debug(s"Apply config: ${config}")
      val reqPayload   = ReqPayloadMQTT("PUT", path, n, date, uuid, "", deviceConfig.toMsgPack)
      sendMessage(KafkaMessage(kafkaTopic, reqPayload.toMsgPack))

    case ApplySingleParamToNode(nodeid, uuid, param) =>
      log.debug(s"Apply Single Param to node: ${param}")
      val date       = DateTime.now().toDateTimeISO.toString()
      val routingKey = s"v1.${nodeid}.in.PUT.config.single.${param.k}"
      val path       = routingKey.replace(".", "/")
      val reqPayload = ReqPayloadMQTT("PUT", path, nodeid, date, uuid, "", param.toMsgPack)
      sendMessage(KafkaMessage(kafkaTopic, reqPayload.toMsgPack))

    case ApplyDefaultConfigToNode(nodeid, defaultConfig) =>
      val date         = DateTime.now().toDateTimeISO.toString()
      val routingKey   = s"v1.${nodeid}.in.PUT.config.list"
      val path         = routingKey.replace(".", "/")
      val config       = parse(defaultConfig)
      val bytes        = compact(config).getBytes
      val token        = ConfigUtils.getToken(config.extract[Map[String, Any]])
      val deviceConfig = DeviceConfig("prov", token, bytes)
      log.debug(s"Apply default config to node: ${nodeid}")
      log.debug(s"Apply default config: ${defaultConfig}")

      val reqPayload =
        ReqPayloadMQTT("PUT", path, nodeid, date, java.util.UUID.randomUUID.toString, "", deviceConfig.toMsgPack)
      sendMessage(KafkaMessage(kafkaTopic, reqPayload.toMsgPack))

    case ResetDevice(nodeid, uuid) =>
      val date       = DateTime.now().toDateTimeISO.toString()
      val routingKey = s"v1.${nodeid}.in.EXEC.device.reboot"
      val path       = routingKey.replace(".", "/")
      val reqPayload = ReqPayloadMQTT("EXEC", path, nodeid, date, uuid, "", "".getBytes)
      log.info(s"Send message ${reqPayload.toJSON}")
      sendMessage(KafkaMessage(kafkaTopic, reqPayload.toMsgPack))

    case ResetDeviceConfig(nodeid, uuid, db) =>
      val date       = DateTime.now().toDateTimeISO.toString()
      val routingKey = s"v1.${nodeid}.in.EXEC.config.reset.${db}"
      val path       = routingKey.replace(".", "/")
      val reqPayload = ReqPayloadMQTT("EXEC", path, nodeid, date, uuid, "", "".getBytes)
      log.info(s"Send message ${reqPayload.toJSON}")
      sendMessage(KafkaMessage(kafkaTopic, reqPayload.toMsgPack))
    
    case VpnOnDemand(nodeId, messageId, command) =>
      val date       = DateTime.now().toDateTimeISO.toString()
      val routingKey   = s"v1.${nodeId}.in.PUT.vpn.on-demand"
      val path         = routingKey.replace(".", "/")

      val reqPayload = ReqPayloadMQTT("PUT", path, nodeId, date, messageId, "", Deserializer.msgpackMapper.writeValueAsBytes(Map("a" -> command)))
      log.info(s"Send VPN command: ${command} message ${reqPayload.toJSON}")
      sendMessage(KafkaMessage(kafkaTopic, reqPayload.toMsgPack))
    
  }

  override def preStart(): Unit =
    system.eventStream.subscribe(this.self, classOf[ConfigPublisherEvent])

  override def postStop(): Unit =
    system.eventStream.unsubscribe(this.self)
}
