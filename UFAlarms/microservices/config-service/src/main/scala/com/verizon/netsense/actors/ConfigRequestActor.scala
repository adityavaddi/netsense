package com.verizon.netsense.actors

import akka.actor.{ActorSystem, Props}
import com.verizon.netsense.database.DeviceConfigDatabaseService
import com.verizon.netsense.entity._
import com.verizon.netsense.services.{KafkaConsumerMessageHandler, KafkaConsumerService}
import com.verizon.netsense.utils.ConfigUtils.defaultConfigMap
import com.verizon.netsense.utils.{ConfigUtils, DeviceModel, Logging}
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods.parse

import scala.concurrent.Future

/**
  * Created by davor on 3/23/2017.
  */

class ConfigRequestHandler(implicit val system: ActorSystem) extends KafkaConsumerMessageHandler with Logging {

  implicit val formats = DefaultFormats
  implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  override def messageReceive(message: String): Future[Unit] = {
    val request = ConfigRequest.fromJson(message.getBytes)
    log.debug(s"n: ${request.n}, t:${request.t}, m: ${request.m}")

    DeviceConfigDatabaseService.getConfigByNodeId(request.n).map {
      case Some(cfg) =>
        if (cfg.token != request.t)
          system.eventStream
            .publish(ApplyConfigToNode(request.n, java.util.UUID.randomUUID().toString, "prov", "", cfg.cfg))
        else log.debug(s"Tokens equal, no need for config update")
      case None =>
        defaultConfigMap.get(DeviceModel(request.m)) match {
          case Some(config) =>
            DeviceConfigDatabaseService.storeConfig(request.n, "", config._1, config._2).map { result =>
              if (result.wasApplied()) {
                system.eventStream.publish(ApplyDefaultConfigToNode(request.n, config._2))
                log.debug(s"Successfully stored config for node ${request.n}")
              } else {
                log.error(s"Failed to store config for node ${request.n}")
              }
            }
          case None =>
            log.error(s"Unsupported device model: ${request.m}!")
        }
    }
  }
}
