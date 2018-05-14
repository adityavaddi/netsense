package com.verizon.netsense.actors

import akka.actor.{Actor, ActorSystem, Props}
import com.datastax.driver.core.utils.UUIDs
import com.verizon.netsense.database._
import com.verizon.netsense.entity._
import com.verizon.netsense.services.{KafkaConsumerMessageHandler, KafkaConsumerService}
import com.verizon.netsense.utils.{ConfigLoader, DeviceModel, DeviceModels, Logging}
import org.json4s._
import org.json4s.jackson.JsonMethods.{parse, _}
import org.json4s.JsonDSL._
import com.verizon.netsense.utils.ConfigUtils._

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * Created by davor on 5/14/2017.
 */
class ISConsumerKafkaHandler(val databaseService: ConfigDatabaseBase )(implicit val system: ActorSystem) extends KafkaConsumerMessageHandler with Logging {

  implicit val formats = DefaultFormats
  implicit val ec = scala.concurrent.ExecutionContext.Implicits.global


  override def messageReceive(message: String): Future[Unit] = {
    log.debug(s"Received IS request: $message")

    val parsedMessage = parse(message)
    val messageId     = (parsedMessage \ "messageid").extractOrElse("")
    val respTopic     = (parsedMessage \ "request" \ "responsetopic").extractOrElse("")
    val replyTo       = (parsedMessage \ "responsetopic").extractOrElse(respTopic)
    val messageType   = (parsedMessage \ "request" \ "type").extract[String]
    val model         = (parsedMessage \ "request" \ "model").extractOrElse("")
    val user         = (parsedMessage \ "request" \ "user").extractOrElse("")

    val requestId = (parsedMessage \ "request" \ "requestid").extractOrElse("")
    log.info(s"requestId: $requestId")

    val requestDataString = requestId + "_" + replyTo + "_" + model

    messageType match {
      case "getDefaultConfigs" =>
        val model = (parsedMessage \ "request" \ "nodeprops" \ "model").extractOrElse("")
        import Predef._

        defaultConfigMap.get(DeviceModel(model)) match {
          case Some(config) =>
            val msgHeader = Map("cfgid" -> config._1, "model" -> model)
            val configMap = parse(config._2).extract[Map[String, Any]]
            val msgMap = msgHeader ++ configMap
            log.info("send message to eventStream")
            system.eventStream.publish(ISConfigResponse(messageId, requestId, replyTo, model, msgMap, List()))
            Future.successful()
          case None =>
            log.error(s"Unsupported device model: $model!")
            system.eventStream.publish(ISRespondWithMessage(messageId, requestId, replyTo, 500, "Unsupported device model"))
            Future.successful()

        }
      case "getDefaultConfigsForSite" =>
        val model          = (parsedMessage \ "request" \ "nodeprops" \ "model").extractOrElse("")
        val siteId         = (parsedMessage \ "request" \ "siteprops" \ "siteid").extractOrElse("")
        val network_region = (parsedMessage \ "request" \ "siteprops" \ "country_code").extractOrElse("")
        import Predef._

        defaultConfigMap.get(DeviceModel(model)) match {
          case Some(config) =>
            val body = if (network_region.length >= 2)
              config._2.replaceFirst("US", network_region.substring(0, 2))
            else config._2
            val msgHeader = Map("cfgid" -> config._1, "model" -> model)
            val configMap = parse(body).extract[Map[String, Any]]
            val msgMap = msgHeader ++ configMap
            system.eventStream.publish(ISConfigResponse(messageId, requestId, replyTo, model, msgMap, List()))
            Future.successful()
          case None =>
            log.error(s"Unsupported device model: $model!")
            system.eventStream.publish(ISRespondWithMessage(messageId, requestId, replyTo, 500, "Unsupported device model"))
            Future.successful()
        }
      case "createConfig" =>
        val name   = (parsedMessage \ "request" \ "configprops" \ "name").extractOrElse("")
        val cfgId  = (parsedMessage \ "request" \ "configprops" \ "configid").extractOrElse("")
        val orgId  = (parsedMessage \ "request" \ "orgprops" \ "orgid").extractOrElse("")
        val siteId = (parsedMessage \ "request" \ "siteprops" \ "siteid").extractOrElse("")
        val model  = (parsedMessage \ "request" \ "configprops" \ "model").extractOrElse("")

        val body = compact(parsedMessage \ "request" \ "configprops" \ "config")

        val msgHeader = render(("configid" -> cfgId) ~ ("model" -> model))

        val newBody       = model match {
                                          case "vdkmaster"  => parse(vdkDefaultConfig)    merge parse(body)
                                          case "merlin"     => parse(merlinDefaultConfig) merge parse(body)
                                          case "cnext"      => parse(cnextDefaultConfig)  merge parse(body)
                                          case _            => parse(falconDefaultConfig) merge parse(body)
                                        }

        val newBodyString = compact(newBody)

        val msgBody = msgHeader merge render(newBody)

        val msgBodyString = compact(msgBody)

        val token = getToken(newBody.extract[Map[String, Any]])

        databaseService
          .storeConfig(Config(name, cfgId, orgId, siteId, name, token, model, newBodyString))
          .map[Unit] { resultSet =>
          if (resultSet.wasApplied()) {
            log.debug(s"Success: $msgBodyString")
            val msgHeader = Map("configid" -> cfgId, "model" -> model)
            val msgBody = msgHeader ++ newBody.extract[Map[String, Any]]
            system.eventStream.publish(ISConfigResponse(messageId, requestId, replyTo, model, msgBody, List()))
            ActLogUserDatabaseService.storeActivity(ActivityLogUser(user,
                                                                    UUIDs.timeBased(),
                                                                    "createConfig",
                                                                    newBodyString,
                                                                    cfgId,
                                                                    "Config" )).map[Unit] { resultSet =>
              if (resultSet.wasApplied()) {
                log.debug(s"Activity Log saved")
              }
              else {
                log.error(s"Store activity log to cassandra failed!")
              }
            }
            if (!siteId.isEmpty)
              ActLogSiteDatabaseService.storeActivity(ActivityLogSite(siteId,
                                                                      UUIDs.timeBased(),
                                                                      "createConfig",
                                                                      newBodyString,
                                                                      cfgId,
                                                                      "Config",
                                                                      user )).map[Unit] { resultSet =>
                if (resultSet.wasApplied()) {
                  log.debug(s"Activity Log saved")
                }
                else {
                  log.error(s"Store activity log to cassandra failed!")
                }
              }

          } else {
            log.error(s"Store config to cassandra failed!")
          }
        }
      case "updateConfig" =>
        val nodeId     = (parsedMessage \ "request" \ "nodeprops" \ "nodeid") extractOrElse ("")
        val cfgId      = (parsedMessage \ "request" \ "configprops" \ "configid") extractOrElse ("")
        val orgId      = (parsedMessage \ "request" \ "orgprops" \ "orgid").extractOrElse("")
        val siteId     = (parsedMessage \ "request" \ "siteprops" \ "siteid").extractOrElse("")
        val model      = (parsedMessage \ "request" \ "configprops" \ "model").extract[String]
        val configBody = compact(parsedMessage \ "request" \ "configprops" \ "config")

        databaseService.getById(cfgId, siteId).flatMap[Unit] {
          case Some(config) =>
            val body          = config.body
            val newBody       = render(parse(body)) merge render(parse(configBody))
            val newBodyString = compact(newBody)
            val token         = getToken(newBody.extract[Map[String, Any]])

            ConfigDatabaseService.updateConfigByCfgId(cfgId, newBodyString, token).map { result =>
              if (result.wasApplied()) {
                  val msgHeader = Map("configid" -> cfgId,"model" -> model)
                  val msgBody = msgHeader ++ newBody.extract[Map[String, Any]]
                DeviceConfigDatabaseService.getNodeConfigsByConfigId(cfgId).map { result =>
                  var deviceIdList = ListBuffer[Map[String, String]]()
                  result.foreach( cfg => deviceIdList += Map("nodeid" -> cfg.nodeid, "model" -> config.model))
                  system.eventStream.publish(ISConfigResponse(messageId, requestId, replyTo, model, msgBody, deviceIdList.toList))
                }
                ActLogUserDatabaseService.storeActivity(ActivityLogUser(user,
                                                                        UUIDs.timeBased(),
                                                                        "updateConfig",
                                                                        newBodyString,
                                                                        cfgId,
                                                                        "Config" )).map[Unit] { resultSet =>
                  if (resultSet.wasApplied()) {
                    log.debug(s"Activity Log saved")
                  }
                  else {
                    log.error(s"Store activity log to cassandra failed!")
                  }
                }
                if (!siteId.isEmpty)
                  ActLogSiteDatabaseService.storeActivity(ActivityLogSite(siteId,
                                                                          UUIDs.timeBased(),
                                                                          "updateConfig",
                                                                          newBodyString,
                                                                          cfgId,
                                                                          "Config",
                                                                          user )).map[Unit] { resultSet =>
                    if (resultSet.wasApplied()) {
                      log.debug(s"Activity Log saved")
                    }
                    else {
                      log.error(s"Store activity log to cassandra failed!")
                    }
                  }

              } else {
                log.error(s"Config update failed")
              }
            }
          case _ =>
            log.error(s"Configuration $cfgId not found!")
            val errorMsg =
              compact(parse("{\"error\":true, \"message\": \"Configuration not found\", \"status\": 404}"))
            system.eventStream.publish(ISRespondWithMessage(messageId, requestId, replyTo, 404, "Configuration not found"))
            Future.successful()
        }
      case "applyConfigToNodes" | "applyConfigToSite" | "applyConfigToGroup" =>
        val nodeIds = (parsedMessage \ "request" \ "nodeprops" \ "nodeids").extract[List[String]]
        val cfgId  = (parsedMessage \ "request" \ "configprops" \ "configid") extractOrElse ("")
        val orgId  = (parsedMessage \ "request" \ "orgprops" \ "orgid").extractOrElse("")
        val siteId = (parsedMessage \ "request" \ "siteprops" \ "siteid").extractOrElse("")

        log.debug(s"NodeIds: ${nodeIds}")

        databaseService.getById(cfgId, siteId).map[Unit] {
          case Some(config) =>
            nodeIds foreach { nodeId =>
              log.debug(s"Apply config to node: $nodeId")
              DeviceConfigDatabaseService.storeConfig(nodeId, "", config.cfgid, config.body).map { result =>
                if (result.wasApplied()) {
                  system.eventStream.publish(ApplyConfigToNode(nodeId, messageId, "prov", config.token, config.body))
                  log.debug(s"Successfully stored config for node ${nodeId}")
                } else {
                  log.error(s"Failed to store config for node ${nodeId}")
                }
              }
            }
            val msgHeader = Map("configid" -> cfgId, "model" -> config.model)
            val msgBody = msgHeader ++ parse(config.body).extract[Map[String, Any]]
            var deviceIdList = ListBuffer[Map[String, String]]()
            nodeIds.foreach( nodeId => deviceIdList += Map("nodeid" -> nodeId, "model" -> config.model))

            system.eventStream.publish(ISConfigResponse(messageId, requestId, replyTo, model, msgBody, deviceIdList.toList))
            ActLogUserDatabaseService.storeActivity(ActivityLogUser(user,
                                                                    UUIDs.timeBased(),
                                                                    messageType,
                                                                    config.body,
                                                                    cfgId,
                                                                    "Config" )).map[Unit] { resultSet =>
              if (resultSet.wasApplied()) {
                log.debug(s"Activity Log saved")
              }
              else {
                log.error(s"Store activity log to cassandra failed!")
              }
            }
            if (!siteId.isEmpty)
              ActLogSiteDatabaseService.storeActivity(ActivityLogSite(siteId,
                                                                      UUIDs.timeBased(),
                                                                      messageType,
                                                                      config.body,
                                                                      cfgId,
                                                                      "Config",
                                                                      user )).map[Unit] { resultSet =>
                if (resultSet.wasApplied()) {
                  log.debug(s"Activity Log saved")
                }
                else {
                  log.error(s"Store activity log to cassandra failed!")
                }
              }
          case _ =>
            log.error(s"Configuration $cfgId not found!")
            val errorMsg =
              compact(parse("{\"error\":true, \"message\": \"Configuration not found\", \"status\": 404}"))
            system.eventStream.publish(ISRespondWithMessage(messageId, requestId, replyTo, 404, "Configuration not found"))
        }
      case "deleteConfig" =>
        val cfgId = (parsedMessage \ "request" \ "configprops" \ "configid") extractOrElse ("")
        val siteId = (parsedMessage \ "request" \ "siteprops" \ "siteid").extractOrElse("")
        databaseService.deleteConfigById(cfgId).map[Unit] { result =>
          if (result.wasApplied()) {
            val successMsg =
              compact(parse("{\"status\": 204, \"message\": \"Successfully deleted config\", \"success\": true}"))
            system.eventStream.publish(ISRespondWithMessage(messageId, requestId, replyTo, 204, "Successfully deleted config"))
            ActLogUserDatabaseService.storeActivity(ActivityLogUser(user,
                                                                    UUIDs.timeBased(),
                                                                    "deleteConfig",
                                                                    "",
                                                                    cfgId,
                                                                    "Config" )).map[Unit] { resultSet =>
              if (resultSet.wasApplied()) {
                log.debug(s"Activity Log saved")
              }
              else {
                log.error(s"Store activity log to cassandra failed!")
              }
            }
            if (!siteId.isEmpty)
              ActLogSiteDatabaseService.storeActivity(ActivityLogSite(siteId,
                                                                      UUIDs.timeBased(),
                                                                      "deleteConfig",
                                                                      "",
                                                                      cfgId,
                                                                      "Config",
                                                                      user )).map[Unit] { resultSet =>
                if (resultSet.wasApplied()) {
                  log.debug(s"Activity Log saved")
                }
                else {
                  log.error(s"Store activity log to cassandra failed!")
                }
              }
          } else {
            log.error(s"Configuration $cfgId not found!")
            val errorMsg =
              compact(parse("{\"error\":true, \"message\": \"Configuration not found\", \"status\": 404}"))
            system.eventStream.publish(ISRespondWithMessage(messageId, requestId, replyTo, 404, "Configuration not found"))
          }
        }
      case "getConfigFromNode" =>
        val nodeId = (parsedMessage \ "request" \ "nodeprops" \ "nodeid") extractOrElse ("")
        val model  = (parsedMessage \ "request" \ "model").extract[String]
        val db     = (parsedMessage \ "request" \ "configprops" \ "type").extract[String]
        log.debug(s"Send request: node: ${nodeId}, uuid: ${messageId}, model: ${model}, db: ${db}")
        system.eventStream.publish(GetConfigFromNode(nodeId, messageId, db))
        Future.successful()
      case "getConfig" =>
        val cfgId  = (parsedMessage \ "request" \ "configprops" \ "configid") extractOrElse ("")
        val orgId  = (parsedMessage \ "request" \ "orgprops" \ "orgid").extractOrElse("")
        val siteId = (parsedMessage \ "request" \ "siteprops" \ "siteid").extractOrElse("")
        databaseService.getById(cfgId, siteId).map {
          case Some(config) =>
              val configMap = parse(config.body).extract[Map[String, Any]]

              val configMapFinal = List(("cfgid" -> cfgId), ("model" -> config.model)).toMap[String, Any] ++ configMap

              val msgHeader = render(("cfgid" -> cfgId) ~ ("model" -> config.model))
            DeviceConfigDatabaseService.getNodeConfigsByConfigId(cfgId).map { result =>
              var deviceIdList = ListBuffer[Map[String, String]]()
              result.foreach( cfg => deviceIdList += Map("nodeid" -> cfg.nodeid, "model" -> config.model))
              system.eventStream.publish(ISConfigResponse(messageId, requestId, replyTo, model, configMapFinal, deviceIdList.toList))
            }
          case _ =>
            val errorMsg =
              compact(parse("{\"error\":true, \"message\": \"Configuration not found\", \"status\": 404}"))
            system.eventStream.publish(ISRespondWithMessage(messageId, requestId, replyTo, 404, "Configuration not found"))
        }
      case "getAllConfigs" =>
        log.debug(s"Get All Config")
        val siteId = (parsedMessage \ "request" \ "siteprops" \ "siteid") extractOrElse ("")
        val orgId  = (parsedMessage \ "request" \ "orgprops" \ "orgid") extractOrElse ("")
        ConfigDatabaseService.getBySiteId(siteId, orgId).map { result =>
          log.debug(s"Result: $result")
          val builder = StringBuilder.newBuilder
          builder.append("[")
          var first = true
          result foreach { element =>
            if (!first) builder.append(",")
            else first = false
            log.debug(s"Model: ${element.model}")
            val msgHeader =
              render(("configid" -> element.cfgid) ~ ("model" -> element.model) ~ ("name" -> element.cfgname))
            val configStr = compact(msgHeader merge parse(element.body))
            builder.append(configStr)
          }
          builder.append("]")
          log.debug(s"Request: ${requestId}, replyTo: ${replyTo}")
          system.eventStream.publish(ISGetAllConfigsResponse(messageId, requestId, replyTo, model, builder.mkString))
        }
      case "commandNode" =>
        val siteId = (parsedMessage \ "request" \ "siteprops" \ "siteid") extractOrElse ("")
        val orgId  = (parsedMessage \ "request" \ "orgprops" \ "orgid") extractOrElse ("")
        val nodeIds = (parsedMessage \ "request" \ "nodeprops" \ "nodeid").extract[List[String]]
        val command = (parsedMessage \ "request" \ "nodeprops" \ "cmd") extractOrElse("")
        log.debug(s"Command: $command")

        command match {
          case "ColdReset" => nodeIds foreach { nodeId =>
            system.eventStream.publish(ResetDevice(nodeId, requestId))
            system.eventStream.publish(ISRespondWithMessage(messageId, requestId, replyTo, 200, "ColdReset command sent to node"))
          }

          case "ResetProvisioning" => nodeIds foreach { nodeId =>
            system.eventStream.publish(ResetDeviceConfig(nodeId, requestId, "prov"))
            DeviceConfigDatabaseService.deleteDeviceConfig(nodeId).onComplete {
              case Success(s) => // Do Nothing
              case Failure(ex) => log.debug(s"Failed to delete config for ${nodeId} " +ex)
            }
            system.eventStream.publish(ISRespondWithMessage(messageId, requestId, replyTo, 200, "Provisional Reset command sent to node"))
          }

          case "ResetFactory" => nodeIds foreach { nodeId =>
            system.eventStream.publish(ResetDeviceConfig(nodeId, requestId, "factory"))
            DeviceConfigDatabaseService.deleteDeviceConfig(nodeId).onComplete {
              case Success(s) => // Do Nothing
              case Failure(ex) => log.debug(s"Failed to delete config for ${nodeId} " +ex)
            }
            system.eventStream.publish(ISRespondWithMessage(messageId, requestId, replyTo, 200, "Factory Reset command sent to node"))
          }

        }
        Future.successful()
      case "updateVPNInfo" | "connectToVPN" | "disconnectFromVPN" =>
       val nodeId     = (parsedMessage \ "request" \ "nodeprops" \ "nodeid") extractOrElse ("")
       val command    = (parsedMessage \ "request" \ "nodeprops" \ "command") extractOrElse ("")
       log.debug(s"Send updateVPNInfo request: node: ${nodeId}, uuid: ${messageId}")
       system.eventStream.publish(VpnOnDemand(nodeId, messageId, command))
       Future.successful()
      case _ =>
        log.error(s"Operation $messageType not implemented!")
        val error = compact(parse("{\"error\":true,\"message\": \"Operation not implemented\",\"status\": 404}"))
        val model = (parsedMessage \ "request" \ "model").extractOrElse("")
        system.eventStream.publish(ISRespondWithMessage(messageId, requestId, replyTo, 500, "Operation not implemented"))
        Future.successful()
    }
  }
}
