package com.verizon.netsense.services

import akka.actor.{ActorSystem, Props}
import com.verizon.netsense.actors._
import com.verizon.netsense.connector.KafkaConnection
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.verizon.netsense.database.{ConfigDatabaseService, ConfigDatabaseServiceImpl, DeviceConfigDatabaseService}
import com.verizon.netsense.utils.{ConfigLoader, HttpServices}

/**
 * Created by davor on 3/22/2017.
 */
object ConfigService extends App {

  Thread.sleep(30000)

  val prodDatabaseService = new ConfigDatabaseServiceImpl

  implicit val actorSystem = ActorSystem("config_service")
  implicit val mat         = ActorMaterializer()

  val connection = new KafkaConnection(actorSystem)

  val configPublisher = actorSystem.actorOf(ConfigPublisherKafkaActor.props(connection))
  //val configRequest       = actorSystem.actorOf(ConfigRequestKafkaActor.props)
  val configConsumerHandler = new ConfigConsumerKafkaHandler
  val configConsumerService = new KafkaConsumerService(connection, ConfigLoader.config.getString("kafka.from-device-topic"), configConsumerHandler)
  //  val isConsumerActor     = actorSystem.actorOf(ISConsumerKafkaActor.props(connection))

  val configRequestHandler = new ConfigRequestHandler
  val configRequestService = new KafkaConsumerService(connection, ConfigLoader.config.getString("kafka.topic-name"), configRequestHandler)

  val isConsumerHandler    = new ISConsumerKafkaHandler(prodDatabaseService)
  val isConsumerService = new KafkaConsumerService(connection, ConfigLoader.config.getString("kafka.is-request-topic"), isConsumerHandler)

  val isPublisherActor   = actorSystem.actorOf(ISPublisherKafkaActor.props(connection))

  /**
   * Binding host and port with mat
   */
  Http().bindAndHandle(HttpServices.route,
                       ConfigLoader.config.getString("httpHealth.host"),
                       ConfigLoader.config.getString("httpHealth.port").toInt)
}
