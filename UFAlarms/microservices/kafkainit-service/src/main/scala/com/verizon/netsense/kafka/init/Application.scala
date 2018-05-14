package com.verizon.netsense.kafka.init

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.verizon.netsense.kafka.init.ConfigLoader.envSuffix
import com.verizon.netsense.services.HealthCheckService
import com.verizon.netsense.utils.Logging

object Application extends App with Logging{
  private implicit val actorSystem = ActorSystem.create("alert_service")
  private implicit val mat = ActorMaterializer()

  log.info("Starting kafka-init service")
  log.info(s"Env Suffix:${envSuffix}")
  log.info(s"Zk Host:${ConfigLoader.zkHost}")
  log.info(s"Zk connection timeout:${ConfigLoader.zkConnectionTimeout}")
  log.info(s"Zk session timeout:${ConfigLoader.zkSessionTimeout}")

  //Bind the health check http service
  HealthCheckService.bindHttp()

  ConfigLoader.topics.forEach(topic => {
    TopicProvisioner.create(s"${topic.getString("name")}${envSuffix}", topic.getInt("partitions"), topic.getInt("replication-factor"))
  })

  TopicProvisioner.closeConnection()
}
