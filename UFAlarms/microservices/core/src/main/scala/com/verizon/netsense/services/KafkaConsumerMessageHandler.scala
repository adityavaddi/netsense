package com.verizon.netsense.services

import scala.concurrent.Future

trait KafkaConsumerMessageHandler {
  def messageReceive(msg: String): Future[Unit]
}
