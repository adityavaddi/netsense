package com.verizon.netsense.sse.simulator.app

import com.verizon.netsense.sse.simulator.config.ConfigLoader.{kafkaServersParam}
import com.verizon.netsense.utils.Logging
import com.verizon.netsense.sse.simulator.service.{SSEEventProcess, SSESimulateDB, SubscribeProcess}

/**
 * Created by subrsi9 on 11/23/17.
 */
object  SSESimulatorApp extends App with SSESimulateDB with SSEEventProcess with SubscribeProcess with Logging {

  override val parProcess = parProcess

  log.info("******* SSE-Event-Simulator Starting *******")
  log.info(s"""Kafka-Server:$kafkaServersParam""")

  log.info("Initializing SSE tables !!!")

  initDB()

  log.info("Initializing SSE Event simulate !!!")
  startEvent()

  log.info("Initializing SSE subscribe !!!")
  startSubscribe()

  log.info("SSE-Event-Simulator running !!!....")

}
