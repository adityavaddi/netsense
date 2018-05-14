package com.verizon.netsense.sse.simulator.service

import com.verizon.netsense.sse.simulator.Event.SimulateEvent._
import com.verizon.netsense.sse.simulator.config.ConfigLoader
import com.verizon.netsense.sse.simulator.config.ConfigLoader._
import com.verizon.netsense.utils.Logging

/**
 * Created by subrsi9 on 11/24/17.
 */
trait SubscribeProcess extends Logging {

  val parProcess: Int = ConfigLoader.parProcess

  def startSubscribe(): Unit = {

    if (isSimulateSubscribe) {

      sseSubscribe("subscribe")
      log.info(s"""Started producing the sse-subscribe message to the kafka-topic:'$sseSubscribeTopic'""")
      log.info(
        s"""Initial-Delay:$msgInitialDelay, Interval:$subscribeInterval , Count:$subscribeCount, No-of-Process:$parProcess"""
      )
    }

    if (isSimulateHeartBeat) {

      sseHeartBeat("heartbeat")
      log.info(s"""Started producing the sse-heartbeat message to the kafka-topic:'$sseSubscribeTopic'""")
      log.info(
        s"""Initial-Delay:$msgInitialDelay, Interval:$heartBeatInterval , Count:$subscribeCount, No-of-Process:$parProcess"""
      )

    }

    if (isSimulateDisconnect) {

      sseDisconnect("disconnect")
      log.info(s"""Started producing the sse-disconnect message to the kafka-topic:'$sseSubscribeTopic'""")
      log.info(
        s"""Initial-Delay:$msgInitialDelay,Interval:$disconnectInterval , Count:$subscribeCount, No-of-Process:$parProcess"""
      )
    }

    if (isSimulateBusinessSubscribe) {

      sseBusinessSubscribe("subscribe")
      log.info(
        s"""Started producing the sse-subscribe message to the kafka-topic:'$sseBusinessSubscribeTopic'"""
      )
      log.info(
        s"""Initial-Delay:$msgInitialDelay, Interval:$subscribeInterval , Count:$subscribeCount, No-of-Process:$parProcess"""
      )
    }

    if (isSimulateBusinessHeartBeat) {

      sseBusinessHeartBeat("heartbeat")
      log.info(s"""Started producing the sse-heartbeat message to the kafka-topic:'$sseBusinessSubscribeTopic'""")
      log.info(
        s"""Initial-Delay:$msgInitialDelay, Interval:$heartBeatInterval , Count:$subscribeCount, No-of-Process:$parProcess"""
      )

    }

    if (isSimulateBusinessDisconnect) {

      sseBusinessDisconnect("disconnect")
      log.info(s"""Started producing the sse-disconnect message to the kafka-topic:'$sseBusinessSubscribeTopic'""")
      log.info(
        s"""Initial-Delay:$msgInitialDelay,Interval:$disconnectInterval , Count:$subscribeCount, No-of-Process:$parProcess"""
      )
    }
  }

}
