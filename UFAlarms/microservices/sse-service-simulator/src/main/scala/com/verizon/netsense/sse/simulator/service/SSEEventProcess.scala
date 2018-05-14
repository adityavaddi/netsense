package com.verizon.netsense.sse.simulator.service

import com.verizon.netsense.sse.simulator.Event.SimulateEvent._
import com.verizon.netsense.sse.simulator.config.ConfigLoader._
import com.verizon.netsense.utils.Logging

/**
 * Created by subrsi9 on 11/24/17.
 */
trait SSEEventProcess extends Logging {

  def startEvent() {
    if (isSimulateLoginReq) {

      sseLoginEvent
      log.info(s"""Started producing the sse-loginReq message to the kafka-topic:'$loginKafkaTopic'""")
      log.info(
        s""""Initial-Delay:$msgInitialDelay-sec, Interval:$msgInterval-sec, Count:$msgCount, No-of-Process:$parProcess"""
      )
    }

    if (isSimulateSensorSample) {

      sseSensorSampleEvent
      log.info(s"""Started producing the sse-SensorSample message to the kafka-topic:'$sensorKafkaTopic'""")
      log.info(
        s"""Message-Initial-Delay:$msgInitialDelay, Message-Interval:$msgInterval , Count:$msgCount, No-of-Process:$parProcess"""
      )
    }

    if (isSimulateCoreNodeSensorSample) {

      sseCoreNodeSensorSampleEvent
      log.info(
        s"""Started producing the sse-CoreNodeSensorSample message to the kafka-topic:'$corenodesensorKafkaTopic'"""
      )
      log.info(
        s"""Message-Initial-Delay:$msgInitialDelay, Message-Interval:$msgInterval , Count:$msgCount, No-of-Process:$parProcess"""
      )
    }

    if (isSimulateDeviceAlarm) {

      sseDeviceAlarmEvent
      log.info(s"""Started producing the sse-DeviceAlarm message to the kafka-topic:'$alertKafkaTopic'""")
      log.info(
        s"""Message-Initial-Delay:$msgInitialDelay, Message-Interval:$msgInterval , Count:$msgCount, No-of-Process:$parProcess"""
      )
    }

    if (isSimulateBusinessAlert) {

      sseBusinessAlertEvent
      log.info(s"""Started producing the sse-BusinessAlert message to the kafka-topic:'$busalertKafkaTopic'""")
      log.info(
        s"""Message-Initial-Delay:$msgInitialDelay, Message-Interval:$msgInterval , Count:$msgCount, No-of-Process:$parProcess"""
      )
    }

    if (isSimulateConStatus) {

      sseConStatusEvent
      log.info(s"""Started producing the sse-ConnectionStatus message to the kafka-topic:'$conStatusKafkaTopic'""")
      log.info(
        s"""Message-Initial-Delay:$msgInitialDelay, Message-Interval:$msgInterval , Count:$msgCount, No-of-Process:$parProcess"""
      )
    }

    if (isSimulateGpsSample) {

      sseGpsSampleEvent
      log.info(s"""Started producing the sse-GpsSample message to the kafka-topic:'$gpsKafkaTopic'""")
      log.info(
        s"""Message-Initial-Delay:$msgInitialDelay, Message-Interval:$msgInterval , Count:$msgCount, No-of-Process:$parProcess"""
      )
    }
  }
}
