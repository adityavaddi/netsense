package com.verizon.netsense.config

import com.typesafe.config.ConfigFactory
import com.verizon.netsense.utils.Logging

/**
  * Created by ssaurav on 1/2/18.
  */
object LSSConfigLoader extends Logging {
  lazy val config                        = ConfigFactory.load()
  lazy val kafkaGroupId                  = config.getString("kafka.group-id")
  lazy val kafkaConfigtoLSSTopic         = config.getString("kafka.device-to-lss-topic")
  lazy val kafkaConfigtoISTopic          = config.getString("kafka.lss-to-is-topic")
  lazy val kafkaDDToScheduleService      = config.getString("kafka.dd-to-lss-topic")
  lazy val kafkaScheduleLoopTriggerTopic = config.getString("kafka.cron-to-lss-topic")
  lazy val kafkaLightForceStateTopic     = config.getString("kafka.lfs-to-lss-topic")
  lazy val kafkaNodeUpdateTopic          = config.getString("kafka.node-systeminfo-topic")
  lazy val kafkaLightForceStateToSTSTopic     = config.getString("kafka.lss-to-sts-lfs-topic")
  lazy val kafkaScheduleServiceToDevice  = config.getString("kafka.lss-to-sts-topic")

  require(kafkaConfigtoLSSTopic.nonEmpty,           "DS -> LSS Kafka Topic undefined")
  require(kafkaDDToScheduleService.nonEmpty,        "DD -> LSS Kafka Topic undefined")
  require(kafkaScheduleServiceToDevice.nonEmpty,    "LSS -> STS Kafka Topic undefined")
  require(kafkaScheduleLoopTriggerTopic.nonEmpty,   "ScheduleLoop Trigger Kafka Topic undefined")

  log.info("Using kafkaGroupId "                            + kafkaGroupId)
  log.info("Subscribing to kafkaConfigtoLSSTopic "          + kafkaConfigtoLSSTopic)
  log.info("Subscribing to kafkaConfigtoISTopic "           + kafkaConfigtoISTopic)
  log.info("Subscribing to kafkaDDToScheduleService "       + kafkaDDToScheduleService)
  log.info("Subscribing to kafkaScheduleLoopTriggerTopic "  + kafkaScheduleLoopTriggerTopic)
  log.info("Subscribing to kafkaLightForceStateToSTSTopic " + kafkaLightForceStateToSTSTopic)
  log.info("Subscribing to kafkaScheduleServiceToDevice "   + kafkaScheduleServiceToDevice)
  log.info("Subscribing to kafkaNodeUpdateTopic "           + kafkaNodeUpdateTopic)
}
