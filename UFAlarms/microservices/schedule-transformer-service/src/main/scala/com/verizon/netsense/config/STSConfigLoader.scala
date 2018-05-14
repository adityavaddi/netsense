package com.verizon.netsense.config

import com.typesafe.config.ConfigFactory

/**
  * Created by ssaurav on 1/2/18.
  */
object STSConfigLoader {
  lazy val config                        = ConfigFactory.load()
  lazy val kafkaGroupId                  = config.getString("kafka.group-id")
  lazy val kafkaLFSGroupId               = config.getString("kafka.lfs-group-id")
  lazy val kafkaLSStoSTSTopic            = config.getString("kafka.lss-to-sts-topic")
  lazy val kafkaLFSFromLSStoSTSTopic     = config.getString("kafka.lss-to-sts-lfs-topic")

  lazy val mqttConfig       = config.getConfig("mqtt")
  lazy val mqttClientId     = mqttConfig.getString("clientId")
  lazy val schMsgMaxRate    = config.getInt("sch-msg-max-rate")
}
