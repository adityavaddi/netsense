package com.verizon.netsense.whatifservice.config

import com.verizon.netsense.whatifservice.util.BaseSpec

/**
 * Created by jittara on 27/02/18.
 */
class KafkaConfigLoaderSpec extends BaseSpec {

  override protected def beforeAll(): Unit =
    super.beforeAll()

  override def afterAll(): Unit =
    super.afterAll()

  "Kafka Config Loader" should "load the kafka config" in {
    val expectKafkaHost    = WhatIfConfigLoader.kafkaHost
    val expectKafkaPort    = WhatIfConfigLoader.kafkaPort
    val expectBootStrapper = WhatIfConfigLoader.bootStrapServers
    expectKafkaPort mustNot be(0)
  }
}
