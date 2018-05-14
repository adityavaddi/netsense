package com.vz.nsp.parking.userdataservice.config

import com.vz.nsp.parking.config.KafkaConfig
import com.vz.nsp.parking.userdataservice.util.BaseSpec


class KafkaConfigLoaderSpec extends BaseSpec {

  override protected def beforeAll(): Unit =
    super.beforeAll()

  override def afterAll(): Unit =
    super.afterAll()

  "Kafka Config Loader" should "load the kafka config" in {
    val expectKafkaHost    = KafkaConfig.kafkaHost
    val expectKafkaPort    = KafkaConfig.kafkaPort
    val expectBootStrapper = KafkaConfig.bootStrapServers
    expectKafkaHost mustNot be(null)
    expectKafkaPort mustNot be(0)
  }
}
