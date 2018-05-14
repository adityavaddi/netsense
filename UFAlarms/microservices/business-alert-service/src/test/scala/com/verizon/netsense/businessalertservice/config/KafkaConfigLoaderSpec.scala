package com.verizon.netsense.businessalertservice.config

import com.verizon.netsense.businessalertservice.util.BaseSpec

/**
 * Created by jittara on 27/02/18.
 */
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
