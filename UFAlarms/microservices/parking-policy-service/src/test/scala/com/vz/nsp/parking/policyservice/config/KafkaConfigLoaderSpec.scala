package com.vz.nsp.parking.policyservice.config

import com.vz.nsp.parking.config.KafkaConfig
import com.vz.nsp.parking.policyservice.util.BaseSpec
//import com.vz.nsp.parking.tagservice.util._

/**
 * Created by maidapr on 5/5/17.
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
