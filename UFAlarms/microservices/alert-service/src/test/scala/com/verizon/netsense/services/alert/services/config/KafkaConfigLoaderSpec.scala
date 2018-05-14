package com.verizon.netsense.services.alert.services.config

import com.verizon.netsense.services.alert.helper.KafkaConfig
import org.scalatest._

/**
 * Created by maidapr on 5/5/17.
 */
class KafkaConfigLoaderSpec extends FlatSpecLike with MustMatchers with BeforeAndAfterAll {

  override protected def beforeAll(): Unit =
    super.beforeAll()

  override def afterAll(): Unit =
    super.afterAll()

  "Kafka Config Loader" should "load the kafka config" in {
    val kafkaHost    = KafkaConfig.kafkaHost
    val kafkaPort    = KafkaConfig.kafkaPort
    val bootStrapper = KafkaConfig.bootStrapServers
    bootStrapper mustNot be(null)
    kafkaHost mustNot be(null)
    kafkaPort mustNot be(0)
  }
}
