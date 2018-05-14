package com.verizon.netsense.services.utils

/**
 * Created by thimija on 7/5/17.
 */
class ConfigLoaderSpec extends BaseSpec {

  val configLoaderInTest = com.verizon.netsense.services.bridge.utils.ConfigLoader

  /**
   * Graphite configuration
   */
  it should "load the Graphite configuration" in {

    configLoaderInTest.graphiteHost mustNot be(None)
    configLoaderInTest.graphitePort mustNot be(None)

  }

  /**
   * RabbitMQ configuration
   */
  it should "load the RabbitMQ configuration" in {

    configLoaderInTest.rabbitConfig mustNot be(None)
    configLoaderInTest.addresses mustNot be(None)
  }

  /**
   * Bridge configuration
   */
  it should "load the Bridge configuration" in {

    configLoaderInTest.bridgeConfig mustNot be(None)

    configLoaderInTest.bridgeConfig mustNot be(None)
    configLoaderInTest.bridgeType mustNot be(None)
    configLoaderInTest.log_info_rate mustNot be(None)

    configLoaderInTest.bridgeToKafkaConfig mustNot be(None)
    configLoaderInTest.bridgeFromKafkaConfig mustNot be(None)

    configLoaderInTest.bridgeFromRabbit mustNot be(None)
    configLoaderInTest.bridgeToRabbit mustNot be(None)

    configLoaderInTest.toKafka_topic mustNot be(None)
    configLoaderInTest.fromRabbit_exchange mustNot be(None)
    configLoaderInTest.fromRabbit_routing mustNot be(None)
    configLoaderInTest.fromRabbit_queue mustNot be(None)
    configLoaderInTest.fromKafka_topic mustNot be(None)
    configLoaderInTest.fromKafka_consumer_group mustNot be(None)
    configLoaderInTest.toRabbit_exchange mustNot be(None)
    configLoaderInTest.toRabbit_routing mustNot be(None)
    configLoaderInTest.mqttConfig mustNot be(None)
    configLoaderInTest.mqttBroker mustNot be(None)
    configLoaderInTest.mqttClientId mustNot be(None)
    configLoaderInTest.mqttAuthUser mustNot be(None)
    configLoaderInTest.mqttAuthPW mustNot be(None)
    configLoaderInTest.mqttCleanSession mustNot be(None)
    configLoaderInTest.mqttFromTopic mustNot be(None)
    configLoaderInTest.mqttToTopic mustNot be(None)

  }

}
