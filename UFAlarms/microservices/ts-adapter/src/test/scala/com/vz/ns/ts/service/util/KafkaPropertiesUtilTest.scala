package com.vz.ns.ts.service.util

class KafkaPropertiesUtilTest extends BaseSpec {

  ignore should "Load the Kafka properties" in {
    KafkaPropertiesUtil.props.size() mustNot equal(0)
  }

  ignore should "Create the KafkaProducer " in {
    KafkaPropertiesUtil.producer mustNot equal(null)
  }
}
