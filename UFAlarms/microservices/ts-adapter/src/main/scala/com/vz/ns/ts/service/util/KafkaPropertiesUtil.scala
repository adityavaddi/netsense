package com.vz.ns.ts.service.util

import java.util.Properties

import com.vz.ns.ts.service.config.ConfigLoader._
import org.apache.kafka.clients.producer.{KafkaProducer, ProducerConfig}

object KafkaPropertiesUtil {

  // Generating Kafka producer properties
  val props = new Properties()
  props.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, hostAndPort)
  props.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, "org.apache.kafka.common.serialization.StringSerializer")
  props.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, "org.apache.kafka.common.serialization.StringSerializer")

  // Creating kafka producer
  val producer = new KafkaProducer[String, String](props)
}
