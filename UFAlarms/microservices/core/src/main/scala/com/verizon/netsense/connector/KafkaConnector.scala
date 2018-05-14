package com.verizon.netsense.connector

import akka.actor.ActorSystem
import akka.kafka.{ConsumerSettings, ProducerSettings}
import com.verizon.netsense.utils.{KafkaJacksonMapper, MsgPackMapper}
import org.apache.kafka.common.serialization.{
  ByteArrayDeserializer,
  ByteArraySerializer
}

/**
 * Created by brefsdal on 3/31/17.
 */
class KafkaConnector(system: ActorSystem) {

  val producerSettings = ProducerSettings(system, new ByteArraySerializer, new ByteArraySerializer)
  val consumerSettings = ConsumerSettings(system, new ByteArrayDeserializer, new MsgPackMapper)
}
