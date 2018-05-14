package com.verizon.netsense.services.gps.config

import akka.kafka.scaladsl.Consumer
import akka.stream.scaladsl.Source
import akka.stream.testkit.scaladsl.TestSink
import com.verizon.netsense.services.gps.helper.KafkaConfig._
import com.verizon.netsense.services.gps.helper.{KafkaConfig, KafkaConnector}
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.common.serialization.StringDeserializer

class ISServiceKafkaConfig extends KafkaConfiguration {

  val isRequestTopic:String = KafkaConfig.isRequestTopic

  val group = groupId

  val consumerISRequest: Source[ConsumerRecord[String, String], Consumer.Control] = iSRequestHandlerservice.configKafkaSource(consumerSettings,Set(isRequestTopic))

  val isResponseTopicConsumerSetting = KafkaConnector.configKafkaConsumerSettings(embeddedBootstrapServers,groupId2,new StringDeserializer,new StringDeserializer)

  val consumerISResponse = iSRequestHandlerservice.configKafkaSource(isResponseTopicConsumerSetting,Set(isResponseTopic))

  val producerISRequest = iSRequestHandlerservice.configKafkaProducer(producerSettings)


  val source = iSRequestHandlerservice.kafkaRestartSource
  val messageParserFlow = iSRequestHandlerservice.messageParser
  val validateRequestFlow = iSRequestHandlerservice.validateRequest
  val processAppRequestFlow = iSRequestHandlerservice.processAppRequest
  val produceRecordFlow = iSRequestHandlerservice.produceRecord
  val sinkFlow = iSRequestHandlerservice.sink

  val probeistopic = consumerISRequest
    .via(messageParserFlow)
    .via(validateRequestFlow)
    .via(processAppRequestFlow)
    .via(produceRecordFlow)
    .runWith(TestSink.probe)

}

object ISServiceKafkaConfig {
  def apply: ISServiceKafkaConfig = new ISServiceKafkaConfig()
}
