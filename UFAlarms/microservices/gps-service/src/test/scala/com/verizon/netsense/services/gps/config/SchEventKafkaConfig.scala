package com.verizon.netsense.services.gps.config

import akka.stream.testkit.scaladsl.TestSink
import com.verizon.netsense.services.gps.helper.KafkaConfig
import com.verizon.netsense.services.gps.helper.KafkaConfig.groupId

class SchEventKafkaConfig extends KafkaConfiguration {

  val schTopic:String = KafkaConfig.gpsSchTopic

  val group = groupId

  val schEventConsumerSource = schGpsEventIngestionService.configKafkaSource(consumerSettings,Set(schTopic))

  val producerISRequest = schGpsEventIngestionService.configKafkaProducer(producerSettings)


  val KafkaMessageInFlow = schGpsEventIngestionService.kafkaRestartSource
  val jsonToGpsEventFlow = schGpsEventIngestionService.messageUnpackFlow
  val validateGpsEvent = schGpsEventIngestionService.validateGpsEventFlow
  val populateOrgHierArchy = schGpsEventIngestionService.populateTupleWithOrgHierarchy
  val populateGpsModelFlow = schGpsEventIngestionService.populateGpsModel
  val persistToCassandra = schGpsEventIngestionService.persistToDb
  val gpsModelToProducerRecordFlow = schGpsEventIngestionService.gpsModelToProduceRecord
  val publishToKafkaFlow = schGpsEventIngestionService.sink

  val probe = schEventConsumerSource
    .via(jsonToGpsEventFlow)
    .via(validateGpsEvent)
    .via(populateOrgHierArchy)
    .via(populateGpsModelFlow)
    .via(gpsModelToProducerRecordFlow)
    .runWith(TestSink.probe)

}

object SchEventKafkaConfig {
  def apply: SchEventKafkaConfig = new SchEventKafkaConfig()
}
