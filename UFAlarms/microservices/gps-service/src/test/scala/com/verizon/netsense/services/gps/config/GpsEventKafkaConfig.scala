package com.verizon.netsense.services.gps.config

import akka.stream.testkit.scaladsl.TestSink
import com.verizon.netsense.services.gps.helper.KafkaConfig.{gpsTopic, groupId}

class GpsEventKafkaConfig extends KafkaConfiguration {

  val gpsEventTopic:String = gpsTopic

  val group = groupId

  val consumerGpsEvent = gpsEventIngestionService.configKafkaSource(consumerSettings,Set(gpsEventTopic))


  val KafkaMessageInFlow = gpsEventIngestionService.kafkaRestartSource
  val jsonToGpsEventFlow = gpsEventIngestionService.messageUnpackFlow
  val validateGpsEvent = gpsEventIngestionService.validateGpsEventFlow
  val populateOrgHierArchy = gpsEventIngestionService.populateTupleWithOrgHierarchy
  val populateGpsModelFlow = gpsEventIngestionService.populateGpsModel
  val persistToCassandra = gpsEventIngestionService.persistToDb
  val gpsModelToProducerRecordFlow = gpsEventIngestionService.gpsModelToProduceRecord
  val publishToKafkaFlow = gpsEventIngestionService.sink

  val probe = consumerGpsEvent
    .via(jsonToGpsEventFlow)
    .via(validateGpsEvent)
    .via(populateOrgHierArchy)
    .via(populateGpsModelFlow)
    .via(gpsModelToProducerRecordFlow)
    .runWith(TestSink.probe)

}

object GpsEventKafkaConfig {
  def apply: GpsEventKafkaConfig = new GpsEventKafkaConfig()
}
