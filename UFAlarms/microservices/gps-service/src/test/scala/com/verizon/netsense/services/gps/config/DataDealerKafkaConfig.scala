package com.verizon.netsense.services.gps.config

import akka.stream.testkit.scaladsl.TestSink
import com.verizon.netsense.services.gps.helper.KafkaConfig
import com.verizon.netsense.services.gps.helper.KafkaConfig.groupId

class DataDealerKafkaConfig extends KafkaConfiguration {

  val ddRequestTopic:String = KafkaConfig.isRequestTopic

  val group = groupId

  val consumerDDRequest = dataDealerRequestHandlerService.configKafkaSource(consumerSettings,Set(ddRequestTopic))


  val messageUnpackerFlow = dataDealerRequestHandlerService.messageUnpacker
  val validateRequestFlow = dataDealerRequestHandlerService.validateRequest
  val processAppRequestFlow = dataDealerRequestHandlerService.processAppRequest
  val populateGpsModelFlow = dataDealerRequestHandlerService.populateGpsModel
  val persistToDbFlow = dataDealerRequestHandlerService.persistToDb

  val probeddtopic = consumerDDRequest
    .via(messageUnpackerFlow)
    .via(validateRequestFlow)
    .via(processAppRequestFlow)
    .via(populateGpsModelFlow)
    .runWith(TestSink.probe)

}

object DataDealerKafkaConfig {
  def apply: DataDealerKafkaConfig = new DataDealerKafkaConfig()
}
