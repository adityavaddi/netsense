package com.verizon.netsense.services.gps.services

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.GraphDSL.Implicits.port2flow
import akka.stream.scaladsl.{Flow, GraphDSL, RestartSource, RunnableGraph}
import akka.stream.{ActorAttributes, ActorMaterializer, ActorMaterializerSettings, ClosedShape}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.services.gps.exceptions.AppRequestException
import com.verizon.netsense.services.gps.helper.KafkaConfig._
import com.verizon.netsense.services.gps.helper._
import com.verizon.netsense.services.gps.model.{AppRequest, AppResponse, CaselType}
import com.verizon.netsense.services.gps.util.ObjectMapperUtil
import com.verizon.netsense.utils.Logging
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{StringDeserializer, StringSerializer}

import scala.concurrent.duration._

class ISRequestHandlerservice(gpsHelper: GpsHelper) extends Instrumented with Logging with Common{

  implicit val system = ActorSystem("Interface-Service-Request-Handler-System")
  implicit val ec = system.dispatcher
  implicit val mat = ActorMaterializer(ActorMaterializerSettings(system)
    .withInputBuffer(Math.pow(2,10).toInt,Math.pow(2,20).toInt))

  val messageParserTimer = metrics.timer("Message Unpack Timer")
  val processAppRequestTimer = metrics.timer("Process App Request Timer")
  val produceRecordTimer = metrics.timer("Producer Record Timer")


  val consumerSettings = KafkaConnector.configKafkaConsumerSettings(bootStrapServers,groupId2,new StringDeserializer,new StringDeserializer)
  val kafkaSource = configKafkaSource(consumerSettings,Set(isRequestTopic))
  var retryCount: Int = 1
  val kafkaRestartSource = RestartSource.withBackoff(1.seconds,10.seconds,0.2) {
    () => {
      if(retryCount ==1) {
        retryCount = retryCount +1
        log.info("Starting IS Service Consumer Source")
      }
      else {
        retryCount = retryCount + 1
        log.info("Restarting IS Service Source with Failure Attempts: " + retryCount)
      }
      kafkaSource
    }
  }

  val messageParser: Flow[ConsumerRecord[String,String],Option[AppRequest],NotUsed] =
    Flow[ConsumerRecord[String,String]].mapAsync(parallelism) { record =>
      log.debug("Unpacking kafka Message " + record.value())
      messageParserTimer.time{ObjectMapperUtil.fromJsonAsync[AppRequest](record.value())}
    }.withAttributes(ActorAttributes.supervisionStrategy(flowDecider))


  val validateRequest: Flow[Option[AppRequest],Option[AppRequest],NotUsed] =
    Flow[Option[AppRequest]].filter{ record =>
      log.debug("Validating casel type of appRequest")
      record match {
      case None => throw new AppRequestException()
      case Some(appRequest) => CaselType.allRequestTypes.contains(appRequest.request.`type`)
    }}.withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val processAppRequest: Flow[Option[AppRequest],AppResponse,NotUsed] =
    Flow[Option[AppRequest]].mapAsync(parallelism) { appRequest =>
      log.debug("Processing App Request ")
      processAppRequestTimer.time{gpsHelper.processGetAppRequest(appRequest)}
    }.withAttributes(ActorAttributes.supervisionStrategy(dbDecider))

  val produceRecord: Flow[AppResponse,ProducerRecord[String,String],NotUsed] =
    Flow[AppResponse].map(appRes => {
      produceRecordTimer.time {
        log.debug("publishing response to kafka topic " + appRes.toJSON)
        new ProducerRecord[String,String](KafkaConfig.isResponseTopic,ObjectMapperUtil.toJson(appRes))
      }
    }).withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val producerSettings = KafkaConnector.configKafkaProducerSettings(KafkaConfig.bootStrapServers,new StringSerializer,new StringSerializer)

  val sink = configKafkaProducer(producerSettings)

  val runnableGraph = RunnableGraph.fromGraph(GraphDSL.create(){ implicit graphBuider =>

    val source = graphBuider.add(kafkaRestartSource).out
    val messageParserFlow = graphBuider.add(messageParser)
    val processAppRequestFlow = graphBuider.add(processAppRequest)
    val produceRecordFlow = graphBuider.add(produceRecord)
    val sinkFlow = graphBuider.add(sink).in

    source ~> messageParserFlow ~> processAppRequestFlow ~> produceRecordFlow ~> sinkFlow

    ClosedShape

  })



}

object ISRequestHandlerservice {
  def apply(gpsHelper: GpsHelper): ISRequestHandlerservice = new ISRequestHandlerservice(gpsHelper)
}
