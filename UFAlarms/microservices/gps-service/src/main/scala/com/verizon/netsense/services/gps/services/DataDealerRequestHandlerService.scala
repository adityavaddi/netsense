package com.verizon.netsense.services.gps.services

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.GraphDSL.Implicits.port2flow
import akka.stream.scaladsl.{Flow, GraphDSL, RunnableGraph, Sink}
import akka.stream.{ActorAttributes, ActorMaterializer, ActorMaterializerSettings, ClosedShape}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.services.gps.exceptions.AppRequestException
import com.verizon.netsense.services.gps.helper.{Common, DataDealerHelper, KafkaConfig}
import com.verizon.netsense.services.gps.model._
import com.verizon.netsense.services.gps.util.ObjectMapperUtil
import com.verizon.netsense.utils.Logging
import org.apache.kafka.clients.consumer.ConsumerRecord

import scala.concurrent.duration._

class DataDealerRequestHandlerService(dataDealerHelper: DataDealerHelper) extends Instrumented with Logging with Common {

  implicit val gpsRequestHandlerSystem = ActorSystem("DataDealer-Request-Handler-system")
  implicit val ec = gpsRequestHandlerSystem.dispatcher
  implicit val mat = ActorMaterializer(ActorMaterializerSettings(gpsRequestHandlerSystem)
    .withInputBuffer(Math.pow(2,10).toInt,Math.pow(2,20).toInt))

  val messageUnpackTimer = metrics.timer("Message Unpack Timer")
  val loadOrgHierArchyTimer = metrics.timer("Load org hierarchy timer")
  val enrichDataDealerGpsModel = metrics.timer("Populate and Enrich GPS model")
  val persistToDbTimer = metrics.timer("Persist Gps to database timer")

  val kafkaRestartsource = createRestartKafkaSource(1.seconds,10.seconds,0.2,KafkaConfig.isRequestTopic)


  val messageUnpacker : Flow[ConsumerRecord[String,String],Option[AppRequest],NotUsed] =
    Flow[ConsumerRecord[String,String]].mapAsync(parallelism){ record =>
    log.debug("unpacking kafka message " + record.value())
    ObjectMapperUtil.fromJsonAsync[AppRequest](record.value())
    }.withAttributes(ActorAttributes.supervisionStrategy(flowDecider))


  val validateRequest: Flow[Option[AppRequest],Option[AppRequest],NotUsed] =
    Flow[Option[AppRequest]].filter{ record =>
      log.debug("Filtering and Validating casel type of appRequest")
      record match {
      case None => throw new AppRequestException()
      case Some(appRequest) => CaselType.GPS.value.equalsIgnoreCase(appRequest.request.`type`)
    }}.withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val processAppRequest: Flow[Option[AppRequest],(GpsProps, Option[OrgHierarchy]),NotUsed] =
    Flow[Option[AppRequest]].mapAsync(parallelism) { apprequest =>
      log.debug("Processing App request")
      loadOrgHierArchyTimer.time{dataDealerHelper.collectGpsPropsAndOrgHierArchy(apprequest)}
    }.withAttributes(ActorAttributes.supervisionStrategy(flowDecider))


  val populateGpsModel: Flow[(GpsProps, Option[OrgHierarchy]),Gps,NotUsed] = {
    log.debug("populating Gps model")
    Flow[(GpsProps, Option[OrgHierarchy])].mapAsync(parallelism) {enrichDataDealerGpsModel.time(tuple => dataDealerHelper.populateGps(tuple))
    }.withAttributes(ActorAttributes.supervisionStrategy(flowDecider))
  }


  val persistToDb: Sink[Gps,NotUsed] = {
    Flow[Gps].mapAsync(parallelism) {gps =>
      persistToDbTimer.time{persistToDbTimer.time{dataDealerHelper.persistGps(gps)}}
    }.to(Sink.ignore).withAttributes(ActorAttributes.supervisionStrategy(dbDecider))
  }

  val runnableGraph = RunnableGraph.fromGraph(GraphDSL.create(){ implicit graphBuilder =>

    val kafkaSource = graphBuilder.add(kafkaRestartsource).out
    val messageUnpackerFlow = graphBuilder.add(messageUnpacker)
    val validateRequestFlow = graphBuilder.add(validateRequest)
    val processAppRequestFlow = graphBuilder.add(processAppRequest)
    val populateGpsModelFlow = graphBuilder.add(populateGpsModel)
    val persistToDbFlow = graphBuilder.add(persistToDb).in


    kafkaSource ~> messageUnpackerFlow ~> validateRequestFlow ~> processAppRequestFlow ~> populateGpsModelFlow ~> persistToDbFlow

    ClosedShape

  })

}
