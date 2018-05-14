package com.verizon.netsense.services.gps.services

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.GraphDSL.Implicits.{fanOut2flow, port2flow}
import akka.stream.scaladsl.{Broadcast, Flow, GraphDSL, RunnableGraph, Sink}
import akka.stream.{ActorAttributes, ActorMaterializer, ActorMaterializerSettings, ClosedShape}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.services.gps.helper._
import com.verizon.netsense.services.gps.model.{Gps, OrgHierarchy, SchGpsEvent}
import com.verizon.netsense.services.gps.util.ObjectMapperUtil
import com.verizon.netsense.utils.Logging
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.StringSerializer

import scala.concurrent.duration._

class SchGpsEventIngestionService(schGpsEventHelper: SchGpsEventHelper) extends Instrumented with Logging with Common {

  implicit val system = ActorSystem("Sch-GpsEvent-Ingestion-System")
  implicit val ec = system.dispatcher
  implicit val mat = ActorMaterializer(ActorMaterializerSettings(system)
    .withInputBuffer(Math.pow(2,10).toInt,Math.pow(2,20).toInt))


  private[this] val messageUnpackerTimer: Timer = metrics.timer("GPS Message Unpacking Timer")
  private[this] val validateGpsEventTimer: Timer = metrics.timer("Validate Gps Event Message Timer")
  private[this] val orgHierarchyLoadTimer: Timer = metrics.timer("Load OrgHierchy Timer")
  private[this] val populateSchNodeGpsTimer: Timer = metrics.timer("Populate and Enrich Gps Timer")
  private[this] val persistToDbTime: Timer = metrics.timer("Persist Gps Cassandra Timer")



  val kafkaRestartSource = createRestartKafkaSource(1.seconds,10.seconds,0.2,KafkaConfig.gpsSchTopic)

  val messageUnpackFlow: Flow[ConsumerRecord[String,String],Option[SchGpsEvent],NotUsed] =
    Flow[ConsumerRecord[String,String]]
      .mapAsync(parallelism) { record =>
        log.debug("Un Packing message " + record.value())
        messageUnpackerTimer.time{schGpsEventHelper.parseGpsEvent(record)
        }
      }.withAttributes(ActorAttributes.supervisionStrategy(flowDecider))


  val validateGpsEventFlow: Flow[Option[SchGpsEvent],Option[SchGpsEvent],NotUsed] = {
    log.debug("validate gps event for null check")
    Flow[Option[SchGpsEvent]].filter(schGpsEventHelper.filter)
      .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))
  }.withAttributes(ActorAttributes.supervisionStrategy(flowDecider))


  val populateTupleWithOrgHierarchy: Flow[Option[SchGpsEvent],(SchGpsEvent, OrgHierarchy),NotUsed] = {
    log.debug("load orgHierArchy for Node")
    Flow[Option[SchGpsEvent]].mapAsync(parallelism) { gs =>
      orgHierarchyLoadTimer.time{schGpsEventHelper.populateGpsTuple2(gs)}
    }.withAttributes(ActorAttributes.supervisionStrategy(flowDecider))
  }


  val populateGpsModel: Flow[(SchGpsEvent, OrgHierarchy),Gps,NotUsed] = {
    log.debug("populate Gps model")
    Flow[(SchGpsEvent, OrgHierarchy)].mapAsync(parallelism) {
      populateSchNodeGpsTimer.time( tuple => schGpsEventHelper.populateGpsModel(tuple))
    }.withAttributes(ActorAttributes.supervisionStrategy(flowDecider))
  }


  val persistToDb: Sink[Gps,NotUsed] = {
    Flow[Gps].mapAsync(parallelism) {gps =>
      persistToDbTime.time{schGpsEventHelper.persistGps(gps)}
    }.to(Sink.ignore).withAttributes(ActorAttributes.supervisionStrategy(dbDecider))
  }

  val gpsModelToProduceRecord: Flow[Gps,ProducerRecord[String,String],NotUsed] =
    Flow[Gps].map(x => new ProducerRecord[String,String](KafkaConfig.gpsResTopic,ObjectMapperUtil.toJson(x)))


  val producerSettings = KafkaConnector.configKafkaProducerSettings(KafkaConfig.bootStrapServers,new StringSerializer
    ,new StringSerializer)

  val sink = configKafkaProducer(producerSettings)

  val runnableGraph = RunnableGraph.fromGraph(GraphDSL.create() { implicit graphBuilder =>

    val KafkaMessageInFlow = graphBuilder.add(kafkaRestartSource).out
    val jsonToGpsEventFlow = graphBuilder.add(messageUnpackFlow)
    val validateGpsEvent = graphBuilder.add(validateGpsEventFlow)
    val populateOrgHierArchy = graphBuilder.add(populateTupleWithOrgHierarchy)
    val populateGpsModelFlow = graphBuilder.add(populateGpsModel)
    val persistToCassandra = graphBuilder.add(persistToDb).in
    val gpsModelToProducerRecordFlow = graphBuilder.add((gpsModelToProduceRecord))
    val publishToKafkaFlow = graphBuilder.add(sink).in
    val gpsBroadcaster = graphBuilder.add(Broadcast[Gps](2))

    KafkaMessageInFlow ~>   jsonToGpsEventFlow ~> validateGpsEvent ~> populateOrgHierArchy ~> populateGpsModelFlow ~>
      gpsBroadcaster ~> persistToCassandra
    gpsBroadcaster ~>  gpsModelToProducerRecordFlow ~> publishToKafkaFlow

    ClosedShape
  })


}

object SchGpsEventIngestionService {
  def apply(schGpsEventHelper: SchGpsEventHelper): SchGpsEventIngestionService =
    new SchGpsEventIngestionService(schGpsEventHelper)
}

