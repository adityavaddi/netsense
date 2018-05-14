package com.verizon.netsense.services.alert.services

import akka.NotUsed
import akka.stream._
import akka.stream.scaladsl.GraphDSL.Implicits._
import akka.stream.scaladsl.{Broadcast, GraphDSL, Merge, RunnableGraph}
import com.verizon.netsense.services.alert.flow.IngestionFlow
import com.verizon.netsense.services.alert.model._

/**
 * Created by vermri5 on 7/12/17.
 */
object AlertIngestionService extends IngestionFlow {

  lazy val runnableGraphCommit: RunnableGraph[NotUsed] =
    RunnableGraph.fromGraph(g = GraphDSL.create() { implicit graphBuilder =>
      val videoSource = graphBuilder.add(videoNodeKafkaSource).out

      val coreSource = graphBuilder.add(coreNodeKafkaSource).out

      val msgPackToRestPack = graphBuilder.add(messagePackTransformFlow)

      val merge = graphBuilder.add(Merge[AlarmEvent](2))

      val jsonStringToAlarmEvent = graphBuilder.add(restPackTransformFlow)

      val alarmValidationFlow = graphBuilder.add(validateAlarmFlow)

      val lookupExistingAlert = graphBuilder.add(checkExistingAlertInDbFlow)

      val lookupOrgForNewAlert = graphBuilder.add(lookupOrgFlowForNewAlert)

      val persistToDb = graphBuilder.add(persistFlow).in

      val persistToAppendOnlyDb = graphBuilder.add(persistToAppendOnlyTableSink)

      val alertBroadCaster = graphBuilder.add(Broadcast[Alert](2))

      val getUfName = graphBuilder.add(UfAlarmFlow)

      val kafkaProducer = graphBuilder.add(kafkaFlow)

      val publishToKafka = graphBuilder.add(kafkaSink).in

      val alarmBroadcaster = graphBuilder.add(Broadcast[AlarmEvent](2))

      coreSource ~> msgPackToRestPack ~> merge
      videoSource ~> jsonStringToAlarmEvent ~> merge ~>
      alarmValidationFlow ~>
      alarmBroadcaster ~> persistToAppendOnlyDb
      alarmBroadcaster ~> lookupOrgForNewAlert ~> lookupExistingAlert ~>
      alertBroadCaster ~> persistToDb
      alertBroadCaster ~> getUfName ~> kafkaProducer ~> publishToKafka

      ClosedShape
    })

}
