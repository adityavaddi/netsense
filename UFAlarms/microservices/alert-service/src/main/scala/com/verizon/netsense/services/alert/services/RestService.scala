package com.verizon.netsense.services.alert.services

import akka.NotUsed
import akka.stream._
import akka.stream.scaladsl.{GraphDSL, RunnableGraph}
import com.verizon.netsense.services.alert.flow.RestFlow

/**
 * Created by vermri5 on 7/12/17.
 */
object RestService extends RestFlow {

  val runnableGraphCommit: RunnableGraph[NotUsed] =
    RunnableGraph.fromGraph(g = GraphDSL.create() { implicit graphBuilder =>
      import GraphDSL.Implicits._

      val source = graphBuilder.add(apiKafkaSource).out.map(e => e.value())

      val jsonStringtoAppRequest = graphBuilder.add(jsonStringToAppRequestFlow)

      val requestValidationFlow = graphBuilder.add(validateRequestFlow)

      val processAppRequest = graphBuilder.add(processAppRequestFlow)

      val jsonToRecordFlow = graphBuilder.add(jsonToRecord)

      val publishToKafka = graphBuilder.add(kafkaSink).in

      source ~> jsonStringtoAppRequest ~> requestValidationFlow ~> processAppRequest ~> jsonToRecordFlow ~> publishToKafka

      ClosedShape
    })

}
