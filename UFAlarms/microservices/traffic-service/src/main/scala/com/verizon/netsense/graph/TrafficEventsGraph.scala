package com.verizon.netsense.graph

import akka.actor.ActorSystem
import akka.stream.ClosedShape
import akka.stream.scaladsl.{Broadcast, GraphDSL, RunnableGraph}

/** A Traffic Graph that streams a specific type of event - to/from given Kafka consumer/producer.
  *
  *  @constructor create a specific Traffic Event Graph with given consumerSrcTopic, producerRespTopic
  *  @param _system  Actor System
  *  @param consumerSrcTopic Kafka Source topic for given Traffic Event Type (linec, objent, objdwl, objlev)
  *  @param producerRespTopic Kafka Producer topic for given Traffic Event Type (linec, objent, objdwl, objlev)
  */

class TrafficEventsGraph(var _system:ActorSystem, var consumerSrcTopic: String, var producerRespTopic: String) {
  this: TrafficGraphStages =>

  lazy val trafficGraph = RunnableGraph.fromGraph(g = GraphDSL.create() { implicit b =>
    import GraphDSL.Implicits._
    log.debug(s"Starting Traffic Graph for Event Type: ${consumerSrcTopic}")
    //Source
    val Source          = b.add(restartableKafkaTrafficConsumerSource).out

    //Flows
    val unmarshallFlow  = b.add(trafficUnmarshallFlow)
    val decodeFlow      = b.add(uuidDecodeFlow)
    val updateJsonFieldNames = b.add(updateMetadataKeys)

    //Sink
    val broadcast2      = b.add(Broadcast[String](2))
    val cSink           = b.add(consoleSink).in
    val kafkaSink       = b.add(restartableKafkaTrafficSink).in

    Source ~> unmarshallFlow ~> decodeFlow ~> updateJsonFieldNames ~> broadcast2 ~> cSink
                                              broadcast2 ~> kafkaSink

    ClosedShape

  })

}
