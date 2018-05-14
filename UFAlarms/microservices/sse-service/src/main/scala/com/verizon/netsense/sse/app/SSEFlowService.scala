package com.verizon.netsense.sse.app

/**
 * Created by sundach on 6/15/17.
 */
import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream._
import akka.stream.scaladsl.{GraphDSL, RunnableGraph}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.sse.config.SSEConfigLoader._
import com.verizon.netsense.sse.db.database.SSEDatabase
import com.verizon.netsense.sse.service.BaseSSEService
import nl.grons.metrics.scala.Timer

import scala.util.control.NonFatal

object SSEFlowService extends Instrumented with BaseSSEService {

  implicit def actorSystem: ActorSystem = ActorSystem.create("SSEService")

  override val mqttSinkTimer: Timer    = metrics.timer("sse-mqtt-sink")
  override val kafkaSourceTimer: Timer = metrics.timer("sse-kafka-source")

  /*
   * Flow Decider
   */
  def decider(): Supervision.Decider = {

    case NonFatal(ex) =>
      log.warn("Got non fatal exception in SSEActor flow", ex)
      Supervision.Restart
    case ex =>
      log.error("Got fatal exception in SSEActor flow, stream will be stopped", ex)
      Supervision.Stop
  }

  implicit val materializer: ActorMaterializer = ActorMaterializer(
    ActorMaterializerSettings(actorSystem).withSupervisionStrategy(decider())
  )

  override def kafkaBootstrapServers: String = kafkaServersParam
  override def kafkaClientId: String         = kafkaClientIdParam
  override def kafkaGroupId: String          = kafkaGroupIdParam
  override def kafkaOffsetReset: String      = kafkaOffsetResetParam

  implicit val parallel: Int = parallellismParam

  object MicroservicesDb extends SSEDatabase(sseCassandraConnector)
  override def database: SSEDatabase = MicroservicesDb
  implicit val session               = database.session

  override val mqttClientId = mqttClientIdParam

  /*
   * SSEFlowService Graph
   */
  def sseGraph: RunnableGraph[NotUsed] =
    RunnableGraph.fromGraph(GraphDSL.create() { implicit builder =>
      import GraphDSL.Implicits._

      val messageSource = builder.add(restartableEnrichSource(kafkaTopicNamesParam)).out
      /*
       * Merge all the kafka topics
       */
      val kafkaToEventFlow = builder.add(kafkaToEvent())

      /*
       * SSE service instances enriches orgid, siteid using the nodeid from a lookup table
       */
      val enrichMessage = builder.add(enrich)

      /*
       * SSE service instances POST filtered messages to MQTT topic by URI
       */
      val filterMessage = builder.add(filter)

      /*
       * SSE service instances POST filtered messages to MQTT topic by URI
       */
      val prepareMqttMsg = builder.add(prepareMqttMessage)
      val sink           = builder.add(mqttSink).in

      messageSource ~> kafkaToEventFlow ~> enrichMessage ~> filterMessage ~> prepareMqttMsg ~> sink
      ClosedShape
    })

}
