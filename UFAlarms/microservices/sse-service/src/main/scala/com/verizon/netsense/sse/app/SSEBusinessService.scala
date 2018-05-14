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

object SSEBusinessService extends Instrumented with BaseSSEService {

  implicit def actorSystem: ActorSystem = ActorSystem.create("SSEService")

  override val mqttSinkTimer: Timer    = metrics.timer("sse-mqtt-sink")
  override val kafkaSourceTimer: Timer = metrics.timer("sse-business-alert-kafka-source")

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

  override val mqttClientId = mqttBusinessClientIdParam

  /*
   * SSEBusinessAlertFlowService Graph
   */
  def sseBusinessGraph: RunnableGraph[NotUsed] =
    RunnableGraph.fromGraph(GraphDSL.create() { implicit builder =>
      import GraphDSL.Implicits._

      val messageSource              = builder.add(restartableEnrichSource(businessAlertkafkaTopicName)).out
      val kafkaToEventFlow           = builder.add(kafkaToEvent())
      val filterBusinessAlertMessage = builder.add(businessAlertfilter)
      val prepareMqttMsg             = builder.add(prepareMqttMessage)
      val sink                       = builder.add(mqttSink).in

      messageSource ~> kafkaToEventFlow ~> filterBusinessAlertMessage ~> prepareMqttMsg ~> sink
      ClosedShape
    })

}
