package com.verizon.netsense.service

import java.net.InetAddress
import java.util.UUID

import akka.actor.ActorSystem
import akka.stream.{ActorAttributes, ClosedShape, Materializer}
import akka.stream.scaladsl.{Flow, GraphDSL, RunnableGraph, Source}
import akka.stream.scaladsl.GraphDSL.Implicits._
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import com.verizon.netsense.config.STSConfigLoader
import com.verizon.netsense.connector.{KafkaConnection, MqttConnector}
import com.verizon.netsense.helper.{KafkaHelper, MQTTHelper}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.model._
import com.verizon.netsense.util.Common
import com.verizon.netsense.utils.{Logging, ObjectMapperUtil}
import nl.grons.metrics.scala.Meter
import org.msgpack.jackson.dataformat.MessagePackFactory

import scala.concurrent.{ExecutionContext, Future}

case class SCHLightForceService()(implicit system: ActorSystem, mat: Materializer, ec: ExecutionContext)
  extends Instrumented
    with Logging
    with Common {

  //Source Kafka
  val kafkaHelper = new KafkaHelper(new KafkaConnection(system), "lfs-kafka-source-consumer")
  private[this] val mqttSinkMeter: Meter = metrics.meter("lfs-toMQTT-meter")

  val hostName     = InetAddress.getLocalHost
  lazy val uniqueMqttClientId = STSConfigLoader.mqttClientId + UUID.randomUUID().toString + "-" + hostName
  val mqttHelper = new MQTTHelper(STSConfigLoader.mqttClientId + UUID.randomUUID().toString + "-" + hostName)


  //Flows
  lazy val routeMessageFlow = Flow[Array[Byte]]
    .mapAsync(parallelism){ msg =>
      log.debug("Message recieved in STS:" + msg.toString)
      ObjectMapperUtil.fromJsonAsync[LightControlSTSModel](msg)
    }.withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  lazy val flattenNodesFlow = Flow[LightControlSTSModel]
    .mapAsync(parallelism = 1000) {
      //Unpacking it beforehand for optimization
      case lcModel if lcModel.name == "LightingForceState" => {
        val msgpackMapper = new ObjectMapper(new MessagePackFactory) with ScalaObjectMapper
        msgpackMapper.registerModule(DefaultScalaModule)
        val lvalue = msgpackMapper.readValue[LFSPayload](lcModel.payload)
        log.debug("The light value is: " + lvalue.toString)
        Future((lcModel.name, lcModel.nodeids, Some(lvalue.level)))
      }
      case lcModel if lcModel.name == "LightingSetAuto" => {
        Future((lcModel.name, lcModel.nodeids, None))
      }
      case lcModel => {
        throw new IllegalArgumentException(s"We do not recognize this message type: ${lcModel.name}")
      }
    }
    .flatMapConcat ( model =>
        //Async as we can send schedule to multiple devices in parallel
        Source(1 to model._2.size).mapAsync(parallelism) { x =>
          log.debug("LightControlSTSModel: " + model._2(x - 1))
          Future(new NodeLightModel(model._1, model._2(x - 1), model._3))
        }
    ).withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  lazy val slotFlow = Flow[NodeLightModel]
    .mapAsync(parallelism = 1000) {
      case nodeModel if nodeModel.name == "LightingForceState" => {
        mqttSinkMeter.mark
        Future(SCHCreateMsgService.getLightForceMsg(nodeModel.nodeid, nodeModel.level.get))
      }
      case nodeModel if nodeModel.name == "LightingSetAuto" => {
        mqttSinkMeter.mark
        Future(SCHCreateMsgService.getLightAutoMsg(nodeModel.nodeid))
      }
    }.withAttributes(ActorAttributes.supervisionStrategy(flowDecider))


    val assignScheduleGraph = RunnableGraph.fromGraph(g = GraphDSL.create() { implicit b =>

    // Source
    val lfsKafkaSource = b.add(kafkaHelper.restartableConsumerSource(STSConfigLoader.kafkaLFSFromLSStoSTSTopic, STSConfigLoader.kafkaLFSGroupId)).out

    // Flows
    val unmarshallMsg = b.add(routeMessageFlow)
    val flattenNodes = b.add(flattenNodesFlow)
    val sendSlots = b.add(slotFlow)

    // Sink
    val mqttSink = b.add(mqttHelper.restartableMqttsink).in

    //Graph
    lfsKafkaSource ~> unmarshallMsg ~> flattenNodes ~> sendSlots ~> mqttSink

    ClosedShape
  })

}
