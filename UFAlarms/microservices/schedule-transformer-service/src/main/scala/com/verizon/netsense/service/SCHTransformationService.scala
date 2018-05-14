package com.verizon.netsense.service

import java.net.InetAddress
import java.util.UUID

import akka.actor.ActorSystem
import akka.stream.ThrottleMode.Shaping
import akka.stream._
import akka.stream.alpakka.mqtt.MqttMessage
import akka.stream.scaladsl.GraphDSL.Implicits._
import akka.stream.scaladsl.{Flow, GraphDSL, RunnableGraph, Source}
import com.verizon.netsense.config.STSConfigLoader
import com.verizon.netsense.connector.KafkaConnection
import com.verizon.netsense.database.DbLayer
import com.verizon.netsense.helper.{KafkaHelper, MQTTHelper}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.model._
import com.verizon.netsense.util.Common
import com.verizon.netsense.utils.{Logging, ObjectMapperUtil}
import nl.grons.metrics.scala.Meter
import org.joda.time.DateTime

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}


/**
  * Created by ssaurav on 1/2/18.
  */
case class SCHTransformationService()(implicit system: ActorSystem, mat: Materializer, ec: ExecutionContext,
                                      dbLayer: DbLayer)
  extends Instrumented
  with Logging
  with Common {

  //Source Kafka
  val kafkaHelper = new KafkaHelper(new KafkaConnection(system), "sts-kafka-source-consumer")
  private[this] val mqttSinkMeter: Meter = metrics.meter("sts-toMQTT-meter")

  //Sink MQTT
  val hostName     = InetAddress.getLocalHost
  lazy val uniqueMqttClientId = STSConfigLoader.mqttClientId + UUID.randomUUID().toString + "-" + hostName
  val mqttHelper = new MQTTHelper(STSConfigLoader.mqttClientId + UUID.randomUUID().toString + "-" + hostName)

  //Flows
  lazy val routeMessageFlow = Flow[Array[Byte]]
    .mapAsync(parallelism){ msg =>
      log.debug("Message recieved in STS:" + msg.toString)
      ObjectMapperUtil.fromJsonAsync[STSModel](msg)
    }
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))


  lazy val flattenNodesFlow = Flow[STSModel]
    .mapAsync(parallelism) { sTSModel =>
        //Transform STS model to device specific stuff
        val slotList = ScheduleTransformer.transform(sTSModel)
        log.debug("Total slots including networks: " + slotList.size.toString)
        Future((slotList, sTSModel.nodeids, sTSModel.id))
      }
    .flatMapConcat {
      case stsTuple if stsTuple._2.nonEmpty =>
        //Async as we can send schedule to multiple devices in parallel
        Source(1 to stsTuple._2.size).mapAsync(parallelism) { x =>
          Future(new NodeSCHModel(stsTuple._1, stsTuple._2(x - 1), stsTuple._3))
        }
      case _ =>
        throw new IllegalArgumentException("Nodeids not specified for the schedule")
    }
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  def calcTimeout(startDt: DateTime): Long = (startDt.getMillis - System.currentTimeMillis()) / 1000

  lazy val slotFlow = Flow[NodeSCHModel].map {
    e => (SCHCreateMsgService.getAIOScheduleMsg(e.nodeid, e.slots,
      e.scheduleId), e)
  }.mapAsync(parallelism) { schModelTuple =>
    dbLayer.getLatestLightMode(schModelTuple._2.nodeid).map {
      //Calc timeout for the override and store(later) the new startdt if there is a timeout
      case Some(light@(Light(_nodeId, Some(driver), _, false, _, _, Some(startDt)))) if calcTimeout(startDt) > 0 =>
        log.debug("Light has existing override, pushing the LFS after schedule: " + light.toJSON)
        mqttSinkMeter.mark
        Vector(schModelTuple._1) :+ SCHCreateMsgService.getLightForceMsg(_nodeId, Array(driver))
      case _ => log.debug("Light does not have any existing override")
        Vector(schModelTuple._1)
    }.recover {
      case ex: Exception => log.error("Unable to get the light mode from db " + ex.getMessage); throw ex
    }
  }
    .mapConcat(identity)
    .throttle(elements = 1, per = STSConfigLoader.schMsgMaxRate.millis, maximumBurst = 1, mode = Shaping)
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val assignScheduleGraph = RunnableGraph.fromGraph(g = GraphDSL.create() { implicit b =>
    // Source
    val kafkaSource = b.add(kafkaHelper.restartableConsumerSource(STSConfigLoader.kafkaLSStoSTSTopic, STSConfigLoader.kafkaGroupId)).out

    // Flows
    val routeMessage = b.add(routeMessageFlow)
    val flattenNodes = b.add(flattenNodesFlow)
    val sendSlots = b.add(slotFlow)

    // Sink
    val mqttSink = b.add(mqttHelper.restartableMqttsink).in

    //Graph
    kafkaSource ~> routeMessage ~> flattenNodes ~> sendSlots ~> mqttSink

    ClosedShape
  })

}