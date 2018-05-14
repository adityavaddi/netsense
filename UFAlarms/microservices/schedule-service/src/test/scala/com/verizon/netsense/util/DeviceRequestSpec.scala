package com.verizon.netsense.util;

import java.util.{Calendar, UUID}

import akka.Done
import akka.actor.ActorSystem
import akka.stream.alpakka.mqtt.MqttMessage
import akka.stream.scaladsl.Source
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import akka.util.ByteString
import com.fasterxml.jackson.annotation.JsonInclude.Include
import com.fasterxml.jackson.databind.{DeserializationFeature, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import com.verizon.netsense.entity.Entity
import com.verizon.netsense.helper.BaseSpec
import com.verizon.netsense.nottests.MQTT

import scala.concurrent.Await

/**
  * Created by maidapr on 8/4/17.
  */
class DeviceRequestSpec extends BaseSpec {

  implicit val actorSystem = ActorSystem.create("LFS-Actor-System")
  implicit val materializer = ActorMaterializer(ActorMaterializerSettings(actorSystem))(actorSystem)

  def generateRandomUUID = UUID.randomUUID().toString

  "Device Request" should "sent to mqtt topic" in {


    val nodeId    = generateRandomUUID
    val messageId = generateRandomUUID
    case class LightingControl(
                                 pri: Int,
                                 mask: Int,
                                 level: Array[Int],
                                 quals: Int
                               ) extends Entity

    val lightControl = LightingControl(pri = 0, mask = 1, level = Array(0), quals = 0)

    case class LightControlPayload(`type`: Int, lctl: LightingControl, rates: Array[Int]) extends Entity

    case class RestPackCommand(a: String,
                        p: String,
                        sid: String,
                        d: String,
                        uuid: String,
                        f: String,
                        l: Array[Byte]) extends Entity


    val lightControlRequestPayload = LightControlPayload(1, lightControl, rates = Array(5000,5000))
    val payload: Array[Byte] = lightControlRequestPayload.toMsgPack

    val deviceRequestPayload = RestPackCommand(a = "PUT", p = s"v1/$nodeId/in/PUT/light/force",
      sid = nodeId, d = Calendar.getInstance().toInstant.toString, uuid = messageId, f = "", l = payload)

    val mapper = new ObjectMapper() with ScalaObjectMapper
    mapper.registerModule(DefaultScalaModule)
    mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
    mapper.setSerializationInclusion(Include.NON_ABSENT)

    def toJson(value: Any): String =
      mapper.writeValueAsString(value)

    val graph = Source.single(deviceRequestPayload.toMsgPack).map{ x =>
      new MqttMessage(deviceRequestPayload.p, ByteString.fromArray(x))}.runWith(MQTT.mqttSink)


    import scala.concurrent.duration._

    val completion = Await.result(graph, 10.seconds)
    completion mustBe Done
  }


}
