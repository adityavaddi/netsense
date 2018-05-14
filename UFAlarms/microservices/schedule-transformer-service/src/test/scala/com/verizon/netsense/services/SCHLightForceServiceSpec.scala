package com.verizon.netsense.services

import java.util.UUID

import akka.Done
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestKit
import com.fasterxml.jackson.annotation.JsonInclude.Include
import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.databind.{DeserializationFeature, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import com.verizon.netsense.helper.BaseSpec
import com.verizon.netsense.model._
import com.verizon.netsense.service.{SCHLightForceService, SCHTransformationService}
import com.verizon.netsense.util.MQTT
import com.verizon.netsense.utils.{Logging, ObjectMapperUtil}
import org.msgpack.jackson.dataformat.MessagePackFactory
import org.velvia.MsgPackUtils

import scala.concurrent.{Await, ExecutionContext}

class SCHLightForceServiceSpec extends TestKit(ActorSystem("SCHLFSTestSystem"))
  with BaseSpec
  with Logging {

    implicit val ec = ExecutionContext.Implicits.global
    implicit val mat = ActorMaterializer()(system)

    //Creating STS Model for integration testing
    val lfspayload = new LFSPayload(level = Array(20))
    val nodeids = Vector("cnext_1", "cnext_2")

    val model = new LightControlSTSModel(id = UUID.randomUUID().toString,
      name = "LightingForceState",
      nodeids = nodeids,
      payload = lfspayload.toMsgPack
    )



    "FlattenNodesFlow" should "distribute the schedule for each node" in {
      lazy val schFlattenNodesTest = SCHLightForceService().flattenNodesFlow

      val probe = Source.single(model)
        .via(schFlattenNodesTest)
        .runWith(TestSink.probe)

      val request = probe.request(1)
      val next1 = request.requestNext()
      assert(nodeids.contains(next1.nodeid) == true)

      val next2 = request.requestNext()
      assert(nodeids.contains(next2.nodeid) == true)

      assert(next1.nodeid != next2.nodeid)
    }

    "SlotFlow" should "send mqtt messages" in {
      val probe = Source.single(model)
        .via(SCHLightForceService().flattenNodesFlow)
        .via(SCHLightForceService().slotFlow)
        .runWith(TestSink.probe)

      val request = probe.request(1)

      val force = request.requestNext()
      assert(force.topic == "v1/cnext_1/in/PUT/light/force")

      val msgpackMapper = new ObjectMapper(new MessagePackFactory) with ScalaObjectMapper
      msgpackMapper.registerModule(DefaultScalaModule)
      val rpack = msgpackMapper.readValue[RestPackCommand](force.payload.toArray)
      val aslots = msgpackMapper.readValue[LightControlPayload](rpack.l)
      assert(aslots.lctl.level.head == 20)


      val commit1 = request.requestNext()
      assert(commit1.topic == "v1/cnext_2/in/PUT/light/force")
    }

    "SlotFlow auto" should "send auto mqtt messages" in {
      val model = new LightControlSTSModel(id = UUID.randomUUID().toString,
        name = "LightingSetAuto",
        nodeids = nodeids,
        payload = Array[Byte]()
      )

      val probe = Source.single(model)
        .via(SCHLightForceService().flattenNodesFlow)
        .via(SCHLightForceService().slotFlow)
        .runWith(TestSink.probe)

      val request = probe.request(1)

      val clear1 = request.requestNext()
      assert(clear1.topic == "v1/cnext_1/in/PUT/light/auto")

      val commit1 = request.requestNext()
      assert(commit1.topic == "v1/cnext_2/in/PUT/light/auto")
    }

  //    "RealFlow" should "mqtt send mqtt messages" in {
//      val probe = Source.single(model)
//        .via(SCHLightForceService().flattenNodesFlow)
//        .via(SCHLightForceService().slotFlow)
//        .runWith(MQTT.mqttSink)
//
//      import scala.concurrent.duration._
//
//      val completion = Await.result(probe, 10.seconds)
//      completion mustBe Done
//    }

  }
