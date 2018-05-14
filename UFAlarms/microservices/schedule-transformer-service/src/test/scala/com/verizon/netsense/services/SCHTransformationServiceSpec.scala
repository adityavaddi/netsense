package com.verizon.netsense.services

import java.util.UUID

import akka.Done
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestKit
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import com.verizon.netsense.constants.Constants
import com.verizon.netsense.database._
import com.verizon.netsense.helper.BaseSpec
import com.verizon.netsense.model._
import com.verizon.netsense.service.{SCHTransformationService, ScheduleTransformer}
import com.verizon.netsense.util.MQTT
import com.verizon.netsense.utils.{Logging, ObjectMapperUtil}
import org.msgpack.jackson.dataformat.MessagePackFactory
import org.velvia.{MsgPack, MsgPackUtils}

import scala.concurrent.{Await, ExecutionContext}

class SCHTransformationServiceSpec extends TestKit(ActorSystem("SCHTransformationTestSystem"))
  with BaseSpec
  with Logging {

  implicit val ec = ExecutionContext.Implicits.global
  implicit val mat = ActorMaterializer()(system)

  //Creating STS Model for integration testing
  var slotList:Vector[SlotAction] = Vector()
  slotList = slotList :+ SlotAction(level= 10, time= Some(new ScheduleCalendar(hr=23 , min=4 , sec=0 , year=2018 , month=2 , dow=4 , dom=22)), mode="normal", lowLevel = None)

  var networkList:Vector[SlotAction] = Vector()
  networkList = networkList :+ SlotAction(level= 100, time= Some(new ScheduleCalendar(hr=23 , min= 0, sec=0 , year=2018 , month=2 , dow=4 , dom=22)), mode="normal", lowLevel = None)
  networkList = networkList :+ SlotAction(level= 0, time= Some(new ScheduleCalendar(hr=6 , min= 0, sec=0 , year=2018 , month=2 , dow=4 , dom=22)), mode="normal", lowLevel = None)

  private val tokenPrefix: String = s"schedule:${System.currentTimeMillis()}@"
  val scheduleId = tokenPrefix + UUID.randomUUID().toString
  val nodeids = Vector("cnext_1", "cnext_2")
  val model = new STSModel(scheduleId, slots = Some(slotList), network = networkList, nodeids = nodeids)

  implicit val dbLayer = new DbLayer(new PhantomService {
    override def database: MicroServicesDb = ProductionDb

  })


  "FlattenNodesFlow" should "distribute the schedule for each node" in {
    lazy val schFlattenNodesTest = SCHTransformationService().flattenNodesFlow

    val probe = Source.single(model)
                      .via(schFlattenNodesTest)
                      .runWith(TestSink.probe)

    val request = probe.request(1)
    val next1 = request.requestNext()
    assert(nodeids.contains(next1.nodeid) == true)
    assert(next1.scheduleId == scheduleId)
    assert(next1.slots.size == 3)

    val next2 = request.requestNext()
    assert(nodeids.contains(next2.nodeid) == true)
    assert(next2.scheduleId == scheduleId)
    assert(next2.slots.size == 3)

    assert(next1.nodeid != next2.nodeid)
    assert(next1.slots == next2.slots)
  }


  "SlotFlow" should "send clear, add and commit messages" in {
    val probe = Source.single(model)
      .via(SCHTransformationService().flattenNodesFlow)
      .via(SCHTransformationService().slotFlow)
      .runWith(TestSink.probe)

    import scala.concurrent.duration._
    val request = probe.request(1)

    val msg = request.requestNext(FiniteDuration(10, java.util.concurrent.TimeUnit.SECONDS))
    assert(msg.topic == "v1/cnext_1/in/PUT/light/schedule/aio")

    val msgpackMapper = new ObjectMapper(new MessagePackFactory) with ScalaObjectMapper
    msgpackMapper.registerModule(DefaultScalaModule)
    val rpack = msgpackMapper.readValue[RestPackCommand](msg.payload.toArray)
    val aslots = msgpackMapper.readValue[AIOSchedulePayload](rpack.l)
    val tokenInMsg = MsgPackUtils.unpackMap(msg.payload.toArray).get("t")

    assert(aslots.Entries.size == 3)
    assert(tokenInMsg.contains(scheduleId))
  }

  "RealFlow" should "send mqtt messages" in {
    val probe = Source.single(model)
      .via(SCHTransformationService().flattenNodesFlow)
      .via(SCHTransformationService().slotFlow)
      .runWith(MQTT.mqttSink)

    import scala.concurrent.duration._
    val completion = Await.result(probe, 10.seconds)
    completion mustBe Done
  }

}
