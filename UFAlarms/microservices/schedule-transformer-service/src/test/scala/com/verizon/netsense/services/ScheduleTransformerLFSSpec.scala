package com.verizon.netsense.services

import java.util.UUID

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestKit
import com.verizon.netsense.database.{DbLayer, EmbeddedDb, MicroServicesDb, PhantomService}
import com.verizon.netsense.helper.BaseSpec
import com.verizon.netsense.model._
import com.verizon.netsense.service.SCHTransformationService
import com.verizon.netsense.utils.Logging
import org.apache.cassandra.service.{CassandraDaemon, EmbeddedCassandraService}
import org.joda.time.DateTime
import org.velvia.{MsgPack, MsgPackUtils}

import scala.concurrent.ExecutionContext

/**
  * Created by maidapr on 3/12/18.
  */
class ScheduleTransformerLFSSpec extends TestKit(ActorSystem("SCHTransformationTestSystem-with-LFS"))
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

  val tokenPrefix = s"schedule:${System.currentTimeMillis()}@"
  val scheduleId = tokenPrefix + UUID.randomUUID().toString
  val nodeids = Vector("cnext_1", "cnext_2")
  val model = new STSModel(scheduleId, slots = Some(slotList), network = networkList, nodeids = nodeids)


  lazy val embeddedCassandraServiceObj = new EmbeddedCassandraService
  lazy val cassandraDaemon = new CassandraDaemon

  private val lightOveridePriority: Int = 0
  lazy val mockLightData = Light("cnext_1", Some(100), false, false, "override",
    Some(lightOveridePriority), Some(DateTime.now().plusHours(1)))

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    val start: Unit = embeddedCassandraServiceObj.start()
    dbLayer.storeLatestLightModeByNodeId(mockLightData)
  }

  override protected def afterAll() = {
    cassandraDaemon.destroy()
    cassandraDaemon.stop()
    cassandraDaemon.stopNativeTransport()
    super.afterAll()
  }

  implicit val dbLayer = new DbLayer(phantomService = new PhantomService {
    override def database: MicroServicesDb = EmbeddedDb })



  "Schedule Transformer" should "push LightingSchedule slots and LFS with explicit delay for existing override" in new {

    val probe = Source.single(model)
      .via(SCHTransformationService().flattenNodesFlow)
      .via(SCHTransformationService().slotFlow)
      .runWith(TestSink.probe)

    import scala.concurrent.duration._
    val request = probe.request(3)

    val aioMessage = request.requestNext(FiniteDuration(5, java.util.concurrent.TimeUnit.SECONDS))
    val aioMessageUnpackedMap_cnext_1 = MsgPackUtils.unpackMap(aioMessage.payload.toArray)


    val lfsCommand_cnext_1 = request.requestNext(FiniteDuration(10, java.util.concurrent.TimeUnit.SECONDS))
    val unpackedLfsCmd = MsgPack.unpack(lfsCommand_cnext_1.payload.toArray).asInstanceOf[Map[String, Object]]
    val unpackedPayloadMap = MsgPack.unpack(unpackedLfsCmd("l").asInstanceOf[Array[Byte]]).asInstanceOf[Map[String, Object]]
    val unpackedCtrlPayloadMap_lfs_cnext = unpackedPayloadMap("lctl").asInstanceOf[Map[String, Object]]


    val aioMessage_cnext_2            = request.requestNext(FiniteDuration(10, java.util.concurrent.TimeUnit.SECONDS))
    val aioMessageUnpackedMap_cnext_2 = MsgPackUtils.unpackMap(aioMessage_cnext_2.payload.toArray)
    val aioMessageUnpackedMap_cnext_2_payload = MsgPackUtils.unpackMap(
      aioMessageUnpackedMap_cnext_2("l").asInstanceOf[Array[Byte]]).asInstanceOf[Map[String, Object]]

    assert(aioMessage.topic == "v1/cnext_1/in/PUT/light/schedule/aio")
    assert(aioMessageUnpackedMap_cnext_1.get("t").contains(scheduleId))
    assert(aioMessage_cnext_2.topic == "v1/cnext_2/in/PUT/light/schedule/aio")
    assert(aioMessageUnpackedMap_cnext_2.get("t").contains(scheduleId))
    assert(lfsCommand_cnext_1.topic == "v1/cnext_1/in/PUT/light/force")
    assert(unpackedCtrlPayloadMap_lfs_cnext("level") == Vector(100))
    assert(unpackedCtrlPayloadMap_lfs_cnext("pri").asInstanceOf[Int] == lightOveridePriority)
    assert(aioMessageUnpackedMap_cnext_2.get("t").contains(scheduleId))
    assert(aioMessageUnpackedMap_cnext_2_payload("Entries").asInstanceOf[Vector[SlotPayload]].size == 3)

  }



}
