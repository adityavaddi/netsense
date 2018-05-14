package com.verizon.netsense.services

import java.util.concurrent.atomic.AtomicInteger

import akka.actor.{Actor, ActorSystem, Props}
import akka.stream.ActorMaterializer
import akka.testkit.{DefaultTimeout, ImplicitSender, TestKit, TestProbe}
import com.verizon.netsense.entity._
import io.scalac.amqp._
import org.joda.time.DateTime
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers, WordSpecLike}

import scala.concurrent.{Await, Promise}
import scala.concurrent.duration._

/**
 * Created by davor on 4/13/17.
 */
class ConfigServiceIntegrationSpec
//extends TestKit(ActorSystem("TestingKitConfigService"))
    extends FlatSpec
    with Matchers {

  //import ConfigServiceIntegrationSpec._

  val totalMessages    = 1000
  var expectedMessages = 0
  val prefetch         = 2
  val expected         = new AtomicInteger(1)
  val expected2        = new AtomicInteger(1)
  val finishedPromise  = Promise[Unit]()
  val finished         = finishedPromise.future

  val finishedPromise2 = Promise[Unit]()
  val finished2        = finishedPromise2.future

  //implicit override val system = ActorSystem()

  //implicit val mat = ActorMaterializer()
  //val connection   = Connection()
  //val consConn     = Connection()

  val uuidList = List[String]()

  val singleParamReqPayload = ReqPayloadMQTT(
    a = "GET",
    p = "v1/N03099480/in/GET/config/single/server",
    sid = "localhost",
    d = DateTime.now.toDateTimeISO.toString,
    uuid = "51e481b2-1f4e-4a6f-9b4e-b11c87b7d887",
    f = "",
    l = SingleParamReq("server", "prov").toMsgPack
  )

  val configListReqPayload = ReqPayloadMQTT(
    a = "GET",
    p = "v1/N03099480/in/GET/config/list",
    sid = "localhost",
    d = DateTime.now.toDateTimeISO.toString,
    uuid = "51e481b2-1f4e-4a6f-9b4e-b11c87b7d887",
    f = "",
    l = ConfigListReq("prov").toMsgPack
  )

  val falconConfig = """{
                                          "network.wlan-x.security.psk": "netsense",
                                          "vaevent.bucket_interval": "1"
                                          }""";

  val applyConfigPayload = ReqPayloadMQTT(
    a = "PUT",
    p = "v1/N03099480/in/PUT/config/list",
    sid = "localhost",
    d = DateTime.now.toDateTimeISO.toString,
    uuid = "51e481b2-1f4e-4a6f-9b4e-b11c87b7d887",
    f = "",
    l = DeviceConfig("prov", "", falconConfig.getBytes).toMsgPack
  )

//  val configPublisherRef       = system.actorOf(ConfigPublisherActor.props(connection))
//  val configConsumerActorRef   = system.actorOf(ConfigConsumerActor.props(connection))
//  val responseReceiverActorRef = system.actorOf(Props(Predef.classOf[ResponseReceiverActor], totalMessages, system))

//  override def afterAll(): Unit =
//    shutdown()

  "ConfigService" should
  "send ApplySingleParamToNode" in {
//
//      for (i <- 1 to totalMessages) {
//        val uuid = java.util.UUID.randomUUID().toString
//        configPublisherRef ! ApplySingleParamToNode("N03099480",
//                                                    uuid,
//                                                    ConfigParam("prov", "server", "server.sensity.com".getBytes))
//        if (i == totalMessages) finishedPromise.success()
//      }
//
//      Await.result(finished, 60.seconds)
//
  }
  it should "send ApplyConfigToNode" in {

    for (j <- 1 to totalMessages) {
      val uuid = java.util.UUID.randomUUID().toString
//        configPublisherRef ! ApplyConfigToNode("N03099480",
//                                               uuid,
//                                               "prov",
//                                               "123456",
//                                               falconConfig.getBytes)
      if (j == totalMessages) finishedPromise2.success()

    }

    Await.result(finished2, 60.seconds)
  }

  it should
  "receive SingleParamResp" in {
//
//      val finishWait = TestProbe()
//      system.eventStream.subscribe(finishWait.ref, classOf[FinishStatusApplySingleToDevice])
//
//      finishWait.expectMsg(2.minutes, FinishStatusApplySingleToDevice(true))
  }
  it should "receive ApplyConfigToNodeResp" in {

//      val finishWait = TestProbe()
//      system.eventStream.subscribe(finishWait.ref, classOf[FinishStatusApplyConfigToDevice])

//      finishWait.expectMsg(2.minutes, FinishStatusApplyConfigToDevice(true))
  }
//  }

//object ConfigServiceIntegrationSpec {
//
//  case class FinishStatusApplySingleToDevice(val finished: Boolean)
//  case class FinishStatusApplyConfigToDevice(val finished: Boolean)
//
//  class ResponseReceiverActor(val totalMessages: Int)(implicit val system: ActorSystem) extends Actor {
//    var expectedMessages = 0;
//    var expectedMessages2 = 0;
//    override def receive: Receive = {
//      case ISApplyConfigToDeviceResponse(uuid, s, t) =>
//        //println(s"Apply Config Received message: ${uuid}, ${s}, ${t}")
//        expectedMessages += 1
//        if (expectedMessages == totalMessages - 1) system.eventStream.publish(FinishStatusApplyConfigToDevice(true))
//      case ISApplySingleParamToDeviceResponse(uuid, s, t) =>
//        println(s"Apply Single Received message: ${uuid}, ${s}, ${t}")
//        expectedMessages2 += 1
//        if (expectedMessages2 == totalMessages - 1) system.eventStream.publish(FinishStatusApplySingleToDevice(true))
//    }
//
//    override def preStart(): Unit =
//      system.eventStream.subscribe(this.self, classOf[ISResponseEvent])
//  }
}
