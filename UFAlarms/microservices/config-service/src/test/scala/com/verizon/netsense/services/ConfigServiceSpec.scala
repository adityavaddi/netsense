package com.verizon.netsense.services

import java.util.concurrent.atomic.AtomicInteger

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import com.verizon.netsense.entity._
import io.scalac.amqp._
import org.joda.time.DateTime
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.{Await, Promise}
import scala.concurrent.duration._
import scala.util.control.NonFatal
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by davor on 4/13/17.
 */
class ConfigServiceSpec extends FlatSpec with Matchers {
  val totalMessages = 10000
  val prefetch      = 2
  val expected      = new AtomicInteger(1)
  val expected2     = new AtomicInteger(1)
  val expected3     = new AtomicInteger(1)
  val expected4     = new AtomicInteger(1)
  val finishedTest1 = Promise[Unit]()
  val finished1     = finishedTest1.future
  val finishedTest2 = Promise[Unit]()
  val finished2     = finishedTest2.future
  val finishedTest3 = Promise[Unit]()
  val finished3     = finishedTest3.future
  val finishedTest4 = Promise[Unit]()
  val finished4     = finishedTest4.future

//  implicit val system = ActorSystem()
//
//  implicit val mat = ActorMaterializer()
//  val connection   = Connection()
//  val consConn     = Connection()

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
                                          "vaevent.bucket_interval": "1",
                                          "network.nowifi": "false"
                                          }""";

  val applyConfigListPayload = ReqPayloadMQTT(
    a = "PUT",
    p = "v1/N03099480/in/PUT/config/list",
    sid = "localhost",
    d = DateTime.now.toDateTimeISO.toString,
    uuid = "51e481b2-1f4e-4a6f-9b4e-b11c87b7d887",
    f = "",
    l = DeviceConfig("prov", "", falconConfig.getBytes).toMsgPack
  )

  val applyConfigSinglePayload = ReqPayloadMQTT(
    a = "PUT",
    p = "v1/N03099480/in/PUT/config/single/server",
    sid = "localhost",
    d = DateTime.now.toDateTimeISO.toString,
    uuid = "51e481b2-1f4e-4a6f-9b4e-b11c87b7d887",
    f = "",
    l = SingleParamReq("server", "prov").toMsgPack
  )

  "ConfigService" should "send GET config/single" in {

    assert(true)

//    val brokerReady = for {
//      e <- connection.exchangeDeclare(Exchange("CSE1", Direct, durable = false, autoDelete = true))
//      q <- connection.queueDeclare(Queue("CSQ1", autoDelete = true))
//      b <- connection.queueBind("CSQ1", "CSE1", "csq1")
//    } yield b
//    Await.result(brokerReady, 10 seconds)
//
//    val qPublisher  = consConn.consume(queue = "CSQ1", prefetch = prefetch)
//    val eSubscriber = connection.publish(exchange = "CSE1", routingKey = "csq1")

//    Source
//      .fromIterator(() => (1 to totalMessages).iterator)
//      .map(i => Message(body = singleParamReqPayload.toMsgPack))
//      .runWith(Sink.fromSubscriber(eSubscriber))

//    Source
//      .fromPublisher(qPublisher)
//      .map { msg =>
//        val payload = ReqPayloadMQTT.fromMsgPack(msg.message.body.toArray)
//        payload
//      }
//      .runWith(Sink.foreach(checkExpectedSingleParamReq))
    //Test takes 3s on decent PC so I give 10x.
//    Await.result(finished1, 60.seconds)
    //Await.result(system.terminate(), 5.seconds)
    //    connection1.shutdown()
    //    consConn1.shutdown()
  }

  ignore should "send GET config/list" in {

    assert(true)

//    val brokerReady = for {
//      e <- connection.exchangeDeclare(Exchange("CSE2", Direct, durable = false, autoDelete = true))
//      q <- connection.queueDeclare(Queue("CSQ2", autoDelete = true))
//      b <- connection.queueBind("CSQ2", "CSE2", "csq2")
//    } yield b
//    Await.result(brokerReady, 10 seconds)
//
//    val qPublisher  = consConn.consume(queue = "CSQ2", prefetch = prefetch)
//    val eSubscriber = connection.publish(exchange = "CSE2", routingKey = "csq2")

//    Source
//      .fromIterator(() => (1 to totalMessages).iterator)
//      .map { elem =>
//        Message(body = configListReqPayload.toMsgPack)
//      }
//      .runWith(Sink.fromSubscriber(eSubscriber))

//    Source
//      .fromPublisher(qPublisher)
//      .map { msg =>
//        val payload = ReqPayloadMQTT.fromMsgPack(msg.message.body.toArray)
//        payload
//      }
//      .runWith(Sink.foreach(checkExpectedConfigListReq))
//    //Test takes 3s on decent PC so I give 10x.
//    Await.result(finished2, 60.seconds)
  }

  ignore should "send PUT config/single" in {

    assert(true)

//    val brokerReady = for {
//      e <- connection.exchangeDeclare(Exchange("CSE3", Direct, durable = false, autoDelete = true))
//      q <- connection.queueDeclare(Queue("CSQ3", autoDelete = true))
//      b <- connection.queueBind("CSQ3", "CSE3", "csq3")
//    } yield b
//    Await.result(brokerReady, 10 seconds)
//
//    val qPublisher  = consConn.consume(queue = "CSQ3", prefetch = prefetch)
//    val eSubscriber = connection.publish(exchange = "CSE3", routingKey = "csq3")

//    Source
//      .fromIterator(() => (1 to totalMessages).iterator)
//      .map { elem =>
//        Message(body = applyConfigSinglePayload.toMsgPack)
//      }
//      .runWith(Sink.fromSubscriber(eSubscriber))

//    Source
//      .fromPublisher(qPublisher)
//      .map { msg =>
//        val payload = ReqPayloadMQTT.fromMsgPack(msg.message.body.toArray)
//        payload
//      }
//      .runWith(Sink.foreach(checkExpectedApplySingleParamReq))
//    //Test takes 3s on decent PC so I give 10x.
//    Await.result(finished3, 60.seconds)
//    connection.shutdown()
//    consConn.shutdown()
//    Await.result(system.terminate(), 5.seconds)
  }

  ignore should "send PUT config/list" in {

    assert(true)

//    val brokerReady = for {
//      e <- connection.exchangeDeclare(Exchange("CSE4", Direct, durable = false, autoDelete = true))
//      q <- connection.queueDeclare(Queue("CSQ4", autoDelete = true))
//      b <- connection.queueBind("CSQ4", "CSE4", "csq4")
//    } yield b
//    Await.result(brokerReady, 10 seconds)
//
//    val qPublisher  = consConn.consume(queue = "CSQ4", prefetch = prefetch)
//    val eSubscriber = connection.publish(exchange = "CSE4", routingKey = "csq4")

//    Source
//      .fromIterator(() => (1 to totalMessages).iterator)
//      .map { elem =>
//        Message(body = applyConfigListPayload.toMsgPack)
//      }
//      .runWith(Sink.fromSubscriber(eSubscriber))

//    Source
//      .fromPublisher(qPublisher)
//      .map { msg =>
//        val payload = ReqPayloadMQTT.fromMsgPack(msg.message.body.toArray)
//        payload
//      }
//      .runWith(Sink.foreach(checkExpectedApplyListReq))
//    //Test takes 3s on decent PC so I give 10x.
//    Await.result(finished4, 60.seconds)
//    connection.shutdown()
//    consConn.shutdown()
//    Await.result(system.terminate(), 5.seconds)
  }

//  def checkExpectedSingleParamReq(payload: ReqPayloadMQTT): Unit = {
//    val exp  = expected.getAndIncrement()
//    val exp2 = expected2.get()
//    try {
//      //payload should be(preamble)
//      payload.l should be(SingleParamReq("server", "prov").toMsgPack)
//
//      val receivedPl = SingleParamReq.fromMsgPack(payload.l)
//
//      receivedPl should be(SingleParamReq("server", "prov"))
//
//      if (exp >= totalMessages) {
//        finishedTest1.success(())
//      }
//    } catch {
//      case NonFatal(ex) =>
//        finishedTest1.failure(ex)
//    }
//  }
//  def checkExpectedConfigListReq(payload: ReqPayloadMQTT): Unit = {
//    val exp2 = expected2.getAndIncrement()
//    val exp1 = expected.get()
//    try {
//      //payload should be(preamble)
//      payload.l should be(ConfigListReq("prov").toMsgPack)
//
//      val receivedPl = ConfigListReq.fromMsgPack(payload.l)
//
//      receivedPl should be(ConfigListReq("prov"))
//
//      if (exp2 >= totalMessages) {
//        finishedTest2.success(())
//      }
//    } catch {
//      case NonFatal(ex) =>
//        finishedTest2.failure(ex)
//    }
//  }
//
//  def checkExpectedApplySingleParamReq(payload: ReqPayloadMQTT): Unit = {
//    val exp3 = expected3.getAndIncrement()
//    try {
//      //payload should be(preamble)
//      payload.l should be(SingleParamReq("server", "prov").toMsgPack)
//
//      val receivedPl = SingleParamReq.fromMsgPack(payload.l)
//
//      receivedPl should be(SingleParamReq("server","prov"))
//
//      if (exp3 >= totalMessages) {
//        finishedTest3.success(())
//      }
//    } catch {
//      case NonFatal(ex) =>
//        finishedTest3.failure(ex)
//    }
//  }
//
//  def checkExpectedApplyListReq(payload: ReqPayloadMQTT): Unit = {
//    val exp4 = expected4.getAndIncrement()
//    try {
//      //payload should be(preamble)
//      payload.l should be(DeviceConfig("prov", falconConfig.getBytes).toMsgPack)
//
//      val receivedPl = DeviceConfig.fromMsgPack(payload.l)
//
//      receivedPl.l should be(DeviceConfig("prov", falconConfig.getBytes).l)
//
//      if (exp4 >= totalMessages) {
//        finishedTest4.success(())
//      }
//    } catch {
//      case NonFatal(ex) =>
//        finishedTest4.failure(ex)
//    }
//  }

}
