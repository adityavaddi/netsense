package com.verizon.netsense.services

import java.util.concurrent.atomic.AtomicInteger

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import com.verizon.netsense.entity._
import io.scalac.amqp._
import org.joda.time.DateTime
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Promise}
import scala.util.control.NonFatal
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by davor on 3/30/2017.
 */
class DeviceServiceSpec extends FlatSpec with Matchers {
  val totalMessages   = 1
  val prefetch        = 2
  val expected        = new AtomicInteger(1)
  val expected2       = new AtomicInteger(1)
  val finishedPromise = Promise[Unit]()
  val finished        = finishedPromise.future

//  implicit val system = ActorSystem()
//
//  implicit val mat = ActorMaterializer()
//  val connection   = Connection()
//  val consConn     = Connection()

  val loginReq = LoginReq(
    nid = "N03099480",
    protoc = 1,
    dev = "unode-v7",
    cid = "1",
    ssid = "SensityDefault",
    profile = "",
    ch = 24,
    tok = "ASFEF654213QWEQWEQWAFDS5632132465fq4w5e643241",
    ip = "127.0.0.1",
    t = 1488823815518486110L,
    bssid = "AD:FF:BE:CB:AB:EF:BB:AA",
    mac = "BA:FC:AF:BF:DA:FD:AE:CB",
    sec = "WPA_PSK",
    subtype = "",
    modemRevEd = ""
  )

  val reqPayload = ReqPayloadMQTT(
    a = "POST",
    p = "v1/N03099480/out/POST/loginReq",
    sid = "localhost",
    d = DateTime.now.toDateTimeISO.toString,
    uuid = "51e481b2-1f4e-4a6f-9b4e-b11c87b7d887",
    f = "",
    l = loginReq.toMsgPack
  )

  val loginResp = LoginReply(1491923486903L)

  val respPaload = RespPayloadMQTT(
    "POST",
    "v1/N03099480/out/POST/loginResp",
    "N03099480",
    "51e481b2-1f4e-4a6f-9b4e-b11c87b7d887",
    200,
    "",
    "",
    loginResp.toMsgPack
  )

  "DeviceService" should "recieve LoginReq" in {

//    val brokerReady = for {
//      e <- connection.exchangeDeclare(Exchange("DSE1", Direct, durable = false, autoDelete = true))
//      q <- connection.queueDeclare(Queue("DSQ1", autoDelete = true))
//      b <- connection.queueBind("DSQ1", "DSE1", "dsq1")
//    } yield b
//    Await.result(brokerReady, 10 seconds)
//
//    val qPublisher  = consConn.consume(queue = "DSQ1", prefetch = prefetch)
//    val eSubscriber = connection.publish(exchange = "DSE1", routingKey = "dsq1")
//
//    Source
//      .fromIterator(() => (1 to totalMessages).iterator)
//      .map(i => Message(body = reqPayload.toMsgPack))
//      .runWith(Sink.fromSubscriber(eSubscriber))
//
//    Source
//      .fromPublisher(qPublisher)
//      .map { msg =>
//        val payload = ReqPayload.fromMsgPack(msg.message.body.toArray)
//        payload
//      }
//      .runWith(Sink.foreach(checkExpectedRequest))
//    //Test takes 3s on decent PC so I give 10x.
//    //Await.result(finished, 60.seconds)
//    //Await.result(system.terminate(), 5.seconds)
////    connection1.shutdown()
////    consConn1.shutdown()
  }

  it should "send LoginReply" in {

//    val brokerReady = for {
//      e <- connection.exchangeDeclare(Exchange("DSE2", Direct, durable = false, autoDelete = true))
//      q <- connection.queueDeclare(Queue("DSQ2", autoDelete = true))
//      b <- connection.queueBind("DSQ2", "DSE2", "dsq2")
//    } yield b
//    Await.result(brokerReady, 10 seconds)
//
//    val qPublisher  = consConn.consume(queue = "DSQ2", prefetch = prefetch)
//    val eSubscriber = connection.publish(exchange = "DSE2", routingKey = "dsq2")
//
//    Source
//      .fromIterator(() => (1 to totalMessages).iterator)
//      .map { elem =>
//        Message(body = respPaload.toMsgPack)
//      }
//      .runWith(Sink.fromSubscriber(eSubscriber))
//
//    Source
//      .fromPublisher(qPublisher)
//      .map { msg =>
//        val payload = RespPayload.fromMsgPack(msg.message.body.toArray)
//        payload
//      }
//      .runWith(Sink.foreach(checkExpectedResponse))
//    //Test takes 3s on decent PC so I give 10x.
//    Await.result(finished, 60.seconds)
//    connection.shutdown()
//    consConn.shutdown()
//    Await.result(system.terminate(), 5.seconds)
  }

  def checkExpectedRequest(payload: ReqPayloadMQTT): Unit = {
    val exp  = expected.getAndIncrement()
    val exp2 = expected2.get()
    try {
      //payload should be(preamble)
      payload.l should be(loginReq.toMsgPack)

      val receivedPl = LoginReq.fromMsgPack(payload.l)

      receivedPl should be(loginReq)

      if (exp >= totalMessages && exp2 >= totalMessages) {
        finishedPromise.success(())
      }
    } catch {
      case NonFatal(ex) =>
        finishedPromise.failure(ex)
    }
  }

  def checkExpectedResponse(payload: RespPayloadMQTT): Unit = {
    val exp2 = expected2.getAndIncrement()
    val exp1 = expected.get()
    try {
      //payload should be(preamble)
      payload.l should be(loginResp.toMsgPack)

      val receivedPl = LoginReply.fromMsgPack(payload.l)

      receivedPl should be(loginResp)

      if (exp2 >= totalMessages && exp1 >= totalMessages) {
        finishedPromise.success(())
      }
    } catch {
      case NonFatal(ex) =>
        finishedPromise.failure(ex)
    }
  }
}
