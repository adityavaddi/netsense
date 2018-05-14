package com.verizon.netsense.services.bridge.flow

import java.nio.charset.Charset

import akka.actor.ActorSystem
import akka.stream.scaladsl.Sink
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import com.verizon.netsense.services.utils.BaseSpec

import scala.collection.immutable.Seq
import scala.concurrent.Await
import scala.concurrent.duration._

class RabbitSpec extends BaseSpec {

  implicit val actorSystem  = ActorSystem.create("RabbitMQ-Spec")
  implicit val materializer = ActorMaterializer(ActorMaterializerSettings(actorSystem))(actorSystem)

//  "RabbitMQ Flow" should "consume messages and publish messages using rabbitmq source and sink" in {
  ignore should "consume messages and publish messages using rabbitmq source and sink" in {
    val messageCount = 10
    val messages     = 1 to messageCount

    val eventualMessages = Rabbit.rabbitSource.take(messageCount).runWith(Sink.seq)

//    val sink: Sink[Message, NotUsed] = Sink.fromSubscriber(Rabbit.fromExchange)
//    Source(messages).map(x => Message(ByteString(s"${x}"))).runWith(sink)

    val rabbitMessages: Seq[(String, Array[Byte])] = Await.result(eventualMessages, 2.seconds)
    rabbitMessages.size must equal(messageCount)

    rabbitMessages.map(msg => new String(msg._2, Charset.forName("UTF-8")).toInt) must equal(messages)
  }

}
