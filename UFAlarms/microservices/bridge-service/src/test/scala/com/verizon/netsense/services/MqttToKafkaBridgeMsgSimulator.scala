package com.verizon.netsense.services

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.alpakka.mqtt.scaladsl.MqttSink
import akka.stream.alpakka.mqtt.{MqttConnectionSettings, MqttMessage, MqttQoS}
import akka.stream.scaladsl.{Sink, Source}
import akka.util.ByteString
import com.verizon.netsense.connector.AmqpConnection
import com.verizon.netsense.services.bridge.flow.MQTT
import com.verizon.netsense.services.data.TestData
import io.scalac.amqp.Message
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence
import org.reactivestreams.Subscriber
import org.scalatest.FunSuite

import scala.concurrent.ExecutionContext

class MqttToKafkaBridgeMsgSimulator extends FunSuite with TestData {

  implicit val system = ActorSystem()
  implicit val mat    = ActorMaterializer()
  implicit val ec     = ExecutionContext.Implicits.global


  val settings = MqttConnectionSettings(
    s"tcp://localhost:1883",
    "test-scala-client",
    new MemoryPersistence,
    auth = Some(("guest", "guest")),
    socketFactory = None,
    cleanSession = true,
    will = None
  ).withClientId("unified-bridge-source")

  ignore("should generate messages from mqtt to the kafka topic based on topic mapping") {
    val mqttSink = MqttSink(settings.withClientId("test-mqtt"), MqttQoS.AtLeastOnce)
    Source( 1 to 10)
      .map(x => message(x))
      .runWith(MQTT.mqttSink)
  }


//  test("should generate messages from RMQ to the kafka topic based on topic mapping") {
//
//    implicit val system = ActorSystem()
//    implicit val mat    = ActorMaterializer()
//    implicit val ec     = ExecutionContext.Implicits.global
//
//    lazy val ampqConnection = AmqpConnection.getConnection
//    val toExchange: Subscriber[Message] = ampqConnection.publish(exchange = "node.events", routingKey = "*")
//
//    Source( 1 to 10)
//      .map(x => {
//        Message(jsonPaylodwithChildByte)
//      })
//      .runWith(Sink.fromSubscriber(toExchange))
//    Thread.sleep(15000)
//  }


  //TODO: Modify the  jsonPaylodwithChildByte to have the exact message format
  def message(i:Int): MqttMessage ={
    i%5 match {
      case 0 => MqttMessage("v1/n100/out/va-v1/evt/12/config/123", ByteString(jsonPaylodwithChildByte))
      case 1 => MqttMessage("v1/n12/out/UNSOL/sensors/"+i, ByteString(jsonPaylodwithChildByte))
      case 2 => MqttMessage("v1/n13/out/POST/loginReq/"+i, ByteString(jsonPaylodwithChildByte))
      case 3 => MqttMessage("v1/112/out/12/alarm/"+i, ByteString(jsonPaylodwithChildByte))
      case 4 => MqttMessage("v1/n990/out/UNSOL/mediaserver/"+i, ByteString(jsonPaylodwithChildByte))
    }
  }

}
