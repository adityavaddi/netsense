package com.verizon.netsense.services.bridge.flow

import akka.actor.ActorSystem
import akka.stream.alpakka.mqtt.MqttMessage
import akka.stream.scaladsl.{Sink, Source}
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import akka.util.ByteString
import com.verizon.netsense.services.utils.BaseSpec
import com.verizon.netsense.utils.Logging
import org.scalatest.BeforeAndAfterAll

import scala.collection.immutable.Seq
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * Created by thimija on 7/21/17.
 *
 */
class MQTTSpec extends BaseSpec with BeforeAndAfterAll with EmbeddedMQTT with Logging {

  implicit val actorSystem  = ActorSystem.create("MQTT-Spec")
  implicit val materializer = ActorMaterializer(ActorMaterializerSettings(actorSystem))(actorSystem)

  /**
   * Start Embedded MQTT broker
   */
  override def beforeAll(): Unit = {
//    startBroker()
  }

  /**
   * Stop Embedded MQTT broker
   */
  override def afterAll(): Unit = {
//    stopBroker()
  }

  /**
   * Consume message from Embedded MQTT topic
   */
  ignore should "be able to Produce and Consume MQTT message from the MQTT topic" in {
    val messageCount = 10
    val messages     = 1 to messageCount

    Source(messages).map(x => MqttMessage("v1/n100/out/va-v1/evt/dpark", ByteString(s"${x}"))).runWith(MQTT.mqttSink)

    val mqttMessages = MQTT.mqttSource.take(messageCount).runWith(Sink.seq)

    val result: Seq[MqttMessage] = Await.result(mqttMessages, 2.seconds)

    result.size must equal(messageCount)
    result.map(msg => msg.payload.utf8String.toInt) must equal(messages)
  }

}
