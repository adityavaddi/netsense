package com.verizon.netsense.connector.mqtt

import akka.Done
import akka.stream.alpakka.mqtt.scaladsl.{MqttSink, MqttSource}
import akka.stream.alpakka.mqtt.{MqttConnectionSettings, MqttMessage, MqttQoS, MqttSourceSettings}
import akka.stream.scaladsl.Source
import com.verizon.netsense.utils.Logging
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence

import scala.concurrent.Future

trait BaseMQTTConnection extends Logging {

  def mqttHost: String
  def mqttPort: Int
  def mqttProtocol: String
  def mqttSourceBufferSize: Int
  def mqttUserName: String
  def mqttPassword: String

  def connectionSettings(mqttClientId: String) =
    MqttConnectionSettings(
      mqttProtocol + "://" + mqttHost + ":" + mqttPort,
      mqttClientId,
      new MemoryPersistence
    ).withAuth(mqttUserName, mqttPassword)

  def mqttSettings(topicName: String, mqttClientId: String): MqttSourceSettings = MqttSourceSettings(
    connectionSettings(mqttClientId),
    Map(topicName -> MqttQoS.AtMostOnce)
  )
  def mqttSource(topicName: String, mqttClientId: String): Source[MqttMessage, Future[Done]] =
    MqttSource(mqttSettings(topicName, mqttClientId), bufferSize = mqttSourceBufferSize)

  def mqttSink(mqttClientId: String) = MqttSink(connectionSettings(mqttClientId), MqttQoS.atMostOnce)

}
