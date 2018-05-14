package com.verizon.netsense.service

import java.net.InetAddress
import java.util.{Calendar, UUID}

import akka.stream.alpakka.mqtt.MqttMessage
import akka.util.ByteString
import com.verizon.netsense.model._
import com.verizon.netsense.utils.Logging

object SCHCreateMsgService extends Logging{
  val hostName     = InetAddress.getLocalHost

  def getClearScheduleMsg(nodeid: String): MqttMessage = {
      val topic = "v1/" + nodeid + "/in/PUT/light/schedule/clear"
      val restpack = RestPackCommand(
        a = "PUT",
        p = topic,
        sid = hostName.toString,
        d = Calendar.getInstance().toInstant.toString,
        uuid = UUID.randomUUID().toString,
        f = "sts",
        l = Array[Byte]()
      )
      log.info(s"Clear Msg to topic $topic")
      new MqttMessage(topic, ByteString.fromArray(restpack.toMsgPack))
  }

  def getCommitScheduleMsg(nodeid: String, id:String): MqttMessage = {
      val topic = "v1/" + nodeid + "/in/PUT/light/schedule/commit"
      val payload = new CommitSchedulePayload(id)

      val restpack = RestPackCommand(
        a = "PUT",
        p = topic,
        sid = hostName.toString,
        d = Calendar.getInstance().toInstant.toString,
        uuid = UUID.randomUUID().toString,
        f = "sts",
        l = payload.toMsgPack,
        t = id
      )
      log.info(s"Commit Msg to topic $topic")
      new MqttMessage(topic, ByteString.fromArray(restpack.toMsgPack))
  }

  def getAddScheduleMsg(nodeid: String, action: Vector[SlotPayload]): MqttMessage = {
      val topic = "v1/" + nodeid + "/in/PUT/light/schedule/add"

      val payload = new AddSchedulePayload(action)
      val restpack = RestPackCommand(
        a = "PUT",
        p = topic,
        sid = hostName.toString,
        d = Calendar.getInstance().toInstant.toString,
        uuid = UUID.randomUUID().toString,
        f = "sts",
        l = payload.toMsgPack
      )
      log.info(s"Add Msg to topic: $topic with payload: ${payload.toJSON}")
      //Create a restpack message for adding schedule
      new MqttMessage(topic, ByteString.fromArray(restpack.toMsgPack))
  }

  def getLightForceMsg(nodeid: String, dlevel: Array[Int]): MqttMessage = {
    val topic = "v1/" + nodeid + "/in/PUT/light/force"
    val lightControl = LightingControl(pri = 0, mask = 1, level = dlevel, quals = 0)
    val lfsReqPayload = LightControlPayload(1, lightControl, rates = Array(5000,5000))

    val restpack = RestPackCommand(
      a = "PUT",
      p = topic,
      sid = hostName.toString,
      d = Calendar.getInstance().toInstant.toString,
      uuid = UUID.randomUUID().toString,
      f = "sts",
      l = lfsReqPayload.toMsgPack
    )
    log.info(s"Light Force Msg to topic $topic with payload: ${lfsReqPayload.toJSON}")
    new MqttMessage(topic, ByteString.fromArray(restpack.toMsgPack))
  }

  def getLightAutoMsg(nodeid: String): MqttMessage = {
    val topic = "v1/" + nodeid + "/in/PUT/light/auto"
    val restpack = RestPackCommand(
      a = "PUT",
      p = topic,
      sid = hostName.toString,
      d = Calendar.getInstance().toInstant.toString,
      uuid = UUID.randomUUID().toString,
      f = "sts",
      l = Array[Byte]()
    )
    log.info(s"Auto Light Msg to topic $topic")
    new MqttMessage(topic, ByteString.fromArray(restpack.toMsgPack))
  }

  def getAIOScheduleMsg(nodeid: String, action: Vector[SlotPayload], tokenId: String): MqttMessage = {
    val topic = "v1/" + nodeid + "/in/PUT/light/schedule/aio"

    val payload = new AIOSchedulePayload(action)
    val restpack = RestPackCommand(
          a = "PUT",
          p = topic,
          sid = hostName.toString,
          d = Calendar.getInstance().toInstant.toString,
          uuid = UUID.randomUUID().toString,
          f = "sts",
          l = payload.toMsgPack,
          t = tokenId
          )
    log.info(s"AIO Msg to topic: $topic with payload: ${payload.toJSON}")
    //Create a restpack message for adding schedule
    new MqttMessage(topic, ByteString.fromArray(restpack.toMsgPack))
    }

}
