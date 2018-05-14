package com.verizon.netsense

import java.util.UUID

import com.verizon.netsense.model.Event
import com.verizon.netsense.entity.{Event => ScalaEvent}
import com.verizon.netsense.utils.Deserializer
import org.scalatest.{FlatSpec, Matchers}
import org.velvia.{MsgPack, MsgPackUtils}

/**
 * Created by brefsdal on 4/7/17.
 */
class MsgPackSpec extends FlatSpec with Matchers {

//  "Jackson" should "serialize and deserialize POJO Event" in {
  ignore should "serialize and deserialize POJO Event" in {

    val expected = new Event("ParkingEvent",
                             "sensor",
                             "Car",
                             UUID.randomUUID().toString,
                             System.currentTimeMillis(),
                             null,
                             null,
                             null,
                             null)

    val eventBytes = Deserializer.msgpackMapper.writeValueAsBytes(expected)

    val event = Deserializer.msgpackMapper.readValue[Event](eventBytes, classOf[Event])

    println(expected)
    println(new String(eventBytes))
    println(event)

    println(expected.hashCode())
    println(event.hashCode())

    //event should equal(expected)

  }

  "Jackson" should "serialize and deserialize Case Class Event" in {

    val expected = ScalaEvent("ParkingEvent",
                              "sensor",
                              "Car",
                              UUID.randomUUID().toString,
                              System.currentTimeMillis(),
                              None,
                              None,
                              None,
                              None)

    val eventBytes = Deserializer.msgpackMapper.writeValueAsBytes(expected)

    val event = Deserializer.msgpackMapper.readValue[ScalaEvent](eventBytes, classOf[ScalaEvent])

    println(expected)
    println(new String(eventBytes))
    println(event)

    event should be(expected)

  }

  "Jackson" should "serialize POJO and deserialize case class Event" in {

    val expected = new Event("ParkingEvent",
                             "sensor",
                             "Car",
                             UUID.randomUUID().toString,
                             System.currentTimeMillis(),
                             null,
                             "imei",
                             null,
                             null)

    val eventBytes = Deserializer.msgpackMapper.writeValueAsBytes(expected)

    val event = Deserializer.msgpackMapper.readValue[ScalaEvent](eventBytes, classOf[ScalaEvent])

    println(expected)
    println(event)

  }

  ignore should "serialize case class and deserialize POJO Event" in {

    val expected = ScalaEvent("ParkingEvent",
                              "sensor",
                              "Car",
                              UUID.randomUUID().toString,
                              System.currentTimeMillis(),
                              None,
                              Some("imei"),
                              None,
                              None)

    val eventBytes = Deserializer.msgpackMapper.writeValueAsBytes(expected)

    val event = Deserializer.msgpackMapper.readValue[Event](eventBytes, classOf[Event])

    println(expected)
    println(event)

  }

  "Velia" should "serialize and deserialize Map" in {

    val expected =
      Map("name"      -> "ParkingEvent",
          "topic"     -> "sensor",
          "event"     -> "Car",
          "uuid"      -> UUID.randomUUID().toString,
          "timestamp" -> System.currentTimeMillis())

    val parkingEvent = MsgPack.pack(expected)
    val unpacked     = MsgPackUtils.unpackMap(parkingEvent)

    unpacked should be(expected)

    println(expected)
    println(unpacked)
  }
}
