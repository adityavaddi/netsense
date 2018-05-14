package com.verizon.netsense.services

import java.util.{Calendar, Properties}

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import com.verizon.netsense.entity.Entity
//import com.verizon.netsense.model.SensorRequestToDevice
import org.apache.kafka.clients.producer.{KafkaProducer, ProducerRecord}
import org.scalatest.{BeforeAndAfterAll, FunSuite}

sealed trait SensorRequestToDevice extends Entity

case class SensorSampleCoreRequest(nodeid: Array[String],
                                   name: String = "SensorSampleReq",
                                   sensor: String) extends SensorRequestToDevice with Entity


@JsonIgnoreProperties(ignoreUnknown = true)
case class SensorSampleRequest(uuid: String,
                               sid: String,
                               a: String,
                               f: String,
                               p: String,
                               d: String = Calendar.getInstance().toInstant.toString,
                               l: Array[Byte])  extends Entity

case class SensorSampleProps(s: String,
                             t: Long = System.currentTimeMillis() * 1000000,
                             `?u`: Boolean = true,
                             `?d`: Boolean = false)
  extends Entity





class UnifiedKafkaToMqttBridgeSpec extends FunSuite with BeforeAndAfterAll {
  val bootstrapServers                        = s"localhost:9092"
  var producer: KafkaProducer[Array[Byte], Array[Byte]] = null

  override def beforeAll(): Unit = {
    producer = new KafkaProducer[Array[Byte], Array[Byte]](getProps(bootstrapServers))
  }

  override def afterAll(): Unit = {
    producer.close()
  }

ignore("should device commands messages to the kafka topic") {

    implicit val system = ActorSystem()
    implicit val mat    = ActorMaterializer()

    val msgPackedRequest: Array[Byte] = SensorSampleProps(s = "p", t = 123123123).toMsgPack

    //val request: SensorSampleRequest = SensorSampleRequest("asdasd", "asd", "asd", "asd", "topic", Calendar.getInstance().toInstant.toString,msgPackedRequest)
  val request = SensorSampleCoreRequest(nodeid= Array("id1"), sensor = "p")
  //for(i <- 1 to 10) {
    //val data = new ProducerRecord[Array[Byte], Array[Byte]]("node.command", request.toMsgPack)
    //producer.send(data)
  //}
for(i <- 1 to 10) {
  val data1 = new ProducerRecord[Array[Byte], Array[Byte]]("node.commands.sensor", request.toMsgPack)
  producer.send(data1)
}

    Thread.sleep(1000)
  }

  def getProps(brokers: String): Properties = {
    val props = new Properties()
    props.put("bootstrap.servers", brokers)
    props.put("client.id", "notification-test")
    props.put("group.id", "testing")


    props.put("key.serializer", "org.apache.kafka.common.serialization.ByteArraySerializer")
    props.put("value.serializer", "org.apache.kafka.common.serialization.ByteArraySerializer")
    props.put("key.deserializer", "org.apache.kafka.common.serialization.ByteArrayDeserializer")
    props.put("value.deserializer", "org.apache.kafka.common.serialization.ByteArrayDeserializer")
    props
  }


}
