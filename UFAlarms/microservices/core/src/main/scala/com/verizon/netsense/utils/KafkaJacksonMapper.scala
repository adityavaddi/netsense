package com.verizon.netsense.utils

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import org.apache.kafka.common.serialization.{Deserializer, Serializer}
import org.msgpack.jackson.dataformat.MessagePackFactory

import scala.reflect.runtime.universe.TypeTag

/**
 * Created by brefsdal on 4/7/17.
 */
class KafkaJacksonMapper[A: TypeTag]() extends Deserializer[A] with Serializer[A] {

  lazy val mapper = new ObjectMapper(new MessagePackFactory)
  mapper.registerModule(DefaultScalaModule)

  override def configure(configs: java.util.Map[String, _], isKey: Boolean): Unit = {}

  override def deserialize(topic: String, data: Array[Byte]): A = ??? //{
  //mapper.readValue[A](data, classOf[A])
  //}

  override def serialize(topic: String, element: A): Array[Byte] =
    mapper.writeValueAsBytes(element)

  override def close(): Unit = {}
}
