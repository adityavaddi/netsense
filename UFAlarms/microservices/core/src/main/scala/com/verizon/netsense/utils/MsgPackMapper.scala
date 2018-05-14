package com.verizon.netsense.utils

import org.apache.kafka.common.serialization.{Deserializer, Serializer}

import scala.reflect.runtime.universe.TypeTag
import org.velvia.{MsgPack, MsgPackUtils}

/**
 * Created by brefsdal on 4/11/17.
 */
//class MsgPackMapper[A : TypeTag]() extends Deserializer[A] with Serializer[A] {
//
//  override def configure(configs: java.util.Map[String, _], isKey: Boolean): Unit = {
//
//  }
//
//  override def deserialize(topic: String, data: Array[Byte]): A = {
//    MsgPackUtils.unpackMap(data)
//  }
//
//  override def serialize(topic: String, element: A): Array[Byte] = {
//    MsgPack.pack(A)
//  }
//
//  override def close(): Unit = {
//
//  }
//}

class MsgPackMapper extends Deserializer[Map[Any, Any]] with Serializer[Map[Any, Any]] {

  override def configure(configs: java.util.Map[String, _], isKey: Boolean): Unit = {}

  override def deserialize(topic: String, data: Array[Byte]): Map[Any, Any] =
    MsgPackUtils.unpackMap(data)

  override def serialize(topic: String, element: Map[Any, Any]): Array[Byte] =
    MsgPack.pack(element)

  override def close(): Unit = {}
}
