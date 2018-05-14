package com.verizon.netsense.util

import com.fasterxml.jackson.annotation.JsonInclude.Include
import com.fasterxml.jackson.databind.{DeserializationFeature, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper

import scala.concurrent.{ExecutionContext, Future}

object ObjectMapperUtil {

  val mapper = new ObjectMapper() with ScalaObjectMapper
  mapper.registerModule(DefaultScalaModule)
  mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
  mapper.setSerializationInclusion(Include.NON_ABSENT)

  def toJson(value: Any): String =
    mapper.writeValueAsString(value)

  def toJsonBytes(value: Any): Array[Byte] =
    toJson(value).getBytes()

  def fromJson[T](json: String)(implicit m: Manifest[T]): T =
    mapper.readValue[T](json)

  def fromJsonAsync[T](json: String)(implicit ec: ExecutionContext, m: Manifest[T]): Future[T] = Future {
    mapper.readValue[T](json)
  }

  def fromJsonByteArrayAsync[T](json: Array[Byte])(implicit ec: ExecutionContext, m: Manifest[T]): Future[Option[T]] =
    Future {
      Option(mapper.readValue[T](json))
    }

  def fromJsonAsync[T](json: Array[Byte])(implicit ec: ExecutionContext, m: Manifest[T]): Future[T] =
    Future { mapper.readValue[T](json) }

}
